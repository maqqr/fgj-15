module Main where

import Prelude
import Data.Function
import DOM
import PhaserHS
import Arrows
import Utils

playerSpeed = 100

data GameState = GameState
    { physics   :: Physics
    , player1   :: Sprite
    , player2   :: Sprite
    , platforms :: Group
    , stars     :: Group
    , score1    :: Int
    , score2    :: Int
    }


-- | Preloads assets.
preloadGame :: Game -> Fay ()
preloadGame game = do
    putStrLn "Preloading....."
    mapM_ (uncurry $ loadImage game) [("ground", "platform.png"), ("star", "star.png")]
    loadSpriteSheet game "dudeblue" "blueplayer.png" (20, 32)
    loadSpriteSheet game "dudered" "redplayer.png" (20, 32)
    loadBitmapFont game "testfont" "font.png" "font.fnt"


-- | Creates player sprite.
createPlayer :: Game -> Physics -> (Double, Double) -> String -> Fay Sprite
createPlayer game physics startPos texName = do
    player <- newSprite game texName (50, 50)
    enable physics player
    player ~> (body >>> gravity >>> setY 300)
    player ~> (anchor >>> setTo (0.5, 0.5))
    player ~> (position >>> setTo startPos)
    player ~> (scale >>> setTo (3, 3))

    addAnimation player "walk" [0 .. 7] 10 True
    playAnimation player "walk"
    return player


-- | Creates initial game state.
createGame :: Game -> Fay GameState
createGame game = do
    putStrLn "Creating..."
    startGamePad game        -- Enables gamepad support.
    setSmoothing game False  -- Enables crisp pixel graphics.
    physics <- startPhysics game arcadePhysics

    platforms <- newGroup game
    enableBody platforms True

    -- Create ground floor.
    ground <- create platforms "ground" (0, (height . world $ game) - 64)
    setImmovable (body ground) True

    -- Create players.
    let cp = createPlayer game physics
    player1' <- cp (50, 50) "dudeblue"
    player2' <- cp (300, 50) "dudered"

    -- TODO: Replace with timer that creates stars.
    stars <- newGroup game
    enableBody stars True
    forM_ [0 .. 5] $ \i -> do
        rBounce <- fmap (* 0.2) random
        star <- create stars "star" (i * 80, 0)
        star ~> (scale >>> setTo (3, 3))
        star ~> (body >>> gravity >>> setY 6)
        star ~> (body >>> bounce >>> setY (0.7 + rBounce))

    -- TODO: Remove this after some seconds.
    txt <- newText game "testfont" 64 (200, 100) "Collect\nstars!"

    return $ GameState physics player1' player2' platforms stars 0 0


-- | Adds score to player 1.
addScore1 :: State GameState -> Int -> Fay ()
addScore1 state x = do
    modify state $ \g -> g { score1 = score1 g + x }
    putStrLn "Player 1 got point"


-- | Adds score to player 2.
addScore2 :: State GameState -> Int -> Fay ()
addScore2 state x = do
    modify state $ \g -> g { score2 = score2 g + x }


-- | Updates game state.
updateGame :: Game -> State GameState -> Fay ()
updateGame game state = do
    gamestate <- get state
    let player1'   = player1 gamestate
        player2'   = player2 gamestate
        platforms' = platforms gamestate
        stars'     = stars gamestate
        physics'   = physics gamestate
        collider   = collide physics'

    -- Physics.
    collider player1' platforms'
    collider player2' platforms'
    collider stars' platforms'

    -- Star collection.
    overlap physics' player1' stars' $ \_ star -> kill star >> addScore1 state 1
    overlap physics' player2' stars' $ \_ star -> kill star >> addScore2 state 1

    -- Update both players.
    forM_ (zip [player1', player2'] [1..]) $ \(pl, i) -> do
        pInput <- getGamePadInput game i
        updatePlayer pInput pl
        -- Jumping.
        when (padUp pInput && (pl ~> (body >>> touchingDown))) $
            pl ~> (body >>> velocity >>> setY (-350))

    where
        updatePlayer :: GamePadInput -> Sprite -> Fay ()
        updatePlayer pad player'
            | pad ~> padRight = do
                player' ~> (body >>> velocity >>> setX playerSpeed)
                player' ~> (scale >>> setX 3)
                player' `playAnimation` "walk"
            | pad ~> padLeft = do
                player' ~> (body >>> velocity >>> setX (-playerSpeed))
                player' `playAnimation` "walk"
                player' ~> (scale >>> setX (-3))
            | otherwise = do
                player' `setFrame` 2
                player' ~> (body >>> velocity >>> setX 0)


-- | Initializes Phaser and starts game.
startGame :: Event -> Fay ()
startGame event = do
    game <- newGame 800 600
    newState game "Mainmenu" preloadGame createGame updateGame
    changeState game "Mainmenu"
    return ()


main :: Fay ()
main = addWindowEvent "load" startGame
