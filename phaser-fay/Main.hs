{-# LANGUAGE RecordWildCards #-}
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
    , emitters  :: (Emitter, Emitter)
    }


-- | Preloads assets.
preloadGame :: Game -> Fay ()
preloadGame game = do
    putStrLn "Preloading....."
    mapM_ (uncurry $ loadImage game)
        [("ground", "platform.png")
        ,("star", "star.png")
        ,("bluepart", "bluepart.png")
        ,("redpart", "redpart.png")]
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
    player ~> (body >>> flip collideWorldBounds True)

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
    ground `setWidth` 800
    ground `setHeight` 64
    setImmovable (body ground) True

    -- Create players.
    let cp = createPlayer game physics
    player1' <- cp (740, 50) "dudeblue"
    player2' <- cp (60, 50) "dudered"

    stars <- newGroup game
    enableBody stars True
    repeatTimer game (3 * seconds) 10 $ createStar game stars

    txt <- newText game "testfont" 64 (260, 100) "Collect\n     3\n stars!"
    singleShot game (3 * seconds) $ destroy txt

    redEmitter <- createEffect "redpart"
    blueEmitter <- createEffect "bluepart"

    return $ GameState physics player1' player2' platforms stars 0 0 (blueEmitter, redEmitter)
    where
        createEffect :: String -> Fay Emitter
        createEffect particleTexName = do
            emitter <- newEmitter game (0, 0) 50 [particleTexName]
            setParticleMaxSpeed emitter (-400, -400)
            setParticleMinSpeed emitter (800, 800)
            return emitter

        createStar :: Game -> Group -> Fay ()
        createStar game stars = do
            rBounce <- fmap (* 0.2) random -- Random bounciness.
            rx      <- fmap (\i -> 80 + i * (800 - 80 * 2)) random -- Random x position.
            star <- create stars "star" (rx, 0)
            star ~> (anchor >>> setTo (0.5, 0.5))
            star ~> (scale >>> setTo (3, 3))
            star ~> (body >>> gravity >>> setY 80)
            star ~> (body >>> bounce >>> setY (0.7 + rBounce))


-- | Adds score to player 1.
addScore1 :: State GameState -> Int -> Fay ()
addScore1 state x = modify state $ \g -> g { score1 = score1 g + x }


-- | Adds score to player 2.
addScore2 :: State GameState -> Int -> Fay ()
addScore2 state x = modify state $ \g -> g { score2 = score2 g + x }


-- | Updates game state.
updateGame :: Game -> State GameState -> Fay ()
updateGame game state = do
    GameState{..} <- get state
    let collider   = collide physics

    -- Physics.
    collider player1 platforms
    collider player2 platforms
    collider stars platforms

    -- Star collection.
    overlap physics player1 stars $ starCollisionHandler addScore1 fst
    overlap physics player2 stars $ starCollisionHandler addScore2 snd

    -- Update both players.
    forM_ (enumerate [player1, player2]) $ \(i, player) -> do
        input <- getGamePadInput game i
        updatePlayer input player
        -- Jumping.
        when (padUp input && (player ~> (body >>> touchingDown))) $
            player ~> (body >>> velocity >>> setY (-350))

    where
        enumerate :: [a] -> [(Int, a)]
        enumerate = zip [1..]

        starCollisionHandler addScore getter _ star = do
            addScore state 1
            createParticleBurst getter (vectorToTuple $ position star)
            kill star

        createParticleBurst :: ((Emitter, Emitter) -> Emitter) -> (Double, Double) -> Fay ()
        createParticleBurst getter pos = do
            emitter <- fmap (getter . emitters) (get state)
            setEmitterPos emitter pos
            emitterBurst emitter 2000 25

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
