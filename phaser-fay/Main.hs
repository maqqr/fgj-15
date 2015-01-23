module Main where

import Prelude
import Data.Function
import DOM
import PhaserHS
import Arrows
import Utils


data GameState = GameState
    { cursor    :: CursorKeys
    , test      :: Int
    , physics   :: Physics
    , player    :: Sprite
    , platforms :: Group
    , stars     :: Group
    }


preloadGame :: Game -> Fay ()
preloadGame game = do
    putStrLn "Preloading..."
    mapM_ (uncurry $ loadImage game) [("ground", "platform.png"), ("star", "star.png")]
    loadSpriteSheet game "dude" "ukko.png" (48, 64)
    loadBitmapFont game "testfont" "font.png" "font.fnt"


createGame :: Game -> Fay GameState
createGame game = do
    putStrLn "Creating..."
    startGamePad game
    physics <- startPhysics game arcadePhysics

    platforms <- newGroup game
    enableBody platforms True

    ground <- create platforms "ground" (0, (height . world $ game) - 64)
    setImmovable (body ground) True

    player <- newSprite game "dude" (50, 50)
    enable physics player
    player ~> (body >>> gravity >>> setY 300)
    player ~> (anchor >>> setTo (0.5, 0.5))

    addAnimation player "still" ([0 .. 6] ++ [5, 4 .. 1]) 10 True
    addAnimation player "walk" [7 .. 30] 30 True
    playAnimation player "right"

    stars <- newGroup game
    enableBody stars True
    forM_ [0 .. 12] $ \i -> do
        rBounce <- fmap (* 0.2) random
        star <- create stars "star" (i * 70, 0)
        star ~> (body >>> gravity >>> setY 6)
        star ~> (body >>> bounce >>> setY (0.7 + rBounce))


    txt <- newText game "testfont" 64 (200, 100) "Collect\nstars!"

    cursors <- newCursorKeys game
    return $ GameState cursors 5 physics player platforms stars


updateGame :: Game -> State GameState -> Fay ()
updateGame game state = do
    gamestate <- get state
    let player'    = player gamestate
        platforms' = platforms gamestate
        stars'     = stars gamestate
        physics'   = physics gamestate
        collider   = collide physics'

    modify state $ \g -> g { test = test g + 1 }

    collider player' platforms'
    collider stars' platforms'

    overlap physics' player' stars' $ \_ star -> kill star

    p1input <- getGamePadInput (pad1 game)
    updatePlayer p1input player'
    when ((gamestate ~> (cursor >>> up >>> isDown)) && (player' ~> (body >>> touchingDown))) $ do
        putStrLn "jump"
        player' ~> (body >>> velocity >>> setY (-350))
    return ()
    where
        updatePlayer :: GamePadInput -> Sprite -> Fay ()
        updatePlayer pad player'
            | pad ~> padA = do
                player' ~> (body >>> velocity >>> setX 150)
                player' ~> (scale >>> setX 1)
                player' `playAnimation` "walk"
            | pad ~> padB = do
                player' ~> (body >>> velocity >>> setX (-150))
                player' `playAnimation` "walk"
                player' ~> (scale >>> setX (-1))
            | otherwise = do
                player' `playAnimation` "still"
                player' ~> (body >>> velocity >>> setX 0)


startGame :: Event -> Fay ()
startGame event = do
    game <- newGame 800 600
    newState game "Mainmenu" preloadGame createGame updateGame
    changeState game "Mainmenu"
    return ()


main :: Fay ()
main = addWindowEvent "load" startGame
