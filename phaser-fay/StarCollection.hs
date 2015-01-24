{-# LANGUAGE RecordWildCards #-}
module StarCollection where

import Prelude
import Data.Function
import DOM
import PhaserHS
import Arrows
import Utils

playerSpeed = 100

data Player = Player
    { sprite    :: Sprite
    , texPrefix :: String
    , score     :: Int
    , scoreText :: BitmapText
    , emitter   :: Emitter
    }

data GameState = GameState
    { physics   :: Physics
    , player1   :: Player
    , player2   :: Player
    , platforms :: Group
    , stars     :: Group
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
    loadSpriteSheet game "bluedude" "blueplayer.png" (20, 32)
    loadSpriteSheet game "reddude" "redplayer.png" (20, 32)
    loadSpriteSheet game "bluesword" "bluesword.png" (40, 32)
    loadSpriteSheet game "redsword" "redsword.png" (40, 32)
    loadBitmapFont game "testfont" "font.png" "font.fnt"


changeTexture :: Sprite -> String -> Fay ()
changeTexture player texSuffix = return () -- TODO
-- (http://stackoverflow.com/a/25396894)
--function changeTexture () {
--    sprite.loadTexture("sea_creature");
--    sprite.animations.add("sea_creature", specificFramesToUse, frameRate, whetherToLoop, whetherToUseANumericIndex);
--    sprite.animations.play("sea_creature");
--}


-- | Creates player sprite.
createPlayer :: Game
             -> Physics
             -> (Double, Double) -- ^ Player start position.
             -> (Double, Double) -- ^ Player score text position.
             -> String -- ^ Player texture name prefix.
             -> Fay Player
createPlayer game physics startPos textPos texPre = do
    sprite <- newSprite game (texPre ++ "dude") (50, 50)
    enable physics sprite
    sprite ~> (body >>> gravity >>> setY 300)
    sprite ~> (anchor >>> setTo (0.5, 0.5))
    sprite ~> (position >>> setTo startPos)
    sprite ~> (scale >>> setTo (3, 3))
    sprite ~> (body >>> flip collideWorldBounds True)

    addAnimation sprite "walk" [0 .. 7] 10 True
    playAnimation sprite "walk"
    Player `fmap` return sprite <*> return texPre <*> return 0 <*> createScoreDisplay <*> createEffect (texPre ++ "part")
    where
        createScoreDisplay :: Fay BitmapText
        createScoreDisplay = newText game "testfont" 64 textPos "0"

        createEffect :: String -> Fay Emitter
        createEffect particleTexName = do
            emitter <- newEmitter game (0, 0) 50 [particleTexName]
            setParticleMaxSpeed emitter (-400, -400)
            setParticleMinSpeed emitter (800, 800)
            return emitter


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
    player1' <- cp (740, 50) (710, 50) "blue"
    player2' <- cp (60, 50) (30, 50) "red"

    stars <- newGroup game
    enableBody stars True
    repeatTimer game (2 * seconds) 5 $ createStar game stars

    txt <- newText game "testfont" 64 (260, 100) "Collect\n stars!"
    singleShot game (3 * seconds) $ destroy txt

    return $ GameState physics player1' player2' platforms stars
    where
        createStar :: Game -> Group -> Fay ()
        createStar game stars = do
            rBounce <- fmap (* 0.2) random -- Random bounciness.
            rx      <- fmap (\i -> 80 + i * (800 - 80 * 2)) random -- Random x position.
            star <- create stars "star" (rx, 0)
            star ~> (anchor >>> setTo (0.5, 0.5))
            star ~> (scale >>> setTo (3, 3))
            star ~> (body >>> gravity >>> setY 80)
            star ~> (body >>> bounce >>> setY (0.7 + rBounce))


-- | Adds score to player.
addScore :: Int -> Player -> Player
addScore x p = p { score = score p + x }

-- | Adds score to player 1.
addScore1 :: State GameState -> Int -> Fay ()
addScore1 state x = do
    modify state $ \g -> g { player1 = addScore x $ player1 g }
    currentState <- get state
    let player = player1 currentState
    setText (scoreText player) $ show (score player)


-- | Adds score to player 1.
addScore2 :: State GameState -> Int -> Fay ()
addScore2 state x = do
    modify state $ \g -> g { player2 = addScore x $ player2 g }
    currentState <- get state
    let player = player2 currentState
    setText (scoreText player) $ show (score player)


-- | Updates game state.
updateGame :: Game -> State GameState -> Fay ()
updateGame game state = do
    GameState{..} <- get state
    let collider   = collide physics

    -- Physics.
    collider (sprite player1) platforms
    collider (sprite player2) platforms
    collider stars platforms

    -- Star collection.
    overlap physics (sprite player1) stars $ starCollisionHandler addScore1 player1
    overlap physics (sprite player2) stars $ starCollisionHandler addScore2 player2

    -- Update both players.
    forM_ (enumerate [player1, player2]) $ \(i, player) -> do
        input <- getGamePadInput game i
        updatePlayer input (sprite player)
        -- Jumping.
        let onGround = player ~> (sprite >>> body >>> touchingDown)
        when (padUp input && onGround) $
            player ~> (sprite >>> body >>> velocity >>> setY (-350))

    where
        enumerate :: [a] -> [(Int, a)]
        enumerate = zip [1..]

        {-
        updateScoreText :: Int -> Fay ()
        updateScoreText 1 = fmap player1 (get state) >>= updater
        updateScoreText 2 = fmap player2 (get state) >>= updater
        
        updater Player{..} = setText scoreText $ show score
        -}

        starCollisionHandler addScore player _ star = do
            addScore state 1
            --updateScoreText player
            createParticleBurst (emitter player) (vectorToTuple $ position star)
            kill star

        createParticleBurst :: Emitter -> (Double, Double) -> Fay ()
        createParticleBurst emitter pos = do
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
