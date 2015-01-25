{-# LANGUAGE RecordWildCards #-}
module StarCollection where

import Prelude
import Data.Function
import DOM
import PhaserHS
import Arrows
import Utils

playerSpeed = 280

--assetDir = ""
assetDir = "./../../Assets/"

data Player = Player
    { sprite     :: Sprite
    , texPrefix  :: String
    , score      :: Int
    , scoreText  :: BitmapText
    , emitter    :: Emitter
    , swordTimer :: Int
    , hurtTimer  :: Int
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
        [("ground", assetDir ++ "platform.png")
        ,("star", assetDir ++ "star.png")
        ,("bluepart", assetDir ++ "bluepart.png")
        ,("redpart", assetDir ++ "redpart.png")]
    loadSpriteSheet game "bluedude" (assetDir ++ "blueplayer.png") (20, 32)
    loadSpriteSheet game "reddude" (assetDir ++ "redplayer.png") (20, 32)
    loadSpriteSheet game "bluesword" (assetDir ++ "bluesword.png") (40, 32)
    loadSpriteSheet game "redsword" (assetDir ++ "redsword.png") (40, 32)
    loadBitmapFont game "testfont" (assetDir ++ "font.png") (assetDir ++ "font.fnt")


changeTexture :: Player -> String -> String -> Fay ()
changeTexture player texSuffix animName = do
    loadTexture (sprite player) (texPrefix player ++ texSuffix)
    createAnim animName
    playAnimation (sprite player) animName
    where
        createAnim :: String -> Fay ()
        createAnim "walk" = addAnimation (sprite player) "walk" [0 .. 7] 10 True
        createAnim "hit" = addAnimation (sprite player) "hit" ([0 .. 4] ++ [4, 4, 4, 4] ) 10 True


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

    Player `fmap` return sprite
              <*> return texPre
              <*> return 0
              <*> createScoreDisplay
              <*> createEffect (texPre ++ "part")
              <*> return 0
              <*> return 0
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

    -- Set default animations.
    forM_ [player1', player2'] $ \p -> changeTexture p "dude" "walk"

    stars <- newGroup game
    enableBody stars True
    repeatTimer game (2 * seconds) 5 $ createStar game stars

    txt <- newText game "testfont" 64 (260, 100) "Collect\n     3\n stars!"
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


type PlayerUpdater = State GameState -> (Player -> Player) -> Fay ()


-- | Updates player 1 state.
updatePlayer1 :: PlayerUpdater
updatePlayer1 state updateFunc =
    modify state $ \g -> g { player1 = updateFunc $ player1 g }


-- | Updates player 2 state.
updatePlayer2 :: PlayerUpdater
updatePlayer2 state updateFunc =
    modify state $ \g -> g { player2 = updateFunc $ player2 g }


-- | Adds score to player.
addScore :: Int -> Player -> Player
addScore x p = p { score = score p + x }


-- | Updates player's score label text.
updateScoreDisplay :: Player -> Fay ()
updateScoreDisplay player = do
    --currentState <- get state
    --let player = getter currentState
    setText (scoreText player) $ show (score player + 1)    


-- | Updates game state.
updateGame :: Game -> State GameState -> Fay ()
updateGame game state = do
    GameState{..} <- get state
    let collider   = collide physics

    -- Physics.
    collider (sprite player1) platforms
    collider (sprite player2) platforms
    collider stars platforms

    swordFight player1 player2 updatePlayer2
    swordFight player2 player1 updatePlayer1

    checkEnd player1 1
    checkEnd player2 2

    -- Star collection.
    overlap physics (sprite player1) stars $ starCollisionHandler updatePlayer1 player1
    overlap physics (sprite player2) stars $ starCollisionHandler updatePlayer2 player2

    -- Update both players.
    forM_ (enumerate [(player1, updatePlayer1), (player2, updatePlayer2)]) $ \(i, (player, updater)) -> do
        input <- getGamePadInput game i

        -- Player is hurt, attacking or moving.

        -- Hurt update.
        when (hurtTimer player > 0) $
            updater state $ \p -> p { hurtTimer = hurtTimer p - 1 }

        -- Sword update.
        when (swordTimer player > 0 && hurtTimer player == 0) $ do
            updater state $ \p -> p { swordTimer = swordTimer p - 1 }
            when (swordTimer player - 1 == 0) $ changeTexture player "dude" "walk"
            (sprite player) ~> (body >>> velocity >>> setX 0)

        -- Regular update.
        when (swordTimer player == 0 && hurtTimer player == 0) $
            updatePlayer input player updater

        -- Jumping.
        let onGround = player ~> (sprite >>> body >>> touchingDown)
        when (padUp input && onGround) $
            player ~> (sprite >>> body >>> velocity >>> setY (-350))

    where
        enumerate :: [a] -> [(Int, a)]
        enumerate = zip [1..]

        checkEnd :: Player -> Int -> Fay ()
        checkEnd Player{..} index =
            when (score >= 3) $ do
                destroy game
                endGame index

        swordFight :: Player -> Player -> PlayerUpdater -> Fay ()
        swordFight attacker defender defenderUpdater =
            let (ax, ay)    = attacker ~> (sprite >>> position >>> vectorToTuple)
                (dx, dy)    = defender ~> (sprite >>> position >>> vectorToTuple)
                distSq      = (dx - ax) ** 2 + (dy - ay) ** 2
                closeEnough = distSq < 70 ** 2
                attacking   = swordTimer attacker > 20
                force = sign (dx - ax) * 300
            in when (closeEnough && attacking) $ do
                (sprite defender) ~> (body >>> velocity >>> setX force)
                defenderUpdater state $ \p -> p { hurtTimer = 20 }

        -- Handles collisions between players and stars.
        starCollisionHandler playerUpdater player _ star = do
            playerUpdater state (addScore 1)
            updateScoreDisplay player
            createParticleBurst (emitter player) (vectorToTuple $ position star)
            kill star

        createParticleBurst :: Emitter -> (Double, Double) -> Fay ()
        createParticleBurst emitter pos = do
            setEmitterPos emitter pos
            emitterBurst emitter 2000 25

        updatePlayer :: GamePadInput -> Player -> PlayerUpdater -> Fay ()
        updatePlayer pad player@Player{..} update
            | pad ~> padA || pad ~> padB = do
                changeTexture player "sword" "hit"
                update state $ \p -> p { swordTimer = 40 }
            | pad ~> padRight = do
                sprite ~> (body >>> velocity >>> setX playerSpeed)
                sprite ~> (scale >>> setX 3)
                sprite `playAnimation` "walk"
            | pad ~> padLeft = do
                sprite ~> (body >>> velocity >>> setX (-playerSpeed))
                sprite `playAnimation` "walk"
                sprite ~> (scale >>> setX (-3))
            | otherwise = do
                when (swordTimer == 0) $
                    sprite `setFrame` 2
                sprite ~> (body >>> velocity >>> setX 0)


-- | Initializes Phaser and starts game.
startGame :: Event -> Fay ()
startGame event = do
    game <- newGame 800 600
    newState game "Mainmenu" preloadGame createGame updateGame
    changeState game "Mainmenu"
    return ()


main :: Fay ()
main = addWindowEvent "load" startGame
