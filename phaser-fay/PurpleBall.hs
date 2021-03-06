{-# LANGUAGE RecordWildCards #-}
module PurpleBall where

import Prelude
import Data.Function
import DOM
import PhaserHS
import Arrows
import Utils

playerSpeed = 180

--assetDir = ""
assetDir = "./../../assets/"

data Player = Player
    { sprite     :: Sprite
    , texPrefix  :: String
    , swordTimer :: Int
    , hurtTimer  :: Int
    }

data GameState = GameState
    { physics   :: Physics
    , player1   :: Player
    , player2   :: Player
    , platforms :: Group
    , ball      :: Sprite
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
    loadSpriteSheet game "ball" (assetDir ++ "ballz.png") (16, 32)
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
    sprite ~> (body >>> gravity >>> setY 600)
    sprite ~> (anchor >>> setTo (0.5, 0.5))
    sprite ~> (position >>> setTo startPos)
    sprite ~> (scale >>> setTo (3, 3))
    sprite ~> (body >>> flip collideWorldBounds True)

    return $ Player sprite texPre 0 0


-- | Creates initial game state.
createGame :: Game -> Fay GameState
createGame game = do
    putStrLn "Creating..."
    startGamePad game        -- Enables gamepad support.
    setSmoothing game False  -- Enables crisp pixel graphics.
    physics <- startPhysics game arcadePhysics

    platforms <- newGroup game
    enableBody platforms True

    -- Create platforms.
    let makep = platf platforms
    makep (400, 600) (800, 32)

    -- Middle plaftorms.
    makep (400, 116) (64, 16)
    makep (400, 300) (64, 16)
    makep (400, 480) (64, 16)

    -- Side platforms
    forM_ [230, 400] $ \i -> do
        makep (100, i) (200, 16)
        makep (700, i) (200, 16)

    -- Create players.
    let cp = createPlayer game physics
    player1' <- cp (740, 500) (710, 50) "blue"
    player2' <- cp (60, 500) (30, 50) "red"

    -- Set default animations.
    forM_ [player1', player2'] $ \p -> changeTexture p "dude" "walk"

    ball <- newSprite game "ball" (400, 60)
    ball ~> (anchor >>> setTo (0.5, 0.5))
    ball ~> (scale >>> setTo (3, 3))
    addAnimation ball "bounce" [0 .. 12] 15 True
    playAnimation ball "bounce"

    txt <- newText game "testfont" 64 (200, 100) "Run to the\npurple ball!"
    singleShot game (3 * seconds) $ destroy txt

    return $ GameState physics player1' player2' platforms ball
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

        platf :: Group -> (Double, Double) -> (Double, Double) -> Fay()
        platf gr pos (w, h) = do
            ground <- create gr "ground" (0, 0)
            ground ~> (anchor >>> setTo (0.5, 0.5))
            ground `setWidth` w
            ground `setHeight` h
            ground ~> (position >>> setTo pos)
            setImmovable (body ground) True


type PlayerUpdater = State GameState -> (Player -> Player) -> Fay ()


-- | Updates player 1 state.
updatePlayer1 :: PlayerUpdater
updatePlayer1 state updateFunc =
    modify state $ \g -> g { player1 = updateFunc $ player1 g }


-- | Updates player 2 state.
updatePlayer2 :: PlayerUpdater
updatePlayer2 state updateFunc =
    modify state $ \g -> g { player2 = updateFunc $ player2 g }


-- | Updates game state.
updateGame :: Game -> State GameState -> Fay ()
updateGame game state = do
    GameState{..} <- get state
    let collider   = collide physics

    -- Physics.
    collider (sprite player1) platforms
    collider (sprite player2) platforms

    swordFight player1 player2 updatePlayer2
    swordFight player2 player1 updatePlayer1

    checkEnd ball player1 1
    checkEnd ball player2 2

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
            player ~> (sprite >>> body >>> velocity >>> setY (-400))

    where
        enumerate :: [a] -> [(Int, a)]
        enumerate = zip [1..]

        -- Ends game when player is outside of screen.
        checkEnd :: Sprite -> Player -> Int -> Fay ()
        checkEnd ball Player{..} index = do
            let (ax, ay)    = sprite ~> (position >>> vectorToTuple)
                (dx, dy)    = ball ~> (position >>> vectorToTuple)
                distSq      = (dx - ax) ** 2 + (dy - ay) ** 2
                closeEnough = distSq < 30 ** 2
            when closeEnough $ do
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
