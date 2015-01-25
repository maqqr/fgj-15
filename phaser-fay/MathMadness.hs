{-# LANGUAGE RecordWildCards #-}
module MathMadness where

import Prelude
import Data.Function
import DOM
import PhaserHS
import Arrows
import Utils

--assetDir = ""
assetDir = "./../../assets/"

data Player = Player
    { rightChoice :: Int
    , rightNumber :: Int
    , question    :: String
    }

data GameState = GameState
    { player1     :: Player
    , player2     :: Player
    , over        :: Bool
    , warmupTimer :: Int
    }


-- | Preloads assets.
preloadGame :: Game -> Fay ()
preloadGame game = do
    mapM_ (uncurry $ loadImage game)
        [("up", assetDir ++ "up.png")
        ,("left", assetDir ++ "left.png")
        ,("right", assetDir ++ "right.png")]
    loadBitmapFont game "testfont" (assetDir ++ "font.png") (assetDir ++ "font.fnt")


-- | Creates player.
createPlayer :: Fay Player
createPlayer = do
    a <- randomInt 10 44
    b <- randomInt 10 44
    choice <- randomInt 0 2
    return $ Player choice (a + b) $ show a ++ "+" ++ show b ++ "=?"


-- | Creates initial game state.
createGame :: Game -> Fay GameState
createGame game = do
    putStrLn "Creating..."
    txt <- newText game "testfont" 64 (260, 100) "Select\ncorrect\nanswer!"
    singleShot game (2 * seconds) $ destroy txt
    p1 <- createPlayer
    p2 <- createPlayer
    return $ GameState p1 p2 False 140


-- | Updates game state.
updateGame :: Game -> State GameState -> Fay ()
updateGame game state = do
    GameState{..} <- get state
    modify state $ \g -> g { warmupTimer = warmupTimer - 1 }
    when (warmupTimer == 0) $ do
        newText game "testfont" 64 (50, 100) (question player2)
        newText game "testfont" 64 (450, 100) (question player1)
        createPlayerInfo 450 player1
        createPlayerInfo 50 player2
        return ()

    when (warmupTimer < 0 && not over) $ do
        forM_ (enumerate [player1, player2]) $ \(i, player) -> do
            input <- getGamePadInput game (i + 1)
            handleInputs i input player
    where
        enumerate :: [a] -> [(Int, a)]
        enumerate = zip [0..]

        createPlayerInfo :: Int -> Player -> Fay ()
        createPlayerInfo startX player = do
            forM_ (enumerate ["left", "up", "right"]) $ \(i, name) -> do
                newSprite game name (fromIntegral (startX + (100 * i)), 400)
                if rightChoice player == i then
                    newText game "testfont" 40 (fromIntegral (startX + (100 * i) + 10), 340) $ show (rightNumber player)
                    else do
                        wrongDelta <- randomInt 2 6
                        let wrongNum = rightNumber player - wrongDelta
                        newText game "testfont" 40 (fromIntegral (startX + (100 * i) + 10), 340) $ show wrongNum

        handleInputs :: Int -> GamePadInput -> Player -> Fay ()
        handleInputs index pad player@Player{..} =
            case getChoice pad of
                Nothing -> return ()
                Just i  -> do
                    let correct = i == rightChoice
                    newText game "testfont" 40 (fromIntegral (60 + (450 * (1-index))), 240) $
                        if correct then "Correct!" else "WRONG!"
                    modify state $ \g -> g { over = True }
                    singleShot game (1 * seconds) $ do
                        destroy game
                        if correct then
                            endGame (index + 1)
                            else
                                endGame (otherIndex (index + 1))

        otherIndex :: Int -> Int
        otherIndex 1 = 2
        otherIndex 2 = 1

        -- Returns player's choice as Int index.
        getChoice :: GamePadInput -> Maybe Int
        getChoice pad
            | pad ~> padLeft  = Just 0
            | pad ~> padUp    = Just 1
            | pad ~> padRight = Just 2
            | otherwise       = Nothing


-- | Initializes Phaser and starts game.
startGame :: Event -> Fay ()
startGame event = do
    game <- newGame 800 600
    newState game "Mainmenu" preloadGame createGame updateGame
    changeState game "Mainmenu"
    return ()


main :: Fay ()
main = addWindowEvent "load" startGame
