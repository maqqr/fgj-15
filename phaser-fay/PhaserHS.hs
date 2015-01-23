{-# LANGUAGE EmptyDataDecls #-}
module PhaserHS where

import FFI
import Utils

data Game
data State s
data Sprite
data Group
data World
data Vector
data BitmapText


random :: Fay Double
random = ffi "Math.random()"

newGame :: Int -> Int -> Fay Game
newGame = ffi "wrNewGame(%*)"

setSmoothing :: Game -> Bool -> Fay ()
setSmoothing = ffi "%1.stage.smoothed=%2"

-- Asset loading

loadImage :: Game -> String -> String -> Fay ()
loadImage = ffi "%1.load.image(%2, %3)"

loadSpriteSheet :: Game -> String -> String -> (Int, Int) -> Fay ()
loadSpriteSheet = ffi "wrLoadSpriteSheet(%*)"

loadBitmapFont :: Game -> String -> String -> String -> Fay ()
loadBitmapFont = ffi "%1.load.bitmapFont(%2, %3, %4)"


-- States

newState :: Game
         -> String -- ^ State name
         -> (Game -> Fay ()) -- ^ Preload function
         -> (Game -> Fay s)  -- ^ Initialization function
         -> (Game -> State s -> Fay ()) -- ^ Update function
         -> Fay (State s)
newState = ffi "wrNewState(%*)"

changeState :: Game -> String -> Fay ()
changeState = ffi "%1.state.start(%2)"

getCurrentState :: Game -> State s
getCurrentState = ffi "%1.state.getCurrentState()"

get :: State s -> Fay s
get = ffi "%1.internalState"

set :: State s -> s -> Fay ()
set = ffi "%1.internalState=%2"

modify :: State s -> (s -> s) -> Fay ()
modify s func = get s >>= return . func >>= set s


-- Text

newText :: Game -> String -> Double -> (Double, Double) -> String -> Fay BitmapText
newText = ffi "wrNewText(%*)"

setText :: BitmapText -> String -> Fay ()
setText = ffi "%1.setText(%2)"


-- Sprites

newSprite :: Game -> String -> (Double, Double) -> Fay Sprite
newSprite = ffi "wrNewSprite(%*)"

anchor :: Sprite -> Vector
anchor = ffi "%1.anchor"

position :: Sprite -> Vector
position = ffi "%1.position"

addAnimation :: Sprite -> String -> [Int] -> Int -> Bool -> Fay ()
addAnimation = ffi "%1.animations.add(%2, %3, %4, %5)"

playAnimation :: Sprite -> String -> Fay ()
playAnimation = ffi "%1.animations.play(%2)"

stopAnimation :: Sprite -> Fay ()
stopAnimation = ffi "%1.animations.stop()"

setFrame :: Sprite -> Int -> Fay ()
setFrame = ffi "%1.frame=%2"

kill :: Ptr a -> Fay ()
kill = ffi "%1.kill()"

destroy :: Ptr a -> Fay ()
destroy = ffi "%1.destroy()"

scale :: Sprite -> Vector
scale = ffi "%1.scale"

setWidth :: Sprite -> Double -> Fay ()
setWidth = ffi "%1.width=%2"

setHeight :: Sprite -> Double -> Fay ()
setHeight = ffi "%1.height=%2"


-- Groups

newGroup :: Game -> Fay Group
newGroup = ffi "%1.add.group()"

enableBody :: Group -> Bool -> Fay ()
enableBody = ffi "%1.enableBody=%2"

create :: Group -> String -> (Double, Double) -> Fay Sprite
create = ffi "wrCreate(%*)"


-- World

world :: Game -> World
world = ffi "%1.world"

centerX :: World -> Double
centerX = ffi "%1.centerX"

centerY :: World -> Double
centerY = ffi "%1.centerY"

width :: World -> Double
width = ffi "%1.width"

height :: World -> Double
height = ffi "%1.height"


-- Physics

data Physics
data PhysicsType
data Body

arcadePhysics :: PhysicsType
arcadePhysics = ffi "Phaser.Physics.ARCADE"

p2Physics :: PhysicsType
p2Physics = ffi "Phaser.Physics.P2"

ninjaPhysics :: PhysicsType
ninjaPhysics = ffi "Phaser.Physics.NINJA"

startPhysics :: Game -> PhysicsType -> Fay Physics
startPhysics = ffi "wrStartPhysics(%*)"

collide :: Physics -> Ptr a -> Ptr b -> Fay Bool
collide = ffi "wrCollide(%*)"

overlap :: Physics -> Ptr a -> Ptr b -> (Sprite -> Sprite -> Fay ()) -> Fay Bool
overlap = ffi "wrOverlap(%*)"

enable :: Physics -> Sprite -> Fay ()
enable = ffi "%1.enable(%2)"

body :: Sprite -> Body
body = ffi "%1.body"

touchingDown :: Body -> Bool
touchingDown = ffi "%1.touching.down"

setImmovable :: Body -> Bool -> Fay ()
setImmovable = ffi "%1.immovable=%2"

gravity :: Body -> Vector
gravity = ffi "%1.gravity"

velocity :: Body -> Vector
velocity = ffi "%1.velocity"

bounce :: Body -> Vector
bounce = ffi "%1.bounce"

getX :: Vector -> Double
getX = ffi "%1.x"

getY :: Vector -> Double
getY = ffi "%1.y"

setX :: Double -> Vector -> Fay ()
setX = ffi "%2.x=%1"

setY :: Double -> Vector -> Fay ()
setY = ffi "%2.y=%1"

vectorToTuple :: Vector -> (Double, Double)
vectorToTuple = ffi "[%1.x, %1.y]"

setTo :: (Double, Double) -> Vector -> Fay ()
setTo = ffi "wrSetTo(%*)"

collideWorldBounds :: Body -> Bool -> Fay ()
collideWorldBounds = ffi "%1.collideWorldBounds=%2"


-- Input

data GamePad
data GamePadInput

startGamePad :: Game -> Fay ()
startGamePad = ffi "%1.input.gamepad.start()"

pad1 :: Game -> GamePad
pad1 = ffi "%1.input.gamepad.pad1"

pad2 :: Game -> GamePad
pad2 = ffi "%1.input.gamepad.pad2"

getGamePadInput :: Game -> Int -> Fay GamePadInput
getGamePadInput = ffi "wrGetGamePadInput(%*)"

padUp :: GamePadInput -> Bool
padUp = ffi "%1.up"

padDown :: GamePadInput -> Bool
padDown = ffi "%1.down"

padLeft :: GamePadInput -> Bool
padLeft = ffi "%1.left"

padRight :: GamePadInput -> Bool
padRight = ffi "%1.right"

padA :: GamePadInput -> Bool
padA = ffi "%1.a"

padB :: GamePadInput -> Bool
padB = ffi "%1.b"


-- Time

seconds :: Double
seconds = ffi "Phaser.Timer.SECOND"

singleShot :: Game -> Double -> Fay () -> Fay ()
singleShot = ffi "%1.time.events.add(%2, %3, null)"

repeatTimer :: Game -> Double -> Int -> Fay () -> Fay ()
repeatTimer = ffi "%1.time.events.repeat(%2, %3, %4, null)"


-- Particles

data Emitter

newEmitter :: Game -> (Double, Double) -> Int -> [String] -> Fay Emitter
newEmitter = ffi "wrNewEmitter(%*)"

setEmitterPos :: Emitter -> (Double, Double) -> Fay ()
setEmitterPos = ffi "wrSetEmitterPos(%*)"

emitterBurst :: Emitter -> Double -> Int -> Fay ()
emitterBurst = ffi "%1.start(true, %2, null, %3)"

