{-# LANGUAGE MultiWayIf #-}

-- | Module that holds everything related to the game logic.
module Tetris where

import Control.Monad.State
import System.Random
import qualified Data.Sequence as Seq
import Data.Sequence((!?), (><))
import Data.Maybe(fromMaybe, fromJust, isJust)
import Graphics.Gloss(Color, red, green, blue, 
                      yellow, cyan, magenta, orange)

-- | Block is a m x n matrix (implemented as Sequence)
-- that says if the block is full or not.
type Field = Seq.Seq Row
type Row   = Seq.Seq Occupied

-- | Whether block is occupied or not.
type Occupied = Maybe Color

-- | Block consists of shape, velocity, and location, and type.
-- Shape is a 4x1 list that holds the location offsets of each block
-- w.r.t to the location of the entire block.
-- The "location" of the entire block is its upper left corner.
data Block = Block {shape    :: [Offsets]
                   ,location :: Location
                   ,color    :: Color}
                   deriving (Show)

-- | x,y coordinates. 
type Location  = (Int, Int)
type Offsets   = (Int, Int)
data Tetromino = I | O | T | J | L | S | Z deriving (Show)

data Game      = Game {field   :: Field
                      ,score   :: Int
                      ,rand    :: StdGen
                      ,block   :: Block
                      ,playing :: IsPlaying}
                      deriving (Show)
type IsPlaying  = Bool

-- | Size of playing field.
numVertical, numHorizontal :: Int
numVertical   = 20
numHorizontal = 10

-- | Initialize playing field.
initField :: Field
initField = Seq.fromList $ replicate numHorizontal row
  where row = Seq.fromList $ replicate numVertical Nothing

startGame :: StdGen -> Game
startGame rand = Game {field   = initField
                      ,score   = 0
                      ,rand    = rand'
                      ,block   = initBlock
                      ,playing = True}
  where 
    (randVal, rand') = runState randomize rand
    initBlock        = mkBlockByInt randVal

testRand :: StdGen
testRand = mkStdGen 0x2f

testBlock :: Block
testBlock = mkBlockByInt $ evalState randomize testRand 

testGame :: Game
testGame = Game{field=initField, score=0
               ,rand=testRand, block=testBlock
               ,playing=True}

translate :: Int -> Int -> Block -> Block
translate xOffset yOffset block =  block {location = updatedLocation}
  where (x,y)           = location block
        updatedLocation = (x + xOffset, y + yOffset)

moveLeft :: Block -> Block
moveLeft = translate (-1) 0

moveRight :: Block -> Block
moveRight = translate 1 0

moveDownFloats :: Float -> Block -> Block
moveDownFloats n = translate 0 (-(round n))

moveDown :: Block -> Block
moveDown = translate 0 (-1)

counterClockwise :: Block -> Block
counterClockwise block = block {shape = map rotateCC cubeOffsets}
  where cubeOffsets = shape block
        
        rotateCC :: Offsets -> Offsets
        rotateCC (x,y) = (-y, x)

clockwise :: Block -> Block
clockwise block = block {shape = map rotateC cubeOffsets}
  where cubeOffsets = shape block

        rotateC :: Offsets -> Offsets
        rotateC (x,y) = (y, -x)

moveBlock :: (Block->Block) -> Block -> Field -> Block
moveBlock move block field =  if legalMove then block' else block
  where 
    block'        = move block
    cubeLocations = locateCubes block'
    collision     = any (`isOccupied` field) cubeLocations
    withinBounds  = inBounds block'
    legalMove     = withinBounds && not collision

locateCubes :: Block -> [Location]
locateCubes (Block offsets (x,y) _) = map (\(x',y')->(x'+x,y'+y)) offsets

-- | FIXME: don't use fromJust. 
isOccupied :: Location -> Field -> Bool
isOccupied (x,y) field = isJust elem
  where row  = fromJust $ field !? x
        elem = fromMaybe Nothing $ row !? y

inBounds :: Block -> Bool
inBounds block = inBoundsX && inBoundsY
  where cubeLocations = locateCubes block
        xPoints       = map fst cubeLocations
        yPoints       = map snd cubeLocations
        inBoundsX     = all (\x-> x>=0 && x<numHorizontal) xPoints
        inBoundsY     = all (>=0) yPoints

hitRockBottom :: Block -> Field -> Bool
hitRockBottom block field = any cubeHitBottom belowEachCube
  where 
    cubeLocations    = locateCubes block
    belowEachCube    = map (\pos -> (fst pos, snd pos -1)) cubeLocations
    hitYBottom c     = snd c <= -1
    cubeHitBottom c  = isOccupied c field || hitYBottom c 

-- | When the bottom of a block hits the playing field,
-- transfer all information in the block to the field.
-- This causes appropriate xy coordinates of the Field
-- to go from `Nothing` to `Just Color`. 
dumpBlock :: Block -> Field -> Field
dumpBlock block@(Block _ _ color) = updateRecursive cubeLocations 
  where 
    cubeLocations = locateCubes block -- Don't use fromJust.

    updateRecursive :: [Location] -> Field -> Field
    updateRecursive loc field = foldl (flip $ updateColor color) field loc

-- | Is this inefficient? 
-- May be better to use a different data structure...
-- FIXME: Don't use fromJust.
updateColor :: Color -> Location -> Field -> Field
updateColor color (x,y) field = field'
  where row    = fromJust $ field !? x
        row'   = Seq.update y (Just color) row
        field' = Seq.update x row' field

randomize :: State StdGen Int
randomize = state $ randomR (0,6)

updateGame :: (Block->Block) -> Game -> Game
updateGame move game@(Game field score rand block playing) = 

  let 
    block'           = moveBlock move block field
    falling          = not $ hitRockBottom block' field
    field'           = dumpBlock block' field
    (randVal, rand') = runState randomize rand
    newBlock         = mkBlockByInt randVal
    game'            = clearFullRows (Game field' score rand' newBlock playing)
        
  in 
    if 
    | falling -> 
      game{block = block'}
    
    | gameOver newBlock field' ->
       game{playing=False}
    
    | otherwise -> 
      game'

-- | Clears all rows at the bottom that are full. 
clearFullRows :: Game -> Game
clearFullRows game@(Game field score _ _ _) = 
  if not bottomFull then game 
    else clearFullRows game{field = newField, score = newScore}

  where 
    newField = clearBottom field
    newScore = score + 100

    bottomFull :: Bool
    bottomFull = all isJust . fromJust $ Seq.lookup 0 field

    clearBottom :: Field -> Field
    clearBottom field = tails >< head
      where tails = fromJust $ Seq.lookup 1 $ Seq.tails field
            head  = Seq.fromList [Seq.fromList $ replicate numVertical Nothing]

-- | Given a block and field, 
-- determines if the game is over or not.
gameOver :: Block -> Field -> IsPlaying
gameOver block field = any (`isOccupied` field) $ locateCubes block


initLocation :: Int -> Location
initLocation x = (x, numVertical)

-- | Function to create new blocks.
-- FIXME: allow random spawning of blocks. 
-- FIXME: make the totality of the blocks appear at once.
mkBlock :: Tetromino -> Block
mkBlock I = Block iBlock (initLocation 0) red
  where iBlock = [(0,0), (0,-1), (0,-2), (0,-3)]
mkBlock O = Block oBlock (initLocation 1) blue
  where oBlock = [(0,0), (1,0), (0,-1), (1,-1)]
mkBlock T = Block tBlock (initLocation 2) green
  where tBlock = [(0,0), (1,0), (2,0), (1,-1)]
mkBlock J = Block jBlock (initLocation 3) yellow
  where jBlock = [(0,0), (0,-1), (0,-2), (-1,-2)]
mkBlock L = Block lBlock (initLocation 4) cyan
  where lBlock = [(0,0), (0,-1), (0,-2), (1,-2)]
mkBlock S = Block sBlock (initLocation 5) magenta
  where sBlock = [(0,0), (1,0), (0,-1), (-1,-1)]
mkBlock Z = Block zBlock (initLocation 6) orange
  where zBlock = [(0,0), (1,0), (1,-1), (2,-1)]

mkBlockByInt :: Int -> Block
mkBlockByInt 0 = mkBlock I
mkBlockByInt 1 = mkBlock O
mkBlockByInt 2 = mkBlock T
mkBlockByInt 3 = mkBlock J
mkBlockByInt 4 = mkBlock L
mkBlockByInt 5 = mkBlock S
mkBlockByInt 6 = mkBlock Z
mkBlockByInt _ = error "This should never happen"