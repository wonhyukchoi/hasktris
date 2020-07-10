-- | Module that holds everything related to the game logic.
module Tetris where

import Control.Monad.State
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
data Block = Block {shape     :: [Offsets]
                   ,location  :: Location
                   ,color     :: Color}
                   deriving (Show)

-- | x,y coordinates. 
type Location  = (Int, Int)
type Offsets   = (Int, Int)
data Tetromino = I | O | T | J | L | S | Z deriving (Show)

type GameState  = State Field Playing
type Playing    = Bool

-- | Size of playing field.
numVertical, numHorizontal :: Int
numVertical   = 20
numHorizontal = 10

-- | Initialize playing field.
initField :: Field
initField = Seq.fromList $ replicate numHorizontal row
  where row = Seq.fromList $ replicate numVertical Nothing

translate :: Int -> Int -> Block -> Block
translate xOffset yOffset block =  block {location = updatedLocation}
  where (x,y)           = location block
        updatedLocation = (x + xOffset, y + yOffset)

moveLeft :: Block -> Block
moveLeft = translate (-1) 0

moveRight :: Block -> Block
moveRight = translate 1 0

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

moveBlock :: (Block->Block) -> Field -> Block -> Block
moveBlock move field block =  if legalMove then block' else block
  where 
    block'        = move block
    cubeLocations = locateCubes block'
    noCollision   = any (`isOccupied` field) cubeLocations
    withinBounds  = inBounds block'
    legalMove     = noCollision && withinBounds

locateCubes :: Block -> [Location]
locateCubes (Block offsets (x,y) _) = map (\(x',y')->(x'+x,y'+y)) offsets

-- | FIXME: don't use fromJust. 
isOccupied :: Location -> Field -> Bool
isOccupied (x,y) field = isJust elem
  where row  = fromJust $ field !? x
        elem = fromJust $ row !? y

inBounds :: Block -> Bool
inBounds block = inBoundsX && inBoundsY
  where cubeLocations = locateCubes block
        xPoints       = map fst cubeLocations
        yPoints       = map snd cubeLocations
        inBoundsX     = all (\x-> x>=0 && x<numHorizontal) xPoints
        inBoundsY     = all (\y-> y>=0 && y<numVertical) yPoints

hitRockBottom :: Block -> Field -> Bool
hitRockBottom block field = any (`isOccupied` field) belowEachCube
  where 
    cubeLocations = locateCubes block
    belowEachCube = map (\pos -> (fst pos, snd pos -1)) cubeLocations

-- | When the bottom of a block hits the playing field,
-- transfer all information in the block to the field.
-- This causes appropriate xy coordinates of the Field
-- to go from `Nothing` to `Just Color`. 
groundBlock :: Block -> Field -> Field
groundBlock block@(Block _ _ color) = updateRecursive cubeLocations 
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
  

-- | TODO
updateGame :: (Block->Block) -> Block -> GameState
updateGame _ = error "Not Implemented"


-- | Clears all rows at the bottom that are full. 
clearFullRows :: Field -> Field
clearFullRows field = 
  if not bottomFull then field else clearFullRows $ clearRow field
  where 
    bottomFull :: Bool
    bottomFull = all isJust . fromJust $ Seq.lookup 0 field

    clearRow :: Field -> Field
    clearRow field = tails >< head
      where tails = fromJust $ Seq.lookup 1 $ Seq.tails field
            head  = Seq.fromList [Seq.fromList $ replicate numVertical Nothing]

-- | Given a block and field, 
-- determines if the game is over or not.
gameOver :: Block -> Field -> Playing
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
  where zBlock = [(0,0), (0,1), (1,-1), (2,-1)]

testBlock = mkBlock I