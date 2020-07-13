{-# LANGUAGE MultiWayIf #-}

-- | Module that holds everything related to the game logic.
module Tetris where

import Control.Monad.State
import System.Random
import qualified Data.Sequence as Seq
import Data.Sequence((!?), (><), Seq(..), (|>))
import Data.Maybe(fromMaybe, fromJust, isJust)
import Graphics.Gloss(Color, red, green, blue, 
                      yellow, cyan, magenta, orange)

-- | Block is a m x n matrix (implemented as Sequence)
-- that says if the block is full or not.
type Field    = Seq.Seq Column
type Column   = Seq.Seq Occupied
type Row      = Seq.Seq Occupied

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

-- | Creates a new game from RNG.
startGame :: StdGen -> Game
startGame rand = Game {field   = initField
                      ,score   = 0
                      ,rand    = rand'
                      ,block   = initBlock
                      ,playing = True}
  where 
    (randVal, rand') = runState randomize rand
    initBlock        = mkBlockByInt randVal

lightField :: Field
lightField = Seq.fromList [Seq.fromList [Nothing, Just red, Just blue]
                          ,Seq.fromList [Just orange, Nothing, Nothing]]

-- testRand :: StdGen
-- testRand = mkStdGen 0x2f

-- testBlock :: Block
-- testBlock = mkBlockByInt $ evalState randomize testRand 

-- testGame :: Game
-- testGame = Game{field=initField, score=0
--                ,rand=testRand, block=testBlock
--                ,playing=True}

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
  where xElems  = fromJust $ field !? x
        elem    = fromMaybe Nothing $ xElems !? y

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
-- transfer block's colors to the field.
-- This causes appropriate xy coordinates of the Field
-- to go from `Nothing` to `Just Color`. 
dumpColor :: Block -> Field -> Field
dumpColor block@(Block _ _ color) = updateRecursive cubeLocations 
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

-- | Code structure left over from originally using State Monads,
-- but had to change to match Gloss API.
-- TODO: refactor (but for aesthetic purposes).
updateGame :: (Block->Block) -> Game -> Game
updateGame _ game@(Game _ _ _ _ False) = game
updateGame move game@(Game field score rand block playing) = 

  let 
    block'           = moveBlock move block field
    falling          = not $ hitRockBottom block' field
    field'           = dumpColor block' field
    (randVal, rand') = runState randomize rand
    newBlock         = mkBlockByInt randVal
    game'            = clearFullRows (Game field' score rand' block' playing)

    aboveScreen :: Bool
    aboveScreen = any (>=numVertical) yPoints
      where cubeLocations = locateCubes block'
            yPoints       = map snd cubeLocations

    -- Game over determined if block is OOB but it's not falling.
    gameOver = aboveScreen && not falling

  in 
    if 
    | falling -> 
      game{block = block'}
    
    | gameOver ->
       game{playing=False}
    
    | otherwise -> 
      game'{block=newBlock}


-- | TODO: refactor this.
-- Clears any full rows from the game and updates score.
-- Due to the fact that game is stupidly designed in (x,y) coordinates
-- (instead of (row, column) indexing)
-- needs to transpose the field twice in order to clear rows.
clearFullRows :: Game -> Game
clearFullRows game@(Game field score _ _ _) = 
  if noFullRows then game
  else 
    clearFullRows game{field=field', score=score'}

  where
    transposed = transposeField field

    notFull :: Row -> Bool
    notFull = not . all isJust

    -- Until you hit a full row
    -- First elem of tails includes the full row.
    (heads, tails_) = Seq.spanl notFull transposed
    tails           = Seq.drop 1 tails_
    fullRemoved     = heads >< tails

    noFullRows      = null tails_
    field'          = transposeField fullRemoved
    score'          = score + 100

-- | Tranpose a field.
transposeField :: Field -> Field
transposeField field = 
  transpose (lenField-1) field Seq.Empty
  where
    lenField = length . fromJust $ field !? 0

    getRow :: Int -> Field -> Row
    getRow n = foldl (\acc col -> acc |> getElem col) Seq.Empty
      where getElem = fromJust . flip (!?) n 

    transpose :: Int -> Field -> Field -> Field
    transpose (-1) _ building  = building 
    transpose n field building = transpose (n-1) field building |> getRow n field  


initLocation :: Int -> Location
initLocation x = (x, numVertical)

-- | Function to create new blocks.
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

-- | Takes an integer to produce a block.
mkBlockByInt :: Int -> Block
mkBlockByInt 0 = mkBlock I
mkBlockByInt 1 = mkBlock O
mkBlockByInt 2 = mkBlock T
mkBlockByInt 3 = mkBlock J
mkBlockByInt 4 = mkBlock L
mkBlockByInt 5 = mkBlock S
mkBlockByInt 6 = mkBlock Z
mkBlockByInt _ = error "This should never happen"