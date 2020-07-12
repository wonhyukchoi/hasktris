-- | Module that contains all UI and user interactions.
module Frontend where

import Tetris
import qualified Data.Sequence as Seq
import qualified Graphics.Gloss as G
import Data.Foldable(toList)
import Data.Bifunctor(bimap)
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State

playGame :: IO ()
-- main = testDisplay sampleImage
-- main = G.display window background $ render initGame
playGame = G.animate window background frame
  where 
    frame :: Float -> G.Picture
    frame sec = render $ gravitate sec initGame

testDisplay :: G.Picture -> IO ()
testDisplay = G.display window background

-- | Dimensions of UI.
fullHeight, fullWidth :: Int  
fullHeight = 1000
fullWidth  = 800

-- | Dimensions of actual game.
-- Game dimension : 400 x 800
bottomMargin, topMargin, sideMargin, gameHeight, gameWidth :: Int
bottomMargin = 100
topMargin    = 100
sideMargin   = 200
gameHeight   = fullHeight - bottomMargin - topMargin
gameWidth    = fullWidth - (2 * sideMargin) 

-- | Dimensions of each cube (each block has 4).
blockSize :: Float
blockSize =  40

xBase, yBase :: Float
xBase = -(fromIntegral fullWidth/2 - fromIntegral sideMargin)+ blockSize/2
yBase = -(fromIntegral fullHeight/2 - fromIntegral bottomMargin) + blockSize/2

-- | TODO: add black screen on top to hide falling block
render :: Game -> G.Picture
render (Game field score rand block) = 
  G.Pictures [gameFrame, displayField field, displayBlock block]

-- | For testing purposes
gravitate :: Float -> Game -> Game
gravitate f = execState (updateGame $ moveDownFloats f) 

gameFrame :: G.Picture
gameFrame = G.color G.white $ G.rectangleWire width height
  where width  = fromIntegral gameWidth
        height = fromIntegral gameHeight

cube :: G.Picture
cube = G.rectangleSolid blockSize blockSize

displayBlock :: Block -> G.Picture
displayBlock block@(Block shape location cubeColor) = 
  G.pictures $ map translatePair locations' <*> [oneCube]
  where 
    locations     = locateCubes block
    upSizeDim     = (* blockSize) . fromIntegral
    fixDimX x     = upSizeDim x + xBase
    fixDimY y     = upSizeDim y + yBase
    locations'    = map (bimap fixDimX fixDimY) locations
    oneCube       = G.color cubeColor cube
    translatePair = uncurry G.translate

displayField :: Field -> G.Picture
displayField field = 
  G.Pictures $ zipWith (\pic row-> G.translate 0 (row*blockSize) pic) rowPics rowNums
  where 
    rowPics  = displayRow <$> toList field
    numRows  = fromIntegral $ Seq.length field :: Float
    rowNums  = [0.0..(numRows-1)]

displayRow :: Row -> G.Picture
displayRow row = 
  G.Pictures $ zipWith (\pic col-> G.translate (col*blockSize) 0 pic) elemPics colNums
  where 
    elemPics  = displayElem <$> toList row 
    numCols   = fromIntegral $ Seq.length row :: Float
    colNums   = [0.0..(numCols-1)]

displayElem :: Occupied -> G.Picture
displayElem Nothing          = G.Blank
displayElem (Just cubeColor) = G.translate xBase yBase $ G.color cubeColor cube


background :: G.Color
background = black

window :: G.Display
window = G.InWindow "Hasktris" (fullWidth, fullHeight) (offset, offset) 
  where offset = 100

sampleField :: Field
sampleField = Seq.fromList [Seq.fromList [Just red, Nothing, Just blue, Just yellow],
                            Seq.fromList [Nothing, Just red, Just yellow, Just blue]]

sampleImage :: G.Picture
sampleImage = G.Pictures [gameFrame, displayField sampleField]