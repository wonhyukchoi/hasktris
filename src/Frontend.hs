-- | Module that contains all UI and user interactions.
module Frontend where

import Tetris
import qualified Data.Sequence as Seq
import qualified Graphics.Gloss as G
import Data.Foldable(toList)
import Data.Bifunctor(bimap)
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State

main :: IO ()
main = testDisplay sampleImage
-- main = G.display window background $ render initGame
-- main = G.animate window background frame
--   where 
--     frame :: Float -> G.Picture
--     frame sec = render $ moveBlock moveDown sec initGame

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

gameBottom, gameLeft :: Float
gameBottom = -(fromIntegral fullHeight/2 - fromIntegral bottomMargin)
gameLeft = -(fromIntegral fullWidth/2 - fromIntegral sideMargin)  

xBase, yBase :: Float
xBase = gameLeft + blockSize/2
yBase = gameBottom + blockSize/2

-- | TODO
render :: Game -> G.Picture
render game = G.Pictures [gameFrame, displayBlock $ mkBlock T]

-- | For testing purposes
moveGame :: Float -> Game -> Game
moveGame f = execState (updateGame moveDown)

gameFrame :: G.Picture
gameFrame = G.color G.white $ G.rectangleWire width height
  where width  = fromIntegral gameWidth
        height = fromIntegral gameHeight

cube :: G.Picture
cube = G.rectangleSolid blockSize blockSize

displayBlock :: Block -> G.Picture
displayBlock block@(Block shape location cubeColor) = 
  G.pictures $ map translatePair locations' <*> [cube]
  where 
    locations     = locateCubes block
    upSizeDim     = (* blockSize) . fromIntegral
    fixDimX x     = upSizeDim x + gameLeft
    fixDimY y     = upSizeDim y + gameBottom
    locations'    = map (bimap fixDimX fixDimY) locations
    cube          = G.color cubeColor cube
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