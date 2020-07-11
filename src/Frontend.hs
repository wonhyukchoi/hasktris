-- | Module that contains all UI and user interactions.
module Frontend where

import Tetris
import qualified Graphics.Gloss as G
import Data.Bifunctor(bimap)
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = G.display window background image

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

gameBottom :: Float
gameBottom = -(fromIntegral fullHeight/2 - fromIntegral bottomMargin)

gameLeft :: Float
gameLeft = -(fromIntegral fullWidth/2 - fromIntegral sideMargin)

-- | FIXME: currently at test image
image :: G.Picture
image = G.Pictures [frame, showBlock $ mkBlock T]

frame :: G.Picture
frame = G.color G.white $ G.rectangleWire width height
  where width  = fromIntegral gameWidth
        height = fromIntegral gameHeight

showBlock :: Block -> G.Picture
showBlock block@(Block shape location cubeColor) = 
  G.pictures $ map translatePair locations' <*> [cube]
  where 
    locations     = locateCubes block
    upSizeDim     = (* blockSize) . fromIntegral
    fixDimX x     = upSizeDim x + gameLeft
    fixDimY y     = upSizeDim y + gameBottom
    locations'    = map (bimap fixDimX fixDimY) locations
    cube          = G.color cubeColor $ G.rectangleSolid blockSize blockSize
    translatePair = uncurry G.translate

-- | TODO
showField :: Field -> G.Picture
showField _ = error "Not implemented"

background :: G.Color
background = black

window :: G.Display
window = G.InWindow "Hasktris" (fullWidth, fullHeight) (offset, offset) 
  where offset = 100