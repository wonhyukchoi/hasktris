-- | Module that contains all UI and user interactions.
module Frontend where

import Tetris
import qualified Data.Sequence as Seq
import qualified Graphics.Gloss as G
import Data.Foldable(toList)
import Data.Bifunctor(bimap)
import Graphics.Gloss.Interface.Pure.Game
import System.Random

playGame :: IO ()
playGame = do
  rand <- getStdGen
  let initGame = startGame rand
  play window background fps initGame displayGame handleKeys update

-- | Maps key inputs to appopriate block movement actions.
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (SpecialKey KeyDown) _ _ _)   = updateGame moveDown
handleKeys (EventKey (SpecialKey KeyLeft) _ _ _)   = updateGame moveLeft
handleKeys (EventKey (SpecialKey KeyRight) _ _ _)  = updateGame moveRight
handleKeys (EventKey (SpecialKey KeySpace) _ _ _)  = updateGame counterClockwise
handleKeys _                                       = id

update :: Float -> Game -> Game
update = updateGame . moveDownFloats

-- | Frames per second.
fps :: Int
fps = 1

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

tetrisText :: G.Picture
tetrisText = moveTitle . addColor . scaleDown $ text
  where 
    coolBlue    = G.makeColorI 0xcc 0xff 0xee 0
    lightPurple = G.makeColorI 0xff 0xb3 0xff 0
    addColor    = G.color lightPurple
    xPosition   = -160
    yPosition   = 
      fromIntegral ((fullHeight `div` 2 - topMargin) + topMargin `div` 2)
    moveTitle   = G.translate xPosition yPosition
    scaleFactor = 0.35
    scaleDown   = G.scale scaleFactor scaleFactor
    text        = G.Text "T E T R I S"

scoreText :: G.Picture
scoreText = moveText . G.color G.white . scaleDown $ text
  where 
    xPosition   = -350
    yPosition   = 350
    moveText    = G.translate xPosition yPosition
    scaleFactor = 0.35
    scaleDown   = G.scale scaleFactor scaleFactor
    text        = G.Text "score"

blackCover :: G.Picture
blackCover =  move . mkBlack $ shape
  where 
    mkBlack = G.color G.black
    yLoc    = fromIntegral ((fullHeight `div` 2 - topMargin) + topMargin `div` 2)
    move    = G.translate 0.0 yLoc
    shape   = G.rectangleSolid (fromIntegral fullWidth) (fromIntegral topMargin)

gameFrame :: G.Picture
gameFrame = G.color G.white $ G.rectangleWire width height
  where width  = fromIntegral gameWidth
        height = fromIntegral gameHeight

cube :: G.Picture
cube = G.rectangleSolid blockSize blockSize

displayCube :: G.Color -> G.Picture
displayCube color = G.Pictures [inside, outside]
  where inside  = G.color color cube
        outside = G.rectangleWire blockSize blockSize

displayBlock :: Block -> G.Picture
displayBlock block@(Block shape location cubeColor) = 
  G.pictures $ map translatePair locations' <*> [oneCube]
  where 
    locations     = locateCubes block
    upSizeDim     = (* blockSize) . fromIntegral
    fixDimX x     = upSizeDim x + xBase
    fixDimY y     = upSizeDim y + yBase
    locations'    = map (bimap fixDimX fixDimY) locations
    oneCube       = displayCube cubeColor
    translatePair = uncurry G.translate

-- | Fix these areas
displayField :: Field -> G.Picture
displayField field = 
  G.Pictures $ zipWith (\pic col-> G.translate (col*blockSize) 0 pic) colPics colNums
  where 
    colPics  = displayCol <$> toList field
    numCols  = fromIntegral $ Seq.length field :: Float
    colNums  = [0.0..(numCols-1)]

displayCol :: Column -> G.Picture
displayCol row = 
  G.Pictures $ zipWith (\pic row-> G.translate 0 (row*blockSize) pic) elemPics rowNums
  where 
    elemPics  = displayElem <$> toList row 
    numRows   = fromIntegral $ Seq.length row :: Float
    rowNums   = [0.0..(numRows-1)]

displayElem :: Occupied -> G.Picture
displayElem Nothing          = G.Blank
displayElem (Just cubeColor) = G.translate xBase yBase $ displayCube cubeColor

displayScore :: Int -> G.Picture
displayScore score = moveScore . addColor . scaleDown $ text 
  where
    addColor    = G.color G.white
    xPosition   = -350
    yPosition   = 275
    moveScore   = G.translate xPosition yPosition
    scaleFactor = 0.35
    scaleDown   = G.scale scaleFactor scaleFactor
    text        = G.Text $ show score   

displayGameOver :: Game -> G.Picture
displayGameOver game = G.Pictures [line1, line2, line3]
  where 
    scaleFactor = 0.35
    scaleDown   = G.scale scaleFactor scaleFactor

    gameOver    = scaleDown $ G.color G.red $ G.Text "GAME OVER !\n"
    line1       = G.translate (-150) 300 gameOver
    
    saysScore   = scaleDown $ G.color G.white $ G.Text "your score:"
    line2       = G.translate (-150) 100 saysScore

    finalScore  = score game
    showScore   = scaleDown $ G.color G.red $ G.Text $ show finalScore
    line3       = G.translate (-50) (-100) showScore

displayGame :: Game -> G.Picture
displayGame game@(Game _ _ _ _ False) = displayGameOver game
displayGame (Game field score rand block playing) = 
  G.Pictures [displayField field
             ,displayBlock block
             ,displayScore score
             ,gameFrame
             ,blackCover
             ,tetrisText
             ,scoreText]

background :: G.Color
background = black

window :: G.Display
window = G.InWindow "Hasktris" (fullWidth, fullHeight) (offset, offset) 
  where offset = 100
