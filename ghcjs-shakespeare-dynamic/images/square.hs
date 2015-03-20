{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface, QuasiQuotes #-}
module Main where

import JavaScript.Canvas
import JavaScript.JQuery
import GHCJS.Types
import GHCJS.Foreign
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import GHCJS.Foreign.QQ

import Data.Text
import Text.Printf

newtype Scale = Scale { unScale :: (Int, Int) } deriving (Eq, Show)

data TankColor = BlueTank | BlackTank deriving (Show, Eq)

type RenderableTank = TankGauge Image

type RawTank = TankGauge TankColor

data TankGauge imgType = TankGauge {
  tankGaugeMaxLevel :: Double
, tankGaugeCurrentLevel :: Double
, tankGaugeColor :: imgType
} deriving (Show, Eq)

toRenderable :: RawTank -> IO RenderableTank
toRenderable (TankGauge maxLevel currentLevel color) = TankGauge maxLevel currentLevel <$> getImage color

testGauge :: RawTank
testGauge = TankGauge 200 100 BlueTank

testGauge2 :: RawTank
testGauge2 = TankGauge 200 150 BlackTank


getImage :: TankColor -> IO Image
getImage BlueTank = createImage "animation-tankBlue.png"
getImage BlackTank = createImage "test1.png"


scaleWidthHeight :: Scale -> (Int,Int) -> (Int, Int)
scaleWidthHeight (Scale (absWidth, absHeight)) (width, height) = (floor width' , floor height')
  where width' = (fromIntegral $ width * absWidth) / 1000.0 :: Double
        height' = (fromIntegral $ height * absHeight )/ 1000.0 :: Double


main = do
  ctx <- getContext =<< indexArray 0 . castRef =<< select "#theCanvas"
  drawTankGauge ctx

drawTankGauge :: Context -> IO ()
drawTankGauge ctx = do
  i <- createImage "test.png"
  g <- createImage "animation-tankBlue.png"
  b <- createImage "test1.png"
  let sc = Scale (200,200)
  renderableGauge <- toRenderable testGauge
  let render = drawTank ctx i renderableGauge
  scale 3 3 ctx
  mapM_ (\ p -> onAnimation $ clearRect 0 0 2000 2000 ctx >> render (easeIn 1.0 p) ) $ [0.0,0.02..1.0]


easeIn :: Double -> Double -> Double
easeIn maxP t = maxP * if t < 0.5 
                         then 2*t*t 
                         else -1+(4-2*t)*t

drawTank :: Context -> Image -> RenderableTank -> Double -> IO ()
drawTank ctx baseImg  (TankGauge maxVal val overlay) progress = do
  let fullPercentage = progress * (val / maxVal)
      currentAnimVal = progress * val
  (width, height) <- imageDimensions overlay
  drawImage baseImg 0 0 124 200 ctx
  drawImagePercentage ctx overlay (124,200) fullPercentage
  drawLines ctx
  translate 0 (190 - 180 * fullPercentage) ctx
  drawCenterLine ctx
  translate 155 0 ctx
  drawTriangle ctx
  drawPrecentage ctx currentAnimVal
  translate (-155) 0 ctx
  translate 0 (- (190 - 180 * fullPercentage)) ctx
  return ()


drawPrecentage :: Context -> Double -> IO ()
drawPrecentage ctx val = do
  let dx = 10
      dy = 4
  translate  dx dy ctx
  font "12px Georgia" ctx
  fillText (pack $ printf "%.2f\n" (val)) 0 0 ctx
  translate (-dx) (-dy) ctx


-- | Given an image, (width, height) and the height percentage,
-- draw an image starting from the bottom of the screen until it hits correct percent of
-- the image using drawImageSlice
drawImagePercentage :: Context -> Image -> (Double, Double) -> Double -> IO ()
drawImagePercentage ctx img (ctxWidth, ctxHeight) perc = do
  (imgWidth, imgHeight) <- imageDimensions img
  let sPerc = scalePerc (0.05, 0.9) perc
      sx = 0
      sHeight = imgHeight * sPerc
      sy = imgHeight - sHeight
      sWidth = imgWidth
      dx = 0
      dWidth = ctxWidth
      dHeight = ctxHeight * sPerc
      dy = ctxHeight - dHeight
  drawImageSlice img sx sy sWidth sHeight dx dy dWidth dHeight ctx

scalePerc :: (Double,Double) -> Double -> Double
scalePerc (minP, maxP) p = minP + (maxP * p)

getPictureSlice :: Double -> Double -> Double -> Double
getPictureSlice minHeight maxHeight percentage = minHeight + ((maxHeight - minHeight) * percentage)

drawTriangle :: Context -> IO ()
drawTriangle ctx = do
  beginPath ctx
  moveTo 0 0 ctx
  lineTo 7.6 (-7.8) ctx
  lineTo 7.6 (7.8) ctx
  fill ctx
  closePath ctx


drawCenterLine :: Context -> IO ()
drawCenterLine ctx = do
  beginPath ctx
  lineWidth 0.5 ctx
  moveTo 5 0 ctx
  lineTo 155 0 ctx
  stroke ctx
  closePath ctx


drawLines :: Context -> IO ()
drawLines ctx = do
  beginPath ctx 
  moveTo 155 10 ctx
  lineTo 155 190 ctx
  lineWidth 2 ctx
  stroke ctx
  lineWidth 1 ctx
  translate 150 10 ctx
  mapM_ makeTick [0,18..180]
  translate (-150) (-10) ctx
  where makeTick :: Double -> IO ()
        makeTick y = do
          beginPath ctx
          moveTo 0 (y) ctx
          lineTo 6 (y) ctx
          stroke ctx
          closePath ctx



-----------------------------------------------------
-- Below are functions that I needed that aren't in
-- the canvas library right now
-----------------------------------------------------



-- | Look at https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Using_images#Slicing for more
-- info
drawImageSlice :: Image -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
drawImageSlice img sx sy sWidth sHeight dx dy dWidth dHeight ctx = do
  [js_|`ctx.drawImage(`img, `sx, `sy, `sWidth, `sHeight, `dx, `dy, `dWidth, `dHeight)|]



imageDimensions :: Image -> IO (Double, Double)
imageDimensions img = (,) <$> (imageWidth img) <*> (imageHeight img)

imageHeight :: Image -> IO Double
imageHeight img = [js|`img.height|]

imageWidth :: Image -> IO Double
imageWidth img = [js|`img.width|]


onAnimation :: IO () -> IO ()
onAnimation action = do
  mv <- newEmptyMVar
  cb <- syncCallback AlwaysRetain True (action >> putMVar mv ())
  [js_|window.requestAnimationFrame(`cb)|]
  takeMVar mv

-- | Load an image into a JSRef wrapper
-- and wait for the image to be loaded
createImage :: String -> IO Image
createImage imageName = do
  image <- newImage
  [js_| `image.src = `imageName|]
  mv <- newEmptyMVar
  cb <- syncCallback AlwaysRetain True (putMVar mv ()) -- Put the mvar when the image has been loaded
  [js_|`image.onload = `cb|]
  _ <- takeMVar mv
  return image

newImage :: IO Image
newImage = [js| new Image() |]