module Main where

import Control.Monad.Eff (foreachE, Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Array ((..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS, Context2D, getCanvasElementById, getContext2D, setCanvasDimensions)
import Graphics.Canvas.Free (fill, rect, runGraphics, setFillStyle)
import Prelude (Unit, bind, pure, ($))

canvasWidth :: Number
canvasWidth = 800.0

canvasHeight :: Number
canvasHeight = 600.0

calculatePixel :: Number -> Number -> Boolean
calculatePixel x y = true

renderPixel :: forall e. Context2D -> Number -> Number -> Eff (canvas :: CANVAS | e) Unit
renderPixel context x y = do
  runGraphics context $ do
    let pixelColor = if calculatePixel x y then "black" else "white"
    setFillStyle pixelColor
    rect { x: x, y: y, w: 1.0, h: 1.0 }
    fill

pixels :: Array (Tuple Number Number)
pixels = do
  x <- 0 .. (floor canvasWidth)
  y <- 0 .. (floor canvasHeight)
  pure (Tuple (toNumber x) (toNumber y))


main :: forall e. Eff (canvas :: CANVAS, console :: CONSOLE | e) Unit
main = do
  maybe_canvas <- getCanvasElementById "canvas"
  case maybe_canvas of
    Nothing -> do
      logShow "Unable to find canvas element"
    Just canvas -> do
      setCanvasDimensions { width: canvasWidth, height: canvasHeight } canvas
      context <- getContext2D canvas
      foreachE pixels $ \(Tuple x y) ->
        renderPixel context x y
