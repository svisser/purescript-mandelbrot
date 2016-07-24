module Main where

import Control.Monad.Eff (foreachE, Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Array ((..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord ((<))
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS, Context2D, getCanvasElementById, getContext2D, setCanvasDimensions)
import Graphics.Canvas.Free (fillRect, runGraphics, setFillStyle)
import Prelude (Unit, bind, map, pure, ($))

type Pixel = Tuple Number Number
type PixelColor = String
type RenderedPixel = Tuple Pixel PixelColor

canvasWidth :: Number
canvasWidth = 800.0

canvasHeight :: Number
canvasHeight = 600.0

calculatePixelColor :: Pixel -> PixelColor
calculatePixelColor (Tuple x y) = if x < 30.0 then "red" else "white"

renderPixels :: forall e. Context2D -> Array RenderedPixel -> Eff (canvas :: CANVAS | e) Unit
renderPixels context rpixels = do
  foreachE rpixels $ \(Tuple (Tuple x y) color) ->
    runGraphics context $ do
      setFillStyle color
      fillRect { x: x, y: y, w: 1.0, h: 1.0 }

pixels :: Array Pixel
pixels = do
  x <- 0 .. (floor canvasWidth)
  y <- 0 .. (floor canvasHeight)
  pure (Tuple (toNumber x) (toNumber y))

pixelColors :: Array Pixel -> Array RenderedPixel
pixelColors = map (\px -> (Tuple px (calculatePixelColor px)))

main :: forall e. Eff (canvas :: CANVAS, console :: CONSOLE | e) Unit
main = do
  maybe_canvas <- getCanvasElementById "canvas"
  case maybe_canvas of
    Nothing -> do
      logShow "Unable to find canvas element"
    Just canvas -> do
      setCanvasDimensions { width: canvasWidth, height: canvasHeight } canvas
      context <- getContext2D canvas
      renderPixels context (pixelColors pixels)
