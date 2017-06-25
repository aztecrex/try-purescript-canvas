module Main where

import Prelude (Unit, negate, bind, discard, (/), (<>), (>>=), pure, ($), void, const)
import Data.Monoid (mempty)
import Data.Maybe
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Graphics.Canvas (CANVAS)
import Graphics.Canvas as C
import Graphics.Drawing as D


data Viewport = Viewport C.Context2D Number Number

data Model = Ship Number

shipShape :: D.Shape
shipShape = D.closed [
    {x: 20.0, y:  0.0},
    {x:  (-20.0), y: 15.0},
    {x:  (-20.0), y: (-15.0)}
  ]

trailShape :: D.Shape
trailShape = D.closed [
    {x: -30.0, y:  0.0},
    {x: -20.0, y:  5.0},
    {x: -20.0, y: -5.0}
  ]

shipDrawing :: Boolean -> D.Drawing
shipDrawing powered = body <> if powered then tail else mempty
      where
        body = D.outlined
              (D.outlineColor D.black <> D.lineWidth 1.0)
              shipShape
        tail = D.filled
              (D.fillColor D.black)
              trailShape


-- render :: ∀ eff. C.Context2D -> Model -> Eff (canvas :: CANVAS | eff) C.Context2D
-- render ctx model = do
--   let shape =

scene :: ∀ eff. Viewport -> Eff (canvas :: CANVAS | eff) Unit
scene (Viewport context width height) = do
  ctx2 <- C.translate {translateX: width / 2.0, translateY: height / 2.0} context
  D.render ctx2 $ shipDrawing true

viewport :: ∀ eff. Eff (console :: CONSOLE, canvas :: CANVAS | eff) (Maybe Viewport)
viewport  = do
  maybeCanvas <- C.getCanvasElementById "viewport"
  case maybeCanvas of
    Just canvas -> do
      context <- C.getContext2D canvas
      width <- C.getCanvasWidth canvas
      height <- C.getCanvasHeight canvas
      pure $ Just $ Viewport context width height
      -- C.getContext2D canvas >>= (pure <<< Just)
    _ -> log "no canvas 'viewport'" >>= (const $ pure Nothing)

main :: ∀ eff. Eff (console :: CONSOLE, canvas :: CANVAS | eff) Unit
main = do
  maybeContext <- viewport
  case maybeContext of
      Just context -> void $ scene context
      _ -> log "Cannot retrieve viewport canvas from document"
  log "Done"
