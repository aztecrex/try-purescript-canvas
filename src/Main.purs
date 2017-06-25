module Main where

import Prelude (Unit, class Show, show, negate, bind, discard, (/), (<>), (>>=), pure, ($), void, const)
import Data.Monoid (mempty)
import Data.Maybe
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Graphics.Canvas (CANVAS)
import Graphics.Canvas as C
import Graphics.Drawing as D

import DOM (DOM)

import Signal(Signal, (~>), runSignal, dropRepeats, foldp)
import Signal.DOM (keyPressed)

data Viewport = Viewport C.Context2D Number Number

data Model = Ship Boolean Number

instance showModel :: Show Model where
  show (Ship powered rotation) =
    "ship " <>
    show rotation <>
    if powered then " powered" else ""

modelLogic :: Boolean -> Model -> Model
modelLogic power (Ship _ rot) = Ship power rot

model :: Signal Boolean -> Signal Model
model input = foldp modelLogic (Ship false 0.0) input

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


scene :: ∀ eff. Viewport -> Eff (canvas :: CANVAS | eff) Unit
scene (Viewport context width height) = do
  ctx2 <- C.translate {translateX: width / 2.0, translateY: height / 2.0} context
  D.render ctx2 $ shipDrawing true

clearViewport :: ∀ eff. Viewport -> Eff (canvas :: CANVAS | eff) Unit
clearViewport (Viewport context w h) = do
  void $ C.setFillStyle "#FFFFFF" context
  void $ C.fillRect context {x: 0.0, y: 0.0, w, h}



render :: ∀ eff. Viewport -> Model -> Eff (canvas :: CANVAS | eff) Unit
render viewport@(Viewport context width height) (Ship powered _) = C.withContext context $ do
  void $ clearViewport viewport
  void $ C.translate {translateX: width / 2.0, translateY: height / 2.0} context
  D.render context $ shipDrawing powered


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




main :: ∀ eff. Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | eff) Unit
main = do
  up <- keyPressed 38
  let changeUp = dropRepeats up
  maybeViewport <- viewport
  case maybeViewport of
      Just viewport -> runSignal $ model changeUp ~> render viewport
      _ -> log "Cannot retrieve viewport canvas from document"
  log "Done"
  runSignal $ model changeUp ~> logShow
