module Main where

import Prelude (Unit, class Show, (&&), (<$>), (<*>), map, show, negate, bind, discard, (/), (<>), (>>=), pure, ($), void, const)
import Data.Monoid (mempty)
import Data.Maybe
import Data.Eq (class Eq, eq)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Graphics.Canvas (CANVAS)
import Graphics.Canvas as C
import Graphics.Drawing as D

import DOM (DOM)

import Signal(Signal, (~>), runSignal, dropRepeats, dropRepeats', foldp, map3)
import Signal.DOM (keyPressed)

data Viewport = Viewport C.Context2D Number Number

data Model = Ship Boolean Number

data Control = Control Boolean Boolean Boolean

instance showModel :: Show Model where
  show (Ship powered rotation) =
    "ship " <>
    show rotation <>
    if powered then " powered" else ""

modelLogic :: Control -> Model -> Model
modelLogic (Control _ up _) (Ship _ rot) = Ship up rot

model :: Signal Control -> Signal Model
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
render vp@(Viewport context width height) (Ship powered _) = C.withContext context $ do
  void $ clearViewport vp
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

instance showControl :: Show Control where
  show (Control left up right) =
    "l: " <> show left <>
    " u:" <> show up <>
    " r:" <> show right

instance eqControl :: Eq Control where
  eq (Control a b c) (Control a' b' c') =
      eq a a' &&
      eq b b' &&
      eq c c'

logDir :: ∀ eff. String -> Boolean -> Eff (console :: CONSOLE | eff) Unit
logDir s b = log $ s <> " " <> show b

main :: ∀ eff. Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | eff) Unit
main = do
  up <- keyPressed 38
  left <- keyPressed 37
  right <- keyPressed 39
  let all = dropRepeats (Control <$> left <*> up <*> right)
  let ship = model all
  maybeViewport <- viewport
  case maybeViewport of
      Just vp -> runSignal $ map (render vp) ship
      _ -> log "Cannot retrieve viewport canvas from document"
  log "Done"
