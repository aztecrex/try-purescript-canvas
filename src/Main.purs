module Main where

import Prelude (Unit, bind, discard, (>>=), pure, (<<<), ($), void, const)
import Data.Maybe
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Graphics.Canvas (CANVAS)
import Graphics.Canvas as C
-- import Graphics.Drawing as D

scene :: ∀ eff. C.Context2D -> Eff (canvas :: CANVAS | eff) C.Context2D
scene ctx = do
  ctx2 <- C.moveTo ctx 100.0 0.0
  ctx3 <- C.lineTo ctx2 0.0 100.0
  C.stroke ctx3

viewport :: ∀ eff. Eff (console :: CONSOLE, canvas :: CANVAS | eff) (Maybe C.Context2D)
viewport  = do
  maybeCanvas <- C.getCanvasElementById "viewport"
  case maybeCanvas of
    Just canvas -> C.getContext2D canvas >>= (pure <<< Just)
    _ -> log "no canvas 'viewport'" >>= (const $ pure Nothing)

main :: ∀ eff. Eff (console :: CONSOLE, canvas :: CANVAS | eff) Unit
main = do
  maybeContext <- viewport
  case maybeContext of
      Just context -> void $ scene context
      _ -> log "Cannot retrieve viewport canvas from document"
  log "Done"
