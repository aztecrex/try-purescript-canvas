module Main where

import Prelude (Unit, bind, discard)
import Data.Maybe
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Partial.Unsafe (unsafePartial)

import Graphics.Canvas (CANVAS)
import Graphics.Canvas as C
-- import Graphics.Drawing as D

scene :: ∀ eff. C.Context2D -> Eff (canvas :: CANVAS | eff) C.Context2D
scene ctx = do
  ctx2 <- C.moveTo ctx 100.0 0.0
  ctx3 <- C.lineTo ctx2 0.0 100.0
  C.stroke ctx3


innerMain :: ∀ eff. Partial => Eff (console :: CONSOLE, canvas :: CANVAS | eff) Unit
innerMain = do
  log "Purescript to the rescue!!"
  Just canvas <- C.getCanvasElementById "viewport"  -- bad, fix
  ctx <- C.getContext2D canvas
  _ <- scene ctx
  log "Done"

main :: ∀ eff. Eff (console :: CONSOLE, canvas :: CANVAS | eff) Unit
main = unsafePartial innerMain
