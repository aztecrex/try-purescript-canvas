module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)


main :: ∀ eff. Eff (console :: CONSOLE | eff) Unit
main = log "Purescript to the rescue!"
