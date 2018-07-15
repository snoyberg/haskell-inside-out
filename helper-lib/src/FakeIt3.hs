module FakeIt3
  ( module FakeIt2
  , promptString
  , promptInt
  , mapAction
  ) where

import Prelude ()
import FakeIt2

promptString :: String -> Action String
promptString msg io1 =
  let (io2, ()) = print msg io1
      (io3, str) = getString io2
   in (io3, str)

mapAction :: (a -> b) -> Action a -> Action b
mapAction f action io1 =
  let (io2, a) = action io1
   in (io2, f a)

promptInt :: String -> Action Int
promptInt msg = mapAction readInt (promptString msg)
