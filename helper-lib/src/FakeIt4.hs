module FakeIt4
  ( module FakeIt3
  , echoName
  , echoAge
  ) where

import FakeIt3
import Prelude ()

echoName :: Action ()
echoName io1 =
  let (io2, name) = promptString "What's your name?" io1
      (io3, ()) = print name io2
   in (io3, ())

echoAge :: Action ()
echoAge io1 =
  let (io2, age) = promptInt "What's your age?" io1
      (io3, ()) = print (showInt age) io2
   in (io3, ())
