module FakeIt2
  ( run
  , Action
  , IOState
  , print
  , getString
  , Int
  , String
  ) where

import Prelude hiding (print)
import Control.Exception (evaluate)
import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)

data IOState = IOState

type Action a = IOState -> (IOState, a)

run :: Action () -> IO ()
run f = void $ evaluate $ f IOState

getString :: Action String
getString io1 = unsafePerformIO $ do
  io2 <- evaluate io1
  str <- getLine
  pure (io2, str)

print :: String -> Action ()
print str io1 = unsafePerformIO $ do
  io2 <- evaluate io1
  putStrLn str
  pure (io2, ())
