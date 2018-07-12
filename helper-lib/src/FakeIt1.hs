module FakeIt1
  ( run
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

run :: (IOState -> IOState) -> IO ()
run f = void $ evaluate $ f IOState

getString :: IOState -> (IOState, String)
getString io1 = unsafePerformIO $ do
  io2 <- evaluate io1
  str <- getLine
  pure (io2, str)

print :: String -> IOState -> IOState
print str io1 = unsafePerformIO $ do
  io2 <- evaluate io1
  putStrLn str
  pure io2
