module FakeIt
  ( run
  , IOState
  , print
  , getString
  ) where

import Prelude hiding (print)
import Control.Exception (evaluate)
import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)

data IOState = IOState

run :: (IOState -> IOState) -> IO ()
run f = void $ evaluate $ f IOState

getString :: IOState -> (String, IOState)
getString io1 = unsafePerformIO $ do
  io2 <- evaluate io1
  str <- getLine
  pure (str, io2)

print :: String -> IOState -> IOState
print str io1 = unsafePerformIO $ do
  io2 <- evaluate io1
  putStrLn str
  pure io2
