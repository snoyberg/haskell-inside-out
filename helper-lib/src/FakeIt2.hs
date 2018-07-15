module FakeIt2
  ( run
  , Action
  , IOState
  , print
  , getString
  , Int
  , String
  , showInt
  , readInt
  ) where

import Prelude hiding (print)
import Control.Exception (evaluate)
import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)

data IOState = IOState

type Action a = IOState -> (IOState, a)

run :: Action () -> IO ()
run f = do
  let (x, y) = f IOState
  _ <- evaluate x
  void $ evaluate y

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

showInt :: Int -> String
showInt = show

readInt :: String -> Int
readInt = read
