module FakeIt5
  ( Action
  , Int
  , String
  , run
  , mapAction
  , doBoth
  , andThen
  , print
  , printInt
  , getString
  , getInt
  , readInt
  , showInt
  ) where

import Prelude hiding (print)
import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)

data IOState = IOState

newtype Action a = Action (IOState -> (IOState, a))

run :: Action a -> IO a
run (Action f) = do
  let (x, y) = f IOState
  _ <- evaluate x
  evaluate y

getString :: Action String
getString = Action $ \io1 -> unsafePerformIO $ do
  io2 <- evaluate io1
  str <- getLine
  pure (io2, str)

getInt :: Action Int
getInt = mapAction readInt getString

print :: String -> Action ()
print str = Action $ \io1 -> unsafePerformIO $ do
  io2 <- evaluate io1
  putStrLn str
  pure (io2, ())

showInt :: Int -> String
showInt = show

readInt :: String -> Int
readInt = read

mapAction :: (a -> b) -> Action a -> Action b
mapAction f (Action action) = Action $ \io1 ->
  let (io2, a) = action io1
   in (io2, f a)

doBoth :: Action a -> Action b -> Action b
doBoth a1 a2 = andThen a1 (const a2)

andThen :: Action a -> (a -> Action b) -> Action b
andThen (Action a1) mkA2 = Action $ \io1 ->
  let (io2, a) = a1 io1
      Action a2 = mkA2 a
   in a2 io2

printInt :: Int -> Action ()
printInt i = print (showInt i)
