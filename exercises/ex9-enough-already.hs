import FakeIt6
import Prelude ()

echoName :: Action ()
echoName = do
  name <- promptString "What's your name?"
  print name

promptString :: String -> Action String
promptString msg = do
  print msg
  getString
  -- or
  -- print msg >> getString

promptInt :: String -> Action Int
promptInt msg = do
  str <- promptString msg
  pure (readInt str)
  -- or
  -- readInt <$> promptString msg

echoAge :: Action ()
echoAge = do
  age <- promptInt "What's your age?"
  printInt age

inner :: Action ()
inner = do
  echoName
  echoAge
  -- or
  -- echoName >> echoAge

main = run inner
