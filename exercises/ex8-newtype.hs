import FakeIt5
import Prelude ()

echoName :: Action ()
echoName = andThen
  (promptString "What's your name?")
  print

promptString :: String -> Action String
promptString msg = doBoth (print msg) getString

promptInt :: String -> Action Int
promptInt msg = mapAction readInt (promptString msg)

echoAge :: Action ()
echoAge = andThen
  (promptInt "What's your age?")
  printInt

inner :: Action ()
inner = doBoth echoName echoAge

main = run inner
