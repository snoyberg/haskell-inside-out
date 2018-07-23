import FakeIt3
import Prelude ((.))

echoName :: Action ()
echoName = andThen
  (promptString "What's your name?")
  print

echoAge :: Action ()
echoAge = andThen
  (promptInt "What's your age?")
  printInt

printInt :: Int -> Action ()
printInt = print . showInt

andThen :: Action a -> (a -> Action b) -> Action b
andThen getVal useVal io1 =
  _

inner :: Action ()
inner = doBoth echoName echoAge

doBoth :: Action a -> Action b -> Action b
doBoth action1 action2 io1 =
  let (io2, _a) = action1 io1
      (io3, b) = action2 io2
   in (io3, b)

main = run inner
