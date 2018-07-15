import FakeIt2
import Prelude ()

promptString :: String -> Action String
promptString msg io1 =
  let (io2, ()) = print msg io1
      (io3, str) = getString io2
   in (io3, str)

mapAction :: (a -> b) -> Action a -> Action b
mapAction f action io1 =
  let (_, _) = action io1
   in (_, _)

promptInt :: String -> Action Int
promptInt msg = mapAction readInt (promptString msg)

inner :: Action ()
inner io1 =
  let (io2, age) = promptInt "What's your age?" io1
      (io3, ()) = print (showInt age) io2
   in (io3, ())

main = run inner
