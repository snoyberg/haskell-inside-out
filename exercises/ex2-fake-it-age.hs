import FakeIt1
import Prelude ()

main = run inner

getInt :: IOState -> (Int, IOState)
getInt io1 =
  let (_, _) = getString io1
      int = read str
   in _

inner :: IOState -> IOState
inner io1 =
  let io2 = print "What's your age?" io1
      (_, _) = getInt _
      _ = print (show age) _
   in _
