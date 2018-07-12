import FakeIt1
import Prelude ()

main = run inner

inner :: IOState -> IOState
inner io1 =
  let io2 = print "What's your name?" io1
      (_, _) = getString _
      _ = print name _
   in _
