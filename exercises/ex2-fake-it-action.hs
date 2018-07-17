import FakeIt2
import Prelude ()

main = run inner

inner :: Action ()
inner io1 =
  let (_, _) = print "What's your name?" io1
      (_, _) = getString io2
      (_, _) = print name io3
   in (_, ())
