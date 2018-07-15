import FakeIt4
import Prelude ()

inner :: Action ()
inner = doBoth echoName echoAge

doBoth :: Action () -> Action () -> Action ()
doBoth action1 action2 io1 =
  _

main = run inner
