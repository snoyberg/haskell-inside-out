myAnd :: Bool -> Bool -> Bool
myAnd True x = x
myAnd False _ = False

main :: IO ()
main = do
  print (myAnd (2 > 1) (3 > 2))
  print (myAnd (2 > 1) (1 > 2))
  print (myAnd (1 > 2) (2 > 1))
  print (myAnd (1 > 2) (2 > undefined))
  print (myAnd (2 > 1) (2 > undefined))
