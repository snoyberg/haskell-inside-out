main = print inner

inner :: Int
inner =
  let w = error "this is not needed!"
      x = 2 + 3
      y = 4 + 5
   in x * y
