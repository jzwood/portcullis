# signature: (id.a -> id.a)
def id(x):
  return x

# signature: (cmp.x -> (cmp.x -> Atom))
def cmp(a):
  return lambda b: (a == b)

# signature: (Atom -> (Num -> Num))
def incr(won):
  return lambda score: (score + (1.0 if won else (0.0 - 1.0)))

FALSE = 0;
TRUE = 1;


pipes = [
  (id, [("&guess", 99)], "&newCoin"),
  (cmp, [("&guess", 99), ("&coin", 99)], "&win"),
  (incr, [("&win", 99), ("&score", 99)], "&score")
]
