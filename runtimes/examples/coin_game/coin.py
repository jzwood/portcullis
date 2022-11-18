# signature: (id.a -> id.a)
def id(x):
  return x

# signature: (Char -> (Char -> (Num -> Num)))
def incr(guess):
  return lambda actual: lambda score: (score + 1.0 if (guess == actual) else 0.0)

FALSE = 0;
TRUE = 1;


pipes = [
  (id, [("&guess", 99)], "&newCoin"),
  (incr, [("&guess", 99), ("&coin", 99), ("&score", 99)], "&score")
]
