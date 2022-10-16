# signature: (Num -> (Num -> Num))
def add(a):
  return lambda b: (a + b)

# signature: (Num -> (Num -> Num))
def sub(a):
  return lambda b: (a - b)

FALSE = 0;
TRUE = 1;


pipes = [
  (add, [("&counter", 1), ("&add", 100)], "&counter"),
  (sub, [("&counter", 1), ("&sub", 100)], "&counter")
]
