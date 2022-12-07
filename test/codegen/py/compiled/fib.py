# signature: (Num -> Num)
def fib(n):
  return (1.0 if (n <= 1.0) else (fib((n - 1.0)) + fib((n - 2.0))))

# signature: (Num -> Num)
def neg(x):
  return (0.0 - x)

FALSE = 0;
TRUE = 1;


pipes = [];
