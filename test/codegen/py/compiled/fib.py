# signature: (Num -> Num)
def fib(n):
  return (1.0 if _lte_(n)(1.0) else _plus_(fib(_minus_(n)(1.0)))(fib(_minus_(n)(2.0))))

# signature: (Num -> Num)
def neg(x):
  return _minus_(0.0)(x)

FALSE = 0;
TRUE = 1;


pipes = [];
# INTERNAL
def _plus_(a):
    return lambda b: a + b

def _minus_(a):
    return lambda b: a - b

def _mult_(a):
    return lambda b: a * b

def _div_(a):
    return lambda b: a / b

def _gt_(a):
    return lambda b: 1 if a > b else 0

def _gte_(a):
    return lambda b: 1 if a >= b else 0

def _lt_(a):
    return lambda b: 1 if a < b else 0

def _lte_(a):
    return lambda b: 1 if a <= b else 0

def _eq_(a):
    return lambda b: 1 if a == b else 0

def _cons_(a):
    return lambda b: [a] + b

