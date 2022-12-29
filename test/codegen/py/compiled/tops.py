def _plus_(a):
    return lambda b: a + b

def _minus_(a):
    return lambda b: a - b

def _mult_(a):
    return lambda b: a * b

def _div_(a):
    return lambda b: a / b

def _rem_(a):
    return lambda b: a % b

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

# signature: (Atom -> Num)
def hello(what):
  return (42.0 if _eq_(what)(WORLD) else (10.0 if _eq_(what)(MA) else _minus_(0.0)(100.0)))

# signature: (Num -> ([Num] -> Num))
def _sum(x):
  return lambda xs: _plus_(x)(sum(xs))

# signature: ([Num] -> Num)
def sum(xs):
  return (0.0 if _eq_(xs)([]) else _sum(xs[0])(xs[1:]))

FALSE = 0;
TRUE = 1;
WORLD = 2;
MA = 3;


pipes = [];
