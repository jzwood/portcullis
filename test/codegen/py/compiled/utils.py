# signature: ((not.a -> (not.a -> Atom)) -> (not.a -> (not.a -> Atom)))
def not(f):
  return lambda a: lambda b: (TRUE if _eq_(FALSE)(f(a)(b)) else FALSE)

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


