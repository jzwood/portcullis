# PORTCULLIS INTERNAL
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

# USER CODE

# signature: (Byte -> Byte)
def id(x):
  return x

# signature: (Byte -> (Byte -> Atom))
def cmp(a):
  return lambda b: _eq_(a)(b)

# signature: (Atom -> (Num -> Num))
def incr(won):
  return lambda score: _plus_(score)((1.0 if won else _minus_(0.0)(1.0)))

FALSE = 0;
TRUE = 1;


pipes = [
  (id, [("&guess", 99)], "&newCoin"),
  (cmp, [("&guess", 99), ("&coin", 99)], "&win"),
  (incr, [("&win", 99), ("&score", 99)], "&score")
]
