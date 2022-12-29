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

# signature: (Num -> (Num -> Num))
def add(a):
  return lambda b: _plus_(a)(b)

# signature: (Num -> (Num -> Num))
def sub(a):
  return lambda b: _minus_(a)(b)

FALSE = 0;
TRUE = 1;


pipes = [
  (add, [("&counter", 1), ("&add", 100)], "&counter"),
  (sub, [("&counter", 1), ("&sub", 100)], "&counter")
]
