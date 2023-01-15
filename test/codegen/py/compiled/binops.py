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

# signature: (Num -> (Num -> Num))
def add(a):
  return lambda b: _plus_(a)(b)

# signature: (Num -> (Num -> (Num -> Num)))
def add3(a):
  return lambda b: lambda c: _plus_(_plus_(a)(b))(c)

# signature: (Num -> Num)
def calc0(num):
  return _plus_(_minus_(0.0)(_mult_(num)(9.0)))(_div_(3.0)(4.0))

# signature: (Num -> Num)
def calc1(num):
  return _plus_(_minus_(0.0)(_mult_(num)(9.0)))(_div_(3.0)(4.0))

# signature: (Num -> Atom)
def choose0(num):
  return (COOL if _gt_(num)(0.0) else (HMM if _eq_(num)(0.0) else BAD))

# signature: (Num -> Atom)
def choose1(num):
  return (COOL if _lte_(num)(0.0) else (HMM if _lt_(num)(100.0) else BAD))

# signature: (Num -> Atom)
def isEven(num):
  return _eq_(_rem_(num)(2.0))(0.0)

# signature: (append3.a -> ([append3.a] -> [append3.a]))
def append3(x):
  return lambda xs: _cons_(x)(_cons_(x)(_cons_(x)(xs)))

FALSE = 0;
TRUE = 1;
COOL = 2;
HMM = 3;
BAD = 4;


pipes = [];
