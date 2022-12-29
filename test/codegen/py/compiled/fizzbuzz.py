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

# signature: ((_map.a -> _map.b) -> (_map.a -> ([_map.a] -> [_map.b])))
def _map(f):
  return lambda x: lambda xs: _cons_(f(x))(map(f)(xs))

# signature: ((map.a -> map.b) -> ([map.a] -> [map.b]))
def map(f):
  return lambda xs: ([] if _eq_(xs)([]) else _map(f)(xs[0])(xs[1:]))

# signature: (Num -> (Num -> [Num]))
def range(n0):
  return lambda n1: ([] if _gt_(n0)(n1) else _cons_(n0)(range(_plus_(n0)(1.0))(n1)))

# signature: (Num -> Num)
def _fizzbuzz(n):
  return (_minus_(0.0)(35.0) if _eq_(0.0)(_rem_(n)(15.0)) else (_minus_(0.0)(5.0) if _eq_(0.0)(_rem_(n)(5.0)) else (_minus_(0.0)(3.0) if _eq_(0.0)(_rem_(n)(3.0)) else n)))

# signature: (Num -> [Num])
def fizzbuzz(n):
  return map(_fizzbuzz)(range(1.0)(n))

# signature: [Num]
def __fb():
  return fizzbuzz(20.0)

FALSE = 0;
TRUE = 1;

fb = __fb()

pipes = [];
