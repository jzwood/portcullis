# signature: (cons.a -> ([cons.a] -> [cons.a]))
def cons(x):
  return lambda xs: _cons_(x)(_cons_(x)(_cons_(x)(xs)))

# signature: (count.a -> ([count.a] -> Num))
def count(x):
  return lambda xs: _plus_(1.0)(length(xs))

# signature: ([uncons.a] -> (uncons.b -> ((uncons.a -> ([uncons.a] -> uncons.b)) -> uncons.b)))
def uncons(as):
  return lambda b: lambda f: b

# signature: ((tail2.w -> ([tail2.w] -> [tail2.w])) -> ([tail2.f] -> [tail2.f]))
def tail2(f):
  return lambda ns: uncons(ns)(ns)(f)

# signature: (_length.a -> ([_length.a] -> Num))
def _length(x):
  return lambda xs: _plus_(1.0)(length(xs))

# signature: ([length.a] -> Num)
def length(xs):
  return (0.0 if _eq_(xs)([]) else _length(xs[0])(xs[1:]))

# signature: ([push.a] -> (push.a -> ([push.a] -> [push.a])))
def push(ys):
  return lambda x: lambda xs: _cons_(x)(concat(xs)(ys))

# signature: ([concat.a] -> ([concat.a] -> [concat.a]))
def concat(xs):
  return lambda ys: (ys if _eq_(xs)([]) else push(ys)(xs[0])(xs[1:]))

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

