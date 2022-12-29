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

# signature: ((_not.a -> (_not.a -> Atom)) -> (_not.a -> (_not.a -> Atom)))
def _not(f):
  return lambda a: lambda b: (TRUE if _eq_(FALSE)(f(a)(b)) else FALSE)

# signature: (_length.a -> ([_length.a] -> Num))
def _length(x):
  return lambda xs: _plus_(1.0)(length(xs))

# signature: ([length.a] -> Num)
def length(xs):
  return (0.0 if _eq_(xs)([]) else _length(xs[0])(xs[1:]))

# signature: ([push.h] -> (push.h -> ([push.h] -> [push.h])))
def push(ys):
  return lambda x: lambda xs: _cons_(x)(concat(xs)(ys))

# signature: ([concat.a] -> ([concat.a] -> [concat.a]))
def concat(xs):
  return lambda ys: (ys if _eq_(xs)([]) else push(ys)(xs[0])(xs[1:]))

# signature: (identity2.x -> ([identity2.x] -> [identity2.x]))
def identity2(x):
  return lambda xs: xs

# signature: ([tail.p] -> [tail.p])
def tail(xs):
  return (xs if _eq_(xs)([]) else identity2(xs[0])(xs[1:]))

# signature: ([drop.g] -> (Num -> [drop.g]))
def drop(xs):
  return lambda n: (xs if _lte_(n)(0.0) else drop(tail(xs))(_minus_(n)(1.0)))

# signature: (Num -> (_take.f -> ([_take.f] -> [_take.f])))
def _take(n):
  return lambda x: lambda xs: ([] if _lte_(n)(0.0) else _cons_(x)(take(xs)(_minus_(n)(1.0))))

# signature: ([take.k] -> (Num -> [take.k]))
def take(xs):
  return lambda n: ([] if _eq_(xs)([]) else _take(n)(xs[0])(xs[1:]))

# signature: ([slice.q] -> (Num -> (Num -> [slice.q])))
def slice(xs):
  return lambda i: lambda j: take(drop(xs)(i))(_minus_(j)(i))

# signature: ((filter2.x -> Atom) -> (filter2.x -> ([filter2.x] -> [filter2.x])))
def filter2(g):
  return lambda w: lambda ws: concat(([w] if g(w) else []))(filter(g)(ws))

# signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
def filter(f):
  return lambda xs: (xs if _eq_(xs)([]) else filter2(f)(xs[0])(xs[1:]))

# signature: (Num -> Num)
def neg(x):
  return _minus_(0.0)(x)

# signature: [Num]
def __empty():
  return []

# signature: ([Num] -> [Num])
def msort(ns):
  return msort2(length(ns))(ns)

# signature: (Num -> ([Num] -> [Num]))
def msort2(len):
  return lambda ns: (ns if _lte_(len)(1.0) else merge(msort(take(ns)(_div_(len)(2.0))))(msort(drop(ns)(_div_(len)(2.0)))))

# signature: (Num -> ([Num] -> (Num -> ([Num] -> [Num]))))
def merge3(x):
  return lambda xs: lambda y: lambda ys: (_cons_(x)(merge(xs)(_cons_(y)(ys))) if _lte_(x)(y) else _cons_(y)(merge(_cons_(x)(xs))(ys)))

# signature: ([Num] -> (Num -> ([Num] -> [Num])))
def merge2(ys):
  return lambda x: lambda xs: (_cons_(x)(xs) if _eq_(ys)([]) else merge3(x)(xs)(ys[0])(ys[1:]))

# signature: ([Num] -> ([Num] -> [Num]))
def merge(xs):
  return lambda ys: (ys if _eq_(xs)([]) else merge2(ys)(xs[0])(xs[1:]))

# signature: (Num -> (Num -> Num))
def avg(a):
  return lambda b: _mult_(0.5)(_plus_(a)(b))

# signature: ([Num] -> Num)
def mean(xs):
  return _div_(total(xs))(length(xs))

# signature: (Num -> ([Num] -> Num))
def _total(x):
  return lambda xs: _plus_(x)(total(xs))

# signature: ([Num] -> Num)
def total(xs):
  return (0.0 if _eq_(xs)([]) else _total(xs[0])(xs[1:]))

# signature: ((compose.b -> compose.c) -> ((compose.a -> compose.b) -> (compose.a -> compose.c)))
def compose(f):
  return lambda g: lambda x: f(g(x))

# signature: (Atom -> (Atom -> [Atom Atom]))
def rankPet(p1):
  return lambda p2: ((p1, p2) if _eq_(CHIPMUNK)(p1) else (p2, p1))

# signature: (Num -> (Num -> Atom))
def lt(x):
  return lambda y: _gt_(x)(y)

# signature: (Num -> (Num -> Atom))
def gte(x):
  return lambda y: _lte_(x)(y)

# signature: (Num -> ([Num] -> [Num]))
def qsortp(x):
  return lambda xs: concat(qsort(filter(lt(x))(xs)))(_cons_(x)(qsort(filter(gte(x))(xs))))

# signature: ([Num] -> [Num])
def qsort(xs):
  return (xs if _eq_(xs)([]) else qsortp(xs[0])(xs[1:]))

FALSE = 0;
TRUE = 1;
CHIPMUNK = 2;

empty = __empty()

pipes = [];
