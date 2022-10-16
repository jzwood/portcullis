# signature: ((not.a -> (not.a -> Atom)) -> (not.a -> (not.a -> Atom)))
def not(f):
  return lambda a: lambda b: True if (False == f(a)(b)) else False

# signature: (_length.a -> ([_length.a] -> Num))
def _length(x):
  return lambda xs: (1.0 + length(xs))

# signature: ([length.a] -> Num)
def length(xs):
  return 0.0 if (xs == []) else _length(xs[0])(xs[1:])

# signature: ([push.h] -> (push.h -> ([push.h] -> [push.h])))
def push(ys):
  return lambda x: lambda xs: [x] + concat(xs)(ys)

# signature: ([concat.a] -> ([concat.a] -> [concat.a]))
def concat(xs):
  return lambda ys: ys if (xs == []) else push(ys)(xs[0])(xs[1:])

# signature: (identity2.x -> ([identity2.x] -> [identity2.x]))
def identity2(x):
  return lambda xs: xs

# signature: ([tail.p] -> [tail.p])
def tail(xs):
  return xs if (xs == []) else identity2(xs[0])(xs[1:])

# signature: ([drop.g] -> (Num -> [drop.g]))
def drop(xs):
  return lambda n: xs if (n <= 0.0) else drop(tail(xs))((n - 1.0))

# signature: (Num -> (_take.f -> ([_take.f] -> [_take.f])))
def _take(n):
  return lambda x: lambda xs: [] if (n <= 0.0) else [x] + take(xs)((n - 1.0))

# signature: ([take.k] -> (Num -> [take.k]))
def take(xs):
  return lambda n: [] if (xs == []) else _take(n)(xs[0])(xs[1:])

# signature: ([slice.q] -> (Num -> (Num -> [slice.q])))
def slice(xs):
  return lambda i: lambda j: take(drop(xs)(i))((j - i))

# signature: ((filter2.x -> Atom) -> (filter2.x -> ([filter2.x] -> [filter2.x])))
def filter2(g):
  return lambda w: lambda ws: concat([w] if g(w) else [])(filter(g)(ws))

# signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
def filter(f):
  return lambda xs: xs if (xs == []) else filter2(f)(xs[0])(xs[1:])

# signature: (Num -> Num)
def neg(x):
  return (0.0 - x)

# signature: [Num]
def __empty():
  return []

# signature: ([Num] -> [Num])
def msort(ns):
  return msort2(length(ns))(ns)

# signature: (Num -> ([Num] -> [Num]))
def msort2(len):
  return lambda ns: ns if (len <= 1.0) else merge(msort(take(ns)((len / 2.0))))(msort(drop(ns)((len / 2.0))))

# signature: (Num -> ([Num] -> (Num -> ([Num] -> [Num]))))
def merge3(x):
  return lambda xs: lambda y: lambda ys: [x] + merge(xs)([y] + ys) if (x <= y) else [y] + merge([x] + xs)(ys)

# signature: ([Num] -> (Num -> ([Num] -> [Num])))
def merge2(ys):
  return lambda x: lambda xs: [x] + xs if (ys == []) else merge3(x)(xs)(ys[0])(ys[1:])

# signature: ([Num] -> ([Num] -> [Num]))
def merge(xs):
  return lambda ys: ys if (xs == []) else merge2(ys)(xs[0])(xs[1:])

# signature: (Num -> (Num -> Num))
def avg(a):
  return lambda b: (0.5 * (a + b))

# signature: ([Num] -> Num)
def mean(xs):
  return (sum(xs) / length(xs))

# signature: (Num -> ([Num] -> Num))
def sum2(x):
  return lambda xs: (x + sum(xs))

# signature: ([Num] -> Num)
def sum(xs):
  return 0.0 if (xs == []) else sum2(xs[0])(xs[1:])

# signature: ((compose.b -> compose.c) -> ((compose.a -> compose.b) -> (compose.a -> compose.c)))
def compose(f):
  return lambda g: lambda x: f(g(x))

# signature: (Atom -> (Atom -> [Atom Atom]))
def rankPet(p1):
  return lambda p2: (p1, p2) if (Chipmunk == p1) else (p2, p1)

# signature: (Num -> (Num -> Atom))
def lt(x):
  return lambda y: (x > y)

# signature: (Num -> (Num -> Atom))
def gte(x):
  return lambda y: (x <= y)

# signature: (Num -> ([Num] -> [Num]))
def qsortp(x):
  return lambda xs: concat(qsort(filter(lt(x))(xs)))([x] + qsort(filter(gte(x))(xs)))

# signature: ([Num] -> [Num])
def qsort(xs):
  return xs if (xs == []) else qsortp(xs[0])(xs[1:])

FALSE = 0;
TRUE = 1;
CHIPMUNK = 2;

empty = __empty()

pipes = [];
