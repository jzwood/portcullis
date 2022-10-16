# signature: (id.x -> id.x)
def id(x):
  return x

# signature: ((compose.b -> compose.c) -> ((compose.a -> compose.b) -> (compose.a -> compose.c)))
def compose(f):
  return lambda g: lambda x: f(g(x))

# signature: (Num -> Num)
def double(x):
  return (2.0 * x)

# signature: (Num -> Num)
def quadruple(n):
  return compose(double)(double)(n)

# signature: (id2.x -> id2.x)
def id2(x):
  return compose(id)(id)(x)

# signature: Num
def __one1():
  return 1.0

# signature: Num
def __one2():
  return compose(id)(id)(one1)

# signature: (id3.z -> ([id3.z] -> [id3.z]))
def id3(x):
  return lambda xs: xs

# signature: ([tail.t] -> [tail.t])
def tail(xs):
  return xs if (xs == []) else id3(xs[0])(xs[1:])

# signature: ((a.h -> a.h) -> (a.h -> a.h))
def a(fx):
  return lambda x: fx(x)

# signature: (b.q -> b.q)
def b(w):
  return w

# signature: (c.p -> c.p)
def c(y):
  return a(b)(y)

# signature: ([push.h] -> (push.h -> ([push.h] -> [push.h])))
def push(ys):
  return lambda x: lambda xs: [x] + concat(xs)(ys)

# signature: ([concat.a] -> ([concat.a] -> [concat.a]))
def concat(xs):
  return lambda ys: ys if (xs == []) else push(ys)(xs[0])(xs[1:])

# signature: ((filter2.x -> Atom) -> (filter2.x -> ([filter2.x] -> [filter2.x])))
def filter2(g):
  return lambda w: lambda ws: concat([w] if g(w) else [])(filter(g)(ws))

# signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
def filter(f):
  return lambda xs: xs if (xs == []) else filter2(f)(xs[0])(xs[1:])

# signature: (eq.a -> (eq.a -> Atom))
def eq(x):
  return lambda y: (x == y)

# signature: ([Num] -> [Num])
def seven(xs):
  return filter(eq(7.0))(xs)

FALSE = 0;
TRUE = 1;

one1 = __one1()
one2 = __one2()

pipes = [];
