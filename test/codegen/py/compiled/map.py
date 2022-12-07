# signature: ((_map.q -> _map.t) -> (_map.q -> ([_map.q] -> [_map.t])))
def _map(f):
  return lambda x: lambda xs: [f(x)] + map(f)(xs)

# signature: ((map.t -> map.g) -> ([map.t] -> [map.g]))
def map(f):
  return lambda xs: ([] if int(xs == []) else _map(f)(xs[0])(xs[1:]))

# signature: (Num -> Num)
def add1(x):
  return (x + 1.0)

# signature: [Num]
def __mapped():
  return map(add1)([1.0, 2.0, 3.0, 4.0])

# signature: ((_foldr.j -> (_foldr.k -> _foldr.k)) -> (_foldr.k -> (_foldr.j -> ([_foldr.j] -> _foldr.k))))
def _foldr(alg):
  return lambda acc: lambda x: lambda xs: foldr(alg)(alg(x)(acc))(xs)

# signature: ((foldr.a -> (foldr.b -> foldr.b)) -> (foldr.b -> ([foldr.a] -> foldr.b)))
def foldr(alg):
  return lambda acc: lambda xs: (acc if int(xs == []) else _foldr(alg)(acc)(xs[0])(xs[1:]))

# signature: ((_foldl.j -> (_foldl.k -> _foldl.k)) -> (_foldl.k -> (_foldl.j -> ([_foldl.j] -> _foldl.k))))
def _foldl(alg):
  return lambda acc: lambda x: lambda xs: alg(x)(foldl(alg)(acc)(xs))

# signature: ((foldl.a -> (foldl.b -> foldl.b)) -> (foldl.b -> ([foldl.a] -> foldl.b)))
def foldl(alg):
  return lambda acc: lambda xs: (acc if int(xs == []) else _foldl(alg)(acc)(xs[0])(xs[1:]))

# signature: (Num -> (Num -> Num))
def add(a):
  return lambda b: (a + b)

# signature: ([Num] -> Num)
def sum(ns):
  return foldr(add)(0.0)(ns)

# signature: Num
def __total():
  return sum([1.0, 2.0, 3.0])

# signature: (Num -> ([Num] -> [Num]))
def double(n):
  return lambda ns: [n] + [n] + ns

# signature: ([Num] -> [Num])
def mapDouble(ns):
  return foldl(double)([])(ns)

# signature: [Num]
def __dub():
  return mapDouble([1.0, 3.0, 5.0, 7.0])

# signature: (push.z -> ([push.z] -> [push.z]))
def push(x):
  return lambda xs: [x] + xs

# signature: ([reverse.a] -> [reverse.a])
def reverse(xs):
  return foldr(push)([])(xs)

# signature: (Num -> [Num])
def range(n):
  return ([] if (n <= 0.0) else [n] + range((n - 1.0)))

FALSE = 0;
TRUE = 1;

mapped = __mapped()
total = __total()
dub = __dub()

pipes = [];
