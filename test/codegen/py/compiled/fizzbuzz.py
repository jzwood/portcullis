# signature: ((_map.a -> _map.b) -> (_map.a -> ([_map.a] -> [_map.b])))
def _map(f):
  return lambda x: lambda xs: [f(x)] + map(f)(xs)

# signature: ((map.a -> map.b) -> ([map.a] -> [map.b]))
def map(f):
  return lambda xs: [] if (xs == []) else _map(f)(xs[0])(xs[1:])

# signature: (Num -> (Num -> [Num]))
def range(n0):
  return lambda n1: [] if (n0 > n1) else [n0] + range((n0 + 1.0))(n1)

# signature: (Num -> Num)
def _fizzbuzz(n):
  return (0.0 - 3.0) if (0.0 == (n % 15.0)) else (0.0 - 2.0) if (0.0 == (n % 5.0)) else (0.0 - 1.0) if (0.0 == (n % 3.0)) else n

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
