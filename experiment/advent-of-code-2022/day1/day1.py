# signature: (Num -> Num)
def asciiDecToChar(dec):
  return (0.0 if (dec == 48.0) else (1.0 if (dec == 49.0) else (2.0 if (dec == 50.0) else (3.0 if (dec == 51.0) else (4.0 if (dec == 52.0) else (5.0 if (dec == 53.0) else (6.0 if (dec == 54.0) else (7.0 if (dec == 55.0) else (8.0 if (dec == 56.0) else (9.0 if (dec == 57.0) else 0.0))))))))))

# signature: (Num -> ([Num] -> [Num]))
def _read(dec):
  return lambda nums: [(exp(10.0)(length(nums)) * asciiDecToChar(dec))] + nums

# signature: ([Num] -> Num)
def read(decimals):
  return sum(foldl(_read)([])(decimals))

# signature: (Num -> (Num -> Num))
def exp(x):
  return lambda n: (exp((1.0 / x))((0.0 - 1.0)) if (n < 0.0) else (1.0 if (n == 0.0) else (exp((x * x))((n / 2.0)) if ((n % 2.0) == 0.0) else (x * exp((x * x))(((n - 1.0) / 2.0))))))

# signature: ((_foldl.j -> (_foldl.k -> _foldl.k)) -> (_foldl.k -> (_foldl.j -> ([_foldl.j] -> _foldl.k))))
def _foldl(alg):
  return lambda acc: lambda x: lambda xs: alg(x)(foldl(alg)(acc)(xs))

# signature: ((foldl.a -> (foldl.b -> foldl.b)) -> (foldl.b -> ([foldl.a] -> foldl.b)))
def foldl(alg):
  return lambda acc: lambda xs: (acc if (xs == []) else _foldl(alg)(acc)(xs[0])(xs[1:]))

# signature: ((_map.q -> _map.t) -> (_map.q -> ([_map.q] -> [_map.t])))
def _map(f):
  return lambda x: lambda xs: [f(x)] + map(f)(xs)

# signature: ((map.t -> map.g) -> ([map.t] -> [map.g]))
def map(f):
  return lambda xs: ([] if (xs == []) else _map(f)(xs[0])(xs[1:]))

# signature: (_len.a -> (Num -> Num))
def _len(x):
  return lambda len: (1.0 + len)

# signature: ([length.a] -> Num)
def length(xs):
  return foldl(_len)(0.0)(xs)

# signature: (Num -> (Num -> Num))
def add(a):
  return lambda b: (a + b)

# signature: ([Num] -> Num)
def sum(ns):
  return foldl(add)(0.0)(ns)

# signature: (_not.x -> (_not.x -> Atom))
def _not(a):
  return lambda b: (False == (a == b))

# signature: (Num -> (Num -> Num))
def _max(num):
  return lambda max: (num if (num > max) else max)

# signature: ([Num] -> Num)
def max(nums):
  return foldl(_max)(0.0)(nums)

# signature: ([[curryCons.a] [[curryCons.a]]] -> [[curryCons.a]])
def curryCons(tup):
  return [tup[0]] + tup[1]

# signature: (_splitOn.z -> (_splitOn.z -> ([[_splitOn.z] [[_splitOn.z]]] -> [[_splitOn.z] [[_splitOn.z]]])))
def _splitOn(on):
  return lambda x: lambda acc: (([x] + acc[0], acc[1]) if _not(x)(on) else ([], curryCons(acc)))

# signature: (splitOn.a -> ([splitOn.a] -> [[splitOn.a]]))
def splitOn(on):
  return lambda xs: curryCons(foldl(_splitOn(on))(([], []))(xs))

# signature: ([Num] -> Num)
def day1a(buffer):
  return max(map(sum)(splitOn(0.0)(map(read)(splitOn(10.0)(buffer)))))

FALSE = 0;
TRUE = 1;


pipes = [];
