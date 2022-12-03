# signature: [[Char]]
def __list():
  return [['1', '2', '3'], ['4', '4']]

# signature: (Char -> (Num -> Num))
def charToNum(char):
  return lambda fallback: (0.0 if (char == '0') else (1.0 if (char == '1') else (2.0 if (char == '2') else (3.0 if (char == '3') else (4.0 if (char == '4') else (5.0 if (char == '5') else (6.0 if (char == '6') else (7.0 if (char == '7') else (8.0 if (char == '8') else (9.0 if (char == '9') else fallback))))))))))

# signature: (Char -> ([Num] -> [Num]))
def _parseInt(char):
  return lambda nums: [(exp(10.0)(length(nums)) * charToNum(char)(0.0))] + nums

# signature: ([Char] -> Num)
def parseInt(chars):
  return sum(foldr(_parseInt)([])(reverse(chars)))

# signature: (Num -> (Num -> Num))
def exp(x):
  return lambda n: (exp((1.0 / x))((0.0 - 1.0)) if (n < 0.0) else (1.0 if (n == 0.0) else (exp((x * x))((n / 2.0)) if ((n % 2.0) == 0.0) else (x * exp((x * x))(((n - 1.0) / 2.0))))))

# signature: ((_foldr.j -> (_foldr.k -> _foldr.k)) -> (_foldr.k -> (_foldr.j -> ([_foldr.j] -> _foldr.k))))
def _foldr(alg):
  return lambda acc: lambda x: lambda xs: foldr(alg)(alg(x)(acc))(xs)

# signature: ((foldr.a -> (foldr.b -> foldr.b)) -> (foldr.b -> ([foldr.a] -> foldr.b)))
def foldr(alg):
  return lambda acc: lambda xs: (acc if (xs == []) else _foldr(alg)(acc)(xs[0])(xs[1:]))

# signature: (_len.a -> (Num -> Num))
def _len(x):
  return lambda len: (1.0 + len)

# signature: ([length.a] -> Num)
def length(xs):
  return foldr(_len)(0.0)(xs)

# signature: (_rev.a -> ([_rev.a] -> [_rev.a]))
def _rev(x):
  return lambda xs: [x] + xs

# signature: ([reverse.a] -> [reverse.a])
def reverse(xs):
  return foldr(_rev)([])(xs)

# signature: (Num -> (Num -> Num))
def add(a):
  return lambda b: (a + b)

# signature: ([Num] -> Num)
def sum(ns):
  return foldr(add)(0.0)(ns)

FALSE = 0;
TRUE = 1;

list = __list()

pipes = [];
