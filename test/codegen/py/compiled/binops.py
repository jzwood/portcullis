# signature: (Num -> (Num -> Num))
def add(a):
  return lambda b: (a + b)

# signature: (Num -> (Num -> (Num -> Num)))
def add3(a):
  return lambda b: lambda c: ((a + b) + c)

# signature: (Num -> Num)
def calc0(num):
  return ((0.0 - (num * 9.0)) + (3.0 / 4.0))

# signature: (Num -> Num)
def calc1(num):
  return ((0.0 - (num * 9.0)) + (3.0 / 4.0))

# signature: (Num -> Atom)
def choose0(num):
  return Cool if (num > 0.0) else Hmm if (num == 0.0) else Bad

# signature: (Num -> Atom)
def choose1(num):
  return Cool if (num <= 0.0) else Hmm if (num < 100.0) else Bad

# signature: (Num -> Atom)
def isEven(num):
  return ((num % 2.0) == 0.0)

# signature: (append3.a -> ([append3.a] -> [append3.a]))
def append3(x):
  return lambda xs: [x] + [x] + [x] + xs

FALSE = 0;
TRUE = 1;
COOL = 2;
HMM = 3;
BAD = 4;


pipes = [];
