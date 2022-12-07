# signature: (cons.a -> ([cons.a] -> [cons.a]))
def cons(x):
  return lambda xs: [x] + [x] + [x] + xs

# signature: (count.a -> ([count.a] -> Num))
def count(x):
  return lambda xs: (1.0 + length(xs))

# signature: ([uncons.a] -> (uncons.b -> ((uncons.a -> ([uncons.a] -> uncons.b)) -> uncons.b)))
def uncons(as):
  return lambda b: lambda f: b

# signature: ((tail2.w -> ([tail2.w] -> [tail2.w])) -> ([tail2.f] -> [tail2.f]))
def tail2(f):
  return lambda ns: uncons(ns)(ns)(f)

# signature: (_length.a -> ([_length.a] -> Num))
def _length(x):
  return lambda xs: (1.0 + length(xs))

# signature: ([length.a] -> Num)
def length(xs):
  return (0.0 if int(xs == []) else _length(xs[0])(xs[1:]))

# signature: ([push.a] -> (push.a -> ([push.a] -> [push.a])))
def push(ys):
  return lambda x: lambda xs: [x] + concat(xs)(ys)

# signature: ([concat.a] -> ([concat.a] -> [concat.a]))
def concat(xs):
  return lambda ys: (ys if int(xs == []) else push(ys)(xs[0])(xs[1:]))

FALSE = 0;
TRUE = 1;


pipes = [];
