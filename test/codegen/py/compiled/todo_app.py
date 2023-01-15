# PORTCULLIS INTERNAL
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

# USER CODE

# signature: ([Atom [Char]] -> ([[Char]] -> [[Char]]))
def update(tup):
  return lambda todos: (_cons_(tup[1])(todos) if _eq_(APPEND)(tup[0]) else (remove(tup[1])(todos) if _eq_(DONE)(tup[0]) else todos))

# signature: ([Char] -> [Atom [Char]])
def append(todo):
  return (APPEND, todo)

# signature: ([Char] -> [Atom [Char]])
def done(done):
  return (DONE, done)

# signature: ([push.a] -> (push.a -> ([push.a] -> [push.a])))
def push(ys):
  return lambda x: lambda xs: _cons_(x)(concat(xs)(ys))

# signature: ([concat.a] -> ([concat.a] -> [concat.a]))
def concat(xs):
  return lambda ys: (ys if _eq_(xs)([]) else push(ys)(xs[0])(xs[1:]))

# signature: ((_filter.x -> Atom) -> (_filter.x -> ([_filter.x] -> [_filter.x])))
def _filter(f):
  return lambda x: lambda xs: concat(([x] if f(x) else []))(filter(f)(xs))

# signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
def filter(f):
  return lambda xs: (xs if _eq_(xs)([]) else _filter(f)(xs[0])(xs[1:]))

# signature: (neq.a -> (neq.b -> Atom))
def neq(a):
  return lambda b: _eq_(FALSE)(_eq_(a)(b))

# signature: ([Char] -> ([[Char]] -> [[Char]]))
def remove(todo):
  return lambda todos: filter(neq(todo))(todos)

FALSE = 0;
TRUE = 1;
APPEND = 2;
DONE = 3;


pipes = [
  (update, [("&update", 100), ("&todo", 1)], "&todo"),
  (append, [("&append", 50)], "&update"),
  (done, [("&done", 50)], "&update")
]
