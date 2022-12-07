# signature: ([Atom [Char]] -> ([[Char]] -> [[Char]]))
def update(tup):
  return lambda todos: ([tup[1]] + todos if (Append == tup[0]) else (remove(tup[1])(todos) if (Done == tup[0]) else todos))

# signature: ([Char] -> [Atom [Char]])
def append(todo):
  return (Append, todo)

# signature: ([Char] -> [Atom [Char]])
def done(done):
  return (Done, done)

# signature: ([push.a] -> (push.a -> ([push.a] -> [push.a])))
def push(ys):
  return lambda x: lambda xs: [x] + concat(xs)(ys)

# signature: ([concat.a] -> ([concat.a] -> [concat.a]))
def concat(xs):
  return lambda ys: (ys if (xs == []) else push(ys)(xs[0])(xs[1:]))

# signature: ((_filter.x -> Atom) -> (_filter.x -> ([_filter.x] -> [_filter.x])))
def _filter(f):
  return lambda x: lambda xs: concat(([x] if f(x) else []))(filter(f)(xs))

# signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
def filter(f):
  return lambda xs: (xs if (xs == []) else _filter(f)(xs[0])(xs[1:]))

# signature: (neq.a -> (neq.b -> Atom))
def neq(a):
  return lambda b: (False == (a == b))

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
