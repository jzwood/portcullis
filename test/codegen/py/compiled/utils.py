# signature: ((not.a -> (not.a -> Atom)) -> (not.a -> (not.a -> Atom)))
def not(f):
  return lambda a: lambda b: (TRUE if int(FALSE == f(a)(b)) else FALSE)

FALSE = 0;
TRUE = 1;


pipes = [];
