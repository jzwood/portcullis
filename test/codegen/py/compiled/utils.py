# signature: ((not.a -> (not.a -> Atom)) -> (not.a -> (not.a -> Atom)))
def not(f):
  return lambda a: lambda b: (True if (False == f(a)(b)) else False)

FALSE = 0;
TRUE = 1;


pipes = [];
