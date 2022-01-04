const False = 0

// function "compose" has type ((a -> b) -> (a -> b))
function compose(f, g, a) {
	return f(g(a));
}
// function "equal" has type (a -> (a -> Atom))
function equal(a, b) {
  return a === b || Array.isArray(a) ? JSON.stringify(a) === JSON.stringify(b) : false
}
