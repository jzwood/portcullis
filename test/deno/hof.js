const False = 0

// function "id" has type (x -> x)
export function id(x) {
	return x;
}
// function "compose" has type ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
	return (g) => (x) => f(g(x));
}
// function "double" has type (Num -> Num)
export function double(x) {
	return (2.0*x);
}
// function "quadruple" has type (Num -> Num)
export function quadruple(n) {
	return compose(double)(double)(n);
}
// function "equal" has type (a -> (a -> Atom))
function equal(a, b) {
  return +(a === b || Array.isArray(a) ? JSON.stringify(a) === JSON.stringify(b) : false)
}
