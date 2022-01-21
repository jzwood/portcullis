const False = 0

export const one1 = $one1()
export const one2 = $one2()

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
// function "id2" has type (x -> x)
export function id2(x) {
	return compose(id)(id)(x);
}
// function "one1" has type Num
export function $one1() {
	return 1.0;
}
// function "one2" has type Num
export function $one2() {
	return compose(id)(id)(one1);
}
// function "equal" has type (a -> (a -> Atom))
function equal(a, b) {
  return +(a === b || Array.isArray(a) ? JSON.stringify(a) === JSON.stringify(b) : false)
}
