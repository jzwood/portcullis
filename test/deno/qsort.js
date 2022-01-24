const False = 0
const True = 1


// function "filter" has type (([a] -> Atom) -> ([a] -> [a]))
export function filter(f) {
	return (xs) => ((pred, ifB, elseB) => pred ? ifB() : elseB())(equal(0.0, ((arr) => arr.length)(xs)), () => xs, () => ((pred, ifB, elseB) => pred ? ifB() : elseB())(f(xs), () => Array.prototype.concat.call(Array.prototype.slice.call(xs, 0.0, 1.0), filter(f)(tail(xs))), () => filter(f)(tail(xs))));
}
// function "tail" has type ([a] -> [a])
export function tail(xs) {
	return Array.prototype.slice.call(xs, 1.0, ((arr) => arr.length)(xs));
}
// function "lt" has type (Num -> ([Num] -> Atom))
export function lt(p) {
	return (xs) => ((Array.prototype.at.call(xs, 0.0) ?? 0.0)<p);
}
// function "eq" has type (Num -> ([Num] -> Atom))
export function eq(p) {
	return (xs) => equal((Array.prototype.at.call(xs, 0.0) ?? 0.0), p);
}
// function "gt" has type (Num -> ([Num] -> Atom))
export function gt(p) {
	return (xs) => ((Array.prototype.at.call(xs, 0.0) ?? 0.0)>p);
}
// function "qsort" has type ([Num] -> [Num])
export function qsort(xs) {
	return ((pred, ifB, elseB) => pred ? ifB() : elseB())((1.0>=((arr) => arr.length)(xs)), () => xs, () => qsortP((Array.prototype.at.call(xs, 0.0) ?? 0.0))(xs));
}
// function "qsortP" has type (Num -> ([Num] -> [Num]))
export function qsortP(pivot) {
	return (xs) => Array.prototype.concat.call(qsort(filter(lt(pivot))(xs)), Array.prototype.concat.call(filter(eq(pivot))(xs), qsort(filter(gt(pivot))(xs))));
}
// function "hofBad" has type ((a -> Atom) -> ([a] -> [a]))
export function hofBad(f) {
	return (xs) => ((pred, ifB, elseB) => pred ? ifB() : elseB())(f(xs), () => xs, () => xs);
}
// function "equal" has type (a -> (a -> Atom))
function equal(a, b) {
  return +(a === b || Array.isArray(a) ? JSON.stringify(a) === JSON.stringify(b) : false)
}
