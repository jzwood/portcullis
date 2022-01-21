const False = 0
const Chipmunk = 1

export const empty = $empty()

// function "neg" has type (Num -> Num)
export function neg(x) {
	return (0.0-x);
}
// function "tail" has type ([a] -> [a])
export function tail(xs) {
	return Array.prototype.slice.call(xs, 1.0, ((arr) => arr.length)(xs));
}
// function "empty" has type [Num]
export function $empty() {
	return /* [Num] */ [];
}
// function "sort" has type ([Num] -> [Num])
export function sort(ns) {
	return sort2(((arr) => arr.length)(ns))(ns);
}
// function "sort2" has type (Num -> ([Num] -> [Num]))
export function sort2(len) {
	return (ns) => (() => {
		if ((len<=1.0)) {
			return ns;
		}
		return merge(sort(Array.prototype.slice.call(ns, 0.0, (len/2.0))))(sort(Array.prototype.slice.call(ns, (len/2.0), len)));
	})();
}
// function "cmpH" has type ([Num] -> ([Num] -> [[Num] [Num]]))
export function cmpH(xs) {
	return (ys) => (() => {
		if (((Array.prototype.at.call(xs, 0.0) ?? 0.0)<(Array.prototype.at.call(ys, 0.0) ?? 0.0))) {
			return [xs,ys];
		}
		return [ys,xs];
	})();
}
// function "merge" has type ([Num] -> ([Num] -> [Num]))
export function merge(xs) {
	return (ys) => (() => {
		if (equal(xs, empty)) {
			return ys;
		}
		if (equal(ys, empty)) {
			return xs;
		}
		return merge2(cmpH(xs)(ys));
	})();
}
// function "merge2" has type ([[Num] [Num]] -> [Num])
export function merge2(xys) {
	return Array.prototype.concat.call(Array.prototype.slice.call((([a,]) => a)(xys), 0.0, 1.0), merge(tail((([a,]) => a)(xys)))((([,b]) => b)(xys)));
}
// function "avg" has type (Num -> (Num -> Num))
export function avg(a) {
	return (b) => (0.5*(a+b));
}
// function "mean" has type ([Num] -> Num)
export function mean(xs) {
	return (sum(xs)/((arr) => arr.length)(xs));
}
// function "sum" has type ([Num] -> Num)
export function sum(xs) {
	return (() => {
		if (equal(0.0, ((arr) => arr.length)(xs))) {
			return 0.0;
		}
		return ((Array.prototype.at.call(xs, 0.0) ?? 0.0)+sum(tail(xs)));
	})();
}
// function "compose" has type ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
	return (g) => (x) => f(g(x));
}
// function "rankPet" has type (Atom -> (Atom -> [Atom Atom]))
export function rankPet(p1) {
	return (p2) => (() => {
		if (equal(Chipmunk, p1)) {
			return [p1,p2];
		}
		return [p2,p1];
	})();
}
// function "equal" has type (a -> (a -> Atom))
function equal(a, b) {
  return +(a === b || Array.isArray(a) ? JSON.stringify(a) === JSON.stringify(b) : false)
}
