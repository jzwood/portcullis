const False = 0
const True = 1
const Chipmunk = 2

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
function $empty() {
	return /* [Num] */ [];
}
// function "msort" has type ([Num] -> [Num])
export function msort(ns) {
	return msort2(((arr) => arr.length)(ns))(ns);
}
// function "msort2" has type (Num -> ([Num] -> [Num]))
export function msort2(len) {
	return (ns) => ((pred, ifB, elseB) => pred ? ifB() : elseB())(
		/* if */ (len<=1.0),
		/* then */ () => ns,
		/* else */ () => merge(msort(Array.prototype.slice.call(ns, 0.0, (len/2.0))))(msort(Array.prototype.slice.call(ns, (len/2.0), len)))
	);
}
// function "cmpHead" has type ([Num] -> ([Num] -> [[Num] [Num]]))
export function cmpHead(xs) {
	return (ys) => ((pred, ifB, elseB) => pred ? ifB() : elseB())(
		/* if */ ((Array.prototype.at.call(xs, 0.0) ?? 0.0)<(Array.prototype.at.call(ys, 0.0) ?? 0.0)),
		/* then */ () => [xs,ys],
		/* else */ () => [ys,xs]
	);
}
// function "merge" has type ([Num] -> ([Num] -> [Num]))
export function merge(xs) {
	return (ys) => ((pred, ifB, elseB) => pred ? ifB() : elseB())(
		/* if */ equal(0.0, ((arr) => arr.length)(xs)),
		/* then */ () => ys,
		/* else */ () => ((pred, ifB, elseB) => pred ? ifB() : elseB())(
			/* if */ equal(0.0, ((arr) => arr.length)(ys)),
			/* then */ () => xs,
			/* else */ () => merge2(cmpHead(xs)(ys))
		)
	);
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
	return ((pred, ifB, elseB) => pred ? ifB() : elseB())(
		/* if */ equal(0.0, ((arr) => arr.length)(xs)),
		/* then */ () => 0.0,
		/* else */ () => ((Array.prototype.at.call(xs, 0.0) ?? 0.0)+sum(tail(xs)))
	);
}
// function "compose" has type ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
	return (g) => (x) => f(g(x));
}
// function "rankPet" has type (Atom -> (Atom -> [Atom Atom]))
export function rankPet(p1) {
	return (p2) => ((pred, ifB, elseB) => pred ? ifB() : elseB())(
		/* if */ equal(Chipmunk, p1),
		/* then */ () => [p1,p2],
		/* else */ () => [p2,p1]
	);
}
// function "equal" has type (a -> (a -> Atom))
function equal(a, b) {
  return +(a === b || Array.isArray(a) ? JSON.stringify(a) === JSON.stringify(b) : false)
}
