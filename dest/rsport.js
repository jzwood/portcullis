// function "equal" has type (a -> (a -> Atom))
function equal(a, b) {
  return a === b || Array.isArray(a) ? JSON.stringify(a) === JSON.stringify(b) : false
}
// function "neg" has type (Num -> Num)
function neg(x) {
	return (0.0-x);
}
// function "tail" has type ([a] -> [a])
function tail(xs) {
	return Array.prototype.slice.call(xs, 1.0, length(xs));
}
// function "empty" has type [Num]
function empty() {
	return /* [Num] */ [];
}
// function "length" has type ([a] -> Num)
function length(xs) {
	return (() => {
		if (equal(/* [Char] */ [], xs)) {
			return 0.0;
		} 
		if (T) {
			return (1.0+length(Array.prototype.slice.call(xs, 0.0, neg(1.0))));
		}
	})();
}
// function "sort" has type ([Num] -> [Num])
function sort(ns) {
	return sort2(length(ns), ns);
}
// function "sort2" has type (Num -> ([Num] -> [Num]))
function sort2(len, ns) {
	return (() => {
		if ((len<=1.0)) {
			return ns;
		} 
		if (T) {
			return merge(sort(Array.prototype.slice.call(ns, 0.0, (len/2.0))), sort(Array.prototype.slice.call(ns, (len/2.0), len)));
		}
	})();
}
// function "order" has type (Num -> (Num -> [Num]))
function order(x, y) {
	return (() => {
		if ((x<y)) {
			return /* [Num] */ [x,y];
		} 
		if (T) {
			return /* [Num] */ [y,x];
		}
	})();
}
// function "merge" has type ([Num] -> ([Num] -> [Num]))
function merge(xs, ys) {
	return (() => {
		if (equal(xs, empty())) {
			return ys;
		} 
		if (equal(ys, empty())) {
			return xs;
		} 
		if (T) {
			return Array.prototype.concat.call(order(Array.prototype.at.call(xs, 0.0) ?? 0.0, Array.prototype.at.call(ys, 0.0) ?? 0.0), merge(tail(xs), tail(ys)));
		}
	})();
}
