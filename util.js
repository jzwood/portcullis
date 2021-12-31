// function "equal" has type (a -> (a -> Atom))
function equal(a, b) {
	return a === b || Array.isArray(a) ? JSON.stringify(a) === JSON.stringify(b) : false
}
// function "filter" has type (a -> ([a] -> [a]))
function filter(x, xs) {
	return (() => {
		if (equal(xs, /* [Num] */ [])) {
			return /* [Num] */ [];
		}
		if (equal(/* [Num] */ [x], head(xs))) {
			return tail(xs);
		}
		if (T) {
			return Array.prototype.concat.call(head(xs), filter(x, tail(xs)));
		}
	})();
}
// function "not" has type ((a -> (a -> Atom)) -> (a -> (a -> Atom)))
function not(f, a, b) {
	return (() => {
		if (equal(False, f(a, b))) {
			return True;
		}
		if (T) {
			return False;
		}
	})();
}
