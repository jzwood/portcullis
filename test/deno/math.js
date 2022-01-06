const False = 0

// function "avg" has type (Num -> (Num -> Num))
export function avg(a) {
	return (b) => (0.5*(a+b));
}
// function "equal" has type (a -> (a -> Atom))
function equal(a, b) {
  return a === b || Array.isArray(a) ? JSON.stringify(a) === JSON.stringify(b) : false
}
