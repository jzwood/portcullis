const False = 0
const True = 1


// function "cons" has type (a -> ([a] -> [a]))
export function cons(x) {
  return (xs) => [x, ...[x, ...[x, ...xs]]];
}
// function "count" has type (a -> ([a] -> Num))
export function count(x) {
  return (xs) => (1.0 + length(xs));
}
// function "length" has type ([a] -> Num)
export function length(xs) {
  return (
    /* if */ equal(xs, /* [] */ []) ?
    /* then */ 0.0 :
    /* else */ count(xs.at(0))(xs.slice(1))
  );
}
// function "equal" has type (a -> (a -> Atom))
export function equal(a, b) {
  if (a === b) {
    return +true;
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length === 0 && b.length === 0) return +true;
    if (a.length !== b.length) return +false;
    const [aHead, ...aTail] = a
    const [bHead, ...bTail] = b
    return equal(aHead, bHead) && equal(aTail, bTail)
  }
  return +false;
}
