const False = 0
const True = 1

export const mapped = $mapped()

// function "_map" has type ((q -> t) -> (q -> ([q] -> [t])))
export function _map(f) {
  return (x) => (xs) => [f(x), ...map(f)(xs)];
}
// function "map" has type ((t -> g) -> ([t] -> [g]))
export function map(f) {
  return (xs) => (
    /* if */ equal(xs, []) ?
    /* then */ /* [g] */ [] :
    /* else */ _map(f)(xs.at(0))(xs.slice(1))
  );
}
// function "add1" has type (Num -> Num)
export function add1(x) {
  return (x + 1.0);
}
// function "mapped" has type [Num]
function $mapped() {
  return map(add1)(/* [Num] */ [1.0,2.0,3.0,4.0]);
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
