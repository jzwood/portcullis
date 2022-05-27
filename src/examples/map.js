const False = 0
const True = 1


// function "map" has type ((t -> g) -> (t -> g))
export function map(f) {
  return (a) => f(a);
}
// function "mplus" has type ((x -> y) -> (x -> y))
export function mplus(f) {
  return (p) => map(f)(p);
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
