const False = 0
const True = 1


// function "hm2" has type ((w -> w) -> (w -> w))
export function hm2(f) {
  return (z) => f(z);
}
// function "test2" has type ((z -> z) -> Atom)
export function test2(f) {
  return hm2(f)(Jake);
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
