const False = 0
const True = 1


// function "id2" has type (w -> ([w] -> [w]))
export function id2(x) {
  return (xs) => xs;
}
// function "tail" has type ([f] -> [f])
export function tail(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ id2(xs.at(0))(xs.slice(1))
  );
}
// function "neg" has type (Num -> Num)
export function neg(x) {
  return (0.0 - x);
}
// function "double" has type (Num -> Num)
export function double(x) {
  return (2.0 * x);
}
// function "add3" has type (Num -> (Num -> (Num -> Num)))
export function add3(a) {
  return (b) => (c) => ((a + b) + c);
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
