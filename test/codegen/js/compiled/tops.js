// signature: (Atom -> Num)
export function hello(what) {
  return (
    equal(what, World) ?
    42.0 :
    (
      equal(what, Ma) ?
      10.0 :
      (0.0 - 100.0)
    )
  );
}

// signature: (Num -> ([Num] -> Num))
export function _sum(x) {
  return (xs) => (x + sum(xs));
}

// signature: ([Num] -> Num)
export function sum(xs) {
  return (
    equal(xs, []) ?
    0.0 :
    _sum(xs.at(0))(xs.slice(1))
  );
}

const False = 0;
const True = 1;
const World = 2;
const Ma = 3;


export const pipes = [];
// INTERNAL
function equal(a, b) {
  if (a === b) {
    return +true;
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length === 0 && b.length === 0) return +true;
    if (a.length !== b.length) return +false;
    return equal(a.at(0), b.at(0)) && equal(a.slice(1), b.slice(1));
  }
  return +false;
}

// for testing
export const _equal = equal;
