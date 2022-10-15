// signature: (Num -> Num)
export function fib(n) {
  return (
    (n <= 1.0) ?
    1.0 :
    (fib((n - 1.0)) + fib((n - 2.0)))
  );
}

// signature: (Num -> Num)
export function neg(x) {
  return (0.0 - x);
}

const False = 0;
const True = 1;


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
