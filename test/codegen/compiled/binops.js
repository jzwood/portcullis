// signature: (Num -> (Num -> Num))
export function add(a) {
  return (b) => (a + b);
}

// signature: (Num -> (Num -> (Num -> Num)))
export function add3(a) {
  return (b) => (c) => ((a + b) + c);
}

// signature: (Num -> Num)
export function calc0(num) {
  return ((0.0 - (num * 9.0)) + (3.0 / 4.0));
}

// signature: (Num -> Num)
export function calc1(num) {
  return ((0.0 - (num * 9.0)) + (3.0 / 4.0));
}

// signature: (Num -> Atom)
export function choose0(num) {
  return (
    (num > 0.0) ?
    Cool :
    (
      equal(num, 0.0) ?
      Hmm :
      Bad
    )
  );
}

// signature: (Num -> Atom)
export function choose1(num) {
  return (
    (num <= 0.0) ?
    Cool :
    (
      (num < 100.0) ?
      Hmm :
      Bad
    )
  );
}

// signature: (Num -> Atom)
export function isEven(num) {
  return equal((num % 2.0), 0.0);
}

// signature: (append3.a -> ([append3.a] -> [append3.a]))
export function append3(x) {
  return (xs) => [x, ...[x, ...[x, ...xs]]];
}

const False = 0;
const True = 1;
const Cool = 2;
const Hmm = 3;
const Bad = 4;


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
