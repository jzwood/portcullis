// signature: (Num -> (Num -> Num))
export function add(a) {
  return (b) => (a + b);
}

// signature: (Num -> (Num -> Num))
export function sub(a) {
  return (b) => (a - b);
}

const False = 0;
const True = 1;


export const pipes = [
  [add, [["&counter", 1], ["&add", 100]], "&counter"],
  [sub, [["&counter", 1], ["&sub", 100]], "&counter"]
]
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
