// signature: (id.a -> id.a)
export function id(x) {
  return x;
}

// signature: (cmp.x -> (cmp.x -> Atom))
export function cmp(a) {
  return (b) => equal(a, b);
}

// signature: (Atom -> (Num -> Num))
export function incr(won) {
  return (score) => (score + (
    won ? 1.0 : (0.0 - 1.0)
  ));
}

const False = 0;
const True = 1;


export const pipes = [
  [id, [["&guess", 99]], "&newCoin"],
  [cmp, [["&guess", 99], ["&coin", 99]], "&win"],
  [incr, [["&win", 99], ["&score", 99]], "&score"]
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
