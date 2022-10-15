// signature: ((not.a -> (not.a -> Atom)) -> (not.a -> (not.a -> Atom)))
export function not(f) {
  return (a) => (b) => (
    equal(False, f(a)(b)) ?
    True :
    False
  );
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
