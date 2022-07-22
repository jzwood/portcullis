const False = 0
const True = 1

export const one1 = $one1()
export const one2 = $one2()
export const one3 = $one3()

// signature: ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}

// signature: (x -> x)
export function id1(x) {
  return x;
}

// signature: (Num -> Num)
export function id2(x) {
  return compose(id1)(id1)(x);
}

// signature: Num
function $one1() {
  return 1.0;
}

// signature: Num
function $one2() {
  return one1;
}

// signature: Num
function $one3() {
  return one1;
}

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
