// signature: ((_map.a -> _map.b) -> (_map.a -> ([_map.a] -> [_map.b])))
export function _map(f) {
  return (x) => (xs) => [f(x), ...map(f)(xs)];
}

// signature: ((map.a -> map.b) -> ([map.a] -> [map.b]))
export function map(f) {
  return (xs) => (
    equal(xs, []) ?
    /* [b] */ [] :
    _map(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (Num -> (Num -> [Num]))
export function range(n0) {
  return (n1) => (
    (n0 > n1) ?
    /* [Num] */ [] :
    [n0, ...range((n0 + 1.0))(n1)]
  );
}

// signature: (Num -> Num)
export function _fizzbuzz(n) {
  return (
    equal(0.0, (n % 15.0)) ?
    (0.0 - 3.0) :
    (
      equal(0.0, (n % 5.0)) ?
      (0.0 - 2.0) :
      (
        equal(0.0, (n % 3.0)) ?
        (0.0 - 1.0) :
        n
      )
    )
  );
}

// signature: (Num -> [Num])
export function fizzbuzz(n) {
  return map(_fizzbuzz)(range(1.0)(n));
}

// signature: [Num]
function $fb() {
  return fizzbuzz(20.0);
}

const False = 0;
const True = 1;

export const fb = $fb()

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
