// signature: (cons.a -> ([cons.a] -> [cons.a]))
export function cons(x) {
  return (xs) => [x, ...[x, ...[x, ...xs]]];
}

// signature: (count.a -> ([count.a] -> Num))
export function count(x) {
  return (xs) => (1.0 + length(xs));
}

// signature: ([uncons.a] -> (uncons.b -> ((uncons.a -> ([uncons.a] -> uncons.b)) -> uncons.b)))
export function uncons(as) {
  return (b) => (f) => b;
}

// signature: ((tail2.w -> ([tail2.w] -> [tail2.w])) -> ([tail2.f] -> [tail2.f]))
export function tail2(f) {
  return (ns) => uncons(ns)(ns)(f);
}

// signature: (_length.a -> ([_length.a] -> Num))
export function _length(x) {
  return (xs) => (1.0 + length(xs));
}

// signature: ([length.a] -> Num)
export function length(xs) {
  return (
    equal(xs, []) ?
    0.0 :
    _length(xs.at(0))(xs.slice(1))
  );
}

// signature: ([push.a] -> (push.a -> ([push.a] -> [push.a])))
export function push(ys) {
  return (x) => (xs) => [x, ...concat(xs)(ys)];
}

// signature: ([concat.a] -> ([concat.a] -> [concat.a]))
export function concat(xs) {
  return (ys) => (
    equal(xs, []) ?
    ys :
    push(ys)(xs.at(0))(xs.slice(1))
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
