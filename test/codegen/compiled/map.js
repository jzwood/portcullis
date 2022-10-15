// signature: ((_map.q -> _map.t) -> (_map.q -> ([_map.q] -> [_map.t])))
export function _map(f) {
  return (x) => (xs) => [f(x), ...map(f)(xs)];
}

// signature: ((map.t -> map.g) -> ([map.t] -> [map.g]))
export function map(f) {
  return (xs) => (
    equal(xs, []) ?
    /* [g] */ [] :
    _map(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (Num -> Num)
export function add1(x) {
  return (x + 1.0);
}

// signature: [Num]
function $mapped() {
  return map(add1)(/* [Num] */ [1.0, 2.0, 3.0, 4.0]);
}

// signature: ((_foldr.j -> (_foldr.k -> _foldr.k)) -> (_foldr.k -> (_foldr.j -> ([_foldr.j] -> _foldr.k))))
export function _foldr(alg) {
  return (acc) => (x) => (xs) => foldr(alg)(alg(x)(acc))(xs);
}

// signature: ((foldr.a -> (foldr.b -> foldr.b)) -> (foldr.b -> ([foldr.a] -> foldr.b)))
export function foldr(alg) {
  return (acc) => (xs) => (
    equal(xs, []) ?
    acc :
    _foldr(alg)(acc)(xs.at(0))(xs.slice(1))
  );
}

// signature: ((_foldl.j -> (_foldl.k -> _foldl.k)) -> (_foldl.k -> (_foldl.j -> ([_foldl.j] -> _foldl.k))))
export function _foldl(alg) {
  return (acc) => (x) => (xs) => alg(x)(foldl(alg)(acc)(xs));
}

// signature: ((foldl.a -> (foldl.b -> foldl.b)) -> (foldl.b -> ([foldl.a] -> foldl.b)))
export function foldl(alg) {
  return (acc) => (xs) => (
    equal(xs, []) ?
    acc :
    _foldl(alg)(acc)(xs.at(0))(xs.slice(1))
  );
}

// signature: (Num -> (Num -> Num))
export function add(a) {
  return (b) => (a + b);
}

// signature: ([Num] -> Num)
export function sum(ns) {
  return foldr(add)(0.0)(ns);
}

// signature: Num
function $total() {
  return sum(/* [Num] */ [1.0, 2.0, 3.0]);
}

// signature: (Num -> ([Num] -> [Num]))
export function double(n) {
  return (ns) => [n, ...[n, ...ns]];
}

// signature: ([Num] -> [Num])
export function mapDouble(ns) {
  return foldl(double)(/* [Num] */ [])(ns);
}

// signature: [Num]
function $dub() {
  return mapDouble(/* [Num] */ [1.0, 3.0, 5.0, 7.0]);
}

// signature: (push.z -> ([push.z] -> [push.z]))
export function push(x) {
  return (xs) => [x, ...xs];
}

// signature: ([reverse.a] -> [reverse.a])
export function reverse(xs) {
  return foldr(push)(/* [a] */ [])(xs);
}

// signature: (Num -> [Num])
export function range(n) {
  return (
    (n <= 0.0) ?
    /* [Num] */ [] :
    [n, ...range((n - 1.0))]
  );
}

const False = 0;
const True = 1;

export const mapped = $mapped()
export const total = $total()
export const dub = $dub()

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
