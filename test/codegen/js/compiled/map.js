// PORTCULLIS INTERNAL
function _plus_(a) {
  return (b) => a + b;
}

function _minus_(a) {
  return (b) => a - b;
}

function _mult_(a) {
  return (b) => a * b;
}

function _div_(a) {
  return (b) => a / b;
}

function _rem_(a) {
  return (b) => a % b;
}

function _gt_(a) {
  return (b) => a > b ? 1 : 0;
}

function _gte_(a) {
  return (b) => a >= b ? 1 : 0;
}

function _lt_(a) {
  return (b) => a < b ? 1 : 0;
}

function _lte_(a) {
  return (b) => a <= b ? 1 : 0;
}

export function _eq_(a) {
  return (b) => {
    if (a === b) {
      return 1;
    }
    if (typeof a === "object" && typeof b === "object") {
      if (a.length === 0 && b.length === 0) return 1;
      if (a.length !== b.length) return 0;
      const length = Math.max(a.length, b.length);
      for (let i = 0; i < length; i++) {
        if (_eq_(a.at(i))(b.at(i)) === 0) return 0;
      }
      return 1;
    }
    return 0;
  };
}

function _cons_(a) {
  return (b) => [a].concat(b);
}

// USER CODE

// signature: ((_map.q -> _map.t) -> (_map.q -> ([_map.q] -> [_map.t])))
export function _map(f) {
  return (x) => (xs) => _cons_(f(x))(map(f)(xs));
}

// signature: ((map.t -> map.g) -> ([map.t] -> [map.g]))
export function map(f) {
  return (xs) => (
    _eq_(xs)([]) ? /* [g] */ [] : _map(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (Num -> Num)
export function add1(x) {
  return _plus_(x)(1.0);
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
    _eq_(xs)([]) ? acc : _foldr(alg)(acc)(xs.at(0))(xs.slice(1))
  );
}

// signature: ((_foldl.j -> (_foldl.k -> _foldl.k)) -> (_foldl.k -> (_foldl.j -> ([_foldl.j] -> _foldl.k))))
export function _foldl(alg) {
  return (acc) => (x) => (xs) => alg(x)(foldl(alg)(acc)(xs));
}

// signature: ((foldl.a -> (foldl.b -> foldl.b)) -> (foldl.b -> ([foldl.a] -> foldl.b)))
export function foldl(alg) {
  return (acc) => (xs) => (
    _eq_(xs)([]) ? acc : _foldl(alg)(acc)(xs.at(0))(xs.slice(1))
  );
}

// signature: (Num -> (Num -> Num))
export function add(a) {
  return (b) => _plus_(a)(b);
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
  return (ns) => _cons_(n)(_cons_(n)(ns));
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
  return (xs) => _cons_(x)(xs);
}

// signature: ([reverse.a] -> [reverse.a])
export function reverse(xs) {
  return foldr(push)(/* [a] */ [])(xs);
}

// signature: (Num -> [Num])
export function range(n) {
  return (
    _lte_(n)(0.0) ? /* [Num] */ [] : _cons_(n)(range(_minus_(n)(1.0)))
  );
}

const False = 0;
const True = 1;

export const mapped = $mapped()
export const total = $total()
export const dub = $dub()

export const pipes = [];
