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

// signature: ((_map.a -> _map.b) -> (_map.a -> ([_map.a] -> [_map.b])))
export function _map(f) {
  return (x) => (xs) => _cons_(f(x))(map(f)(xs));
}

// signature: ((map.a -> map.b) -> ([map.a] -> [map.b]))
export function map(f) {
  return (xs) => (
    _eq_(xs)([]) ? /* [map.b] */ [] : _map(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (Num -> (Num -> [Num]))
export function range(n0) {
  return (n1) => (
    _gt_(n0)(n1) ? /* [Num] */ [] : _cons_(n0)(range(_plus_(n0)(1.0))(n1))
  );
}

// signature: (Num -> Num)
export function _fizzbuzz(n) {
  return (
    _eq_(0.0)(_rem_(n)(15.0)) ? _minus_(0.0)(35.0) : (
      _eq_(0.0)(_rem_(n)(5.0)) ? _minus_(0.0)(5.0) : (
        _eq_(0.0)(_rem_(n)(3.0)) ? _minus_(0.0)(3.0) : n
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
