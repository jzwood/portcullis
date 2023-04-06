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

// signature: (cons.a -> ([cons.a] -> [cons.a]))
export function cons(x) {
  return (xs) => _cons_(x)(_cons_(x)(_cons_(x)(xs)));
}

// signature: (count.a -> ([count.a] -> Num))
export function count(x) {
  return (xs) => _plus_(1.0)(length(xs));
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
  return (xs) => _plus_(1.0)(length(xs));
}

// signature: ([length.a] -> Num)
export function length(xs) {
  return (
    _eq_(xs)([]) ? 0.0 : _length(xs.at(0))(xs.slice(1))
  );
}

// signature: ([push.a] -> (push.a -> ([push.a] -> [push.a])))
export function push(ys) {
  return (x) => (xs) => _cons_(x)(concat(xs)(ys));
}

// signature: ([concat.a] -> ([concat.a] -> [concat.a]))
export function concat(xs) {
  return (ys) => (
    _eq_(xs)([]) ? ys : push(ys)(xs.at(0))(xs.slice(1))
  );
}

// signature: (lid.a -> [lid.a])
export function lid(x) {
  return /* [lid.a] */ [x];
}

const False = 0;
const True = 1;


export const pipes = [];
