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

// signature: (Atom -> Num)
export function hello(what) {
  return (
    _eq_(what)(World) ? 42.0 : (
      _eq_(what)(Ma) ? 10.0 : _minus_(0.0)(100.0)
    )
  );
}

// signature: (Num -> ([Num] -> Num))
export function _sum(x) {
  return (xs) => _plus_(x)(sum(xs));
}

// signature: ([Num] -> Num)
export function sum(xs) {
  return (
    _eq_(xs)([]) ? 0.0 : _sum(xs.at(0))(xs.slice(1))
  );
}

const False = 0;
const True = 1;
const World = 2;
const Ma = 3;


export const pipes = [];
