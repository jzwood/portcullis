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

// signature: (Num -> (Num -> Num))
export function add(a) {
  return (b) => _plus_(a)(b);
}

// signature: (Num -> (Num -> (Num -> Num)))
export function add3(a) {
  return (b) => (c) => _plus_(_plus_(a)(b))(c);
}

// signature: (Num -> Num)
export function calc0(num) {
  return _plus_(_minus_(0.0)(_mult_(num)(9.0)))(_div_(3.0)(4.0));
}

// signature: (Num -> Num)
export function calc1(num) {
  return _plus_(_minus_(0.0)(_mult_(num)(9.0)))(_div_(3.0)(4.0));
}

// signature: (Num -> Atom)
export function choose0(num) {
  return (
    _gt_(num)(0.0) ? Cool : (
      _eq_(num)(0.0) ? Hmm : Bad
    )
  );
}

// signature: (Num -> Atom)
export function choose1(num) {
  return (
    _lte_(num)(0.0) ? Cool : (
      _lt_(num)(100.0) ? Hmm : Bad
    )
  );
}

// signature: (Num -> Atom)
export function isEven(num) {
  return _eq_(_rem_(num)(2.0))(0.0);
}

// signature: (append3.a -> ([append3.a] -> [append3.a]))
export function append3(x) {
  return (xs) => _cons_(x)(_cons_(x)(_cons_(x)(xs)));
}

const False = 0;
const True = 1;
const Cool = 2;
const Hmm = 3;
const Bad = 4;


export const pipes = [];
