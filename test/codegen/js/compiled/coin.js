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

// signature: (id.a -> id.a)
export function id(x) {
  return x;
}

// signature: (cmp.x -> (cmp.x -> Atom))
export function cmp(a) {
  return (b) => _eq_(a)(b);
}

// signature: (Atom -> (Num -> Num))
export function incr(won) {
  return (score) => _plus_(score)((
    won ? 1.0 : _minus_(0.0)(1.0)
  ));
}

const False = 0;
const True = 1;


export const pipes = [
  [id, [["&guess", 99]], "&newCoin"],
  [cmp, [["&guess", 99], ["&coin", 99]], "&win"],
  [incr, [["&win", 99], ["&score", 99]], "&score"]
]
