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

// signature: ((_not.a -> (_not.a -> Atom)) -> (_not.a -> (_not.a -> Atom)))
export function _not(f) {
  return (a) => (b) => (
    _eq_(False)(f(a)(b)) ? True : False
  );
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

// signature: ([push.h] -> (push.h -> ([push.h] -> [push.h])))
export function push(ys) {
  return (x) => (xs) => _cons_(x)(concat(xs)(ys));
}

// signature: ([concat.a] -> ([concat.a] -> [concat.a]))
export function concat(xs) {
  return (ys) => (
    _eq_(xs)([]) ? ys : push(ys)(xs.at(0))(xs.slice(1))
  );
}

// signature: (identity2.x -> ([identity2.x] -> [identity2.x]))
export function identity2(x) {
  return (xs) => xs;
}

// signature: ([tail.p] -> [tail.p])
export function tail(xs) {
  return (
    _eq_(xs)([]) ? xs : identity2(xs.at(0))(xs.slice(1))
  );
}

// signature: ([drop.g] -> (Num -> [drop.g]))
export function drop(xs) {
  return (n) => (
    _lte_(n)(0.0) ? xs : drop(tail(xs))(_minus_(n)(1.0))
  );
}

// signature: (Num -> (_take.f -> ([_take.f] -> [_take.f])))
export function _take(n) {
  return (x) => (xs) => (
    _lte_(n)(0.0) ? /* [_take.f] */ [] : _cons_(x)(take(xs)(_minus_(n)(1.0)))
  );
}

// signature: ([take.k] -> (Num -> [take.k]))
export function take(xs) {
  return (n) => (
    _eq_(xs)([]) ? /* [take.k] */ [] : _take(n)(xs.at(0))(xs.slice(1))
  );
}

// signature: ([slice.q] -> (Num -> (Num -> [slice.q])))
export function slice(xs) {
  return (i) => (j) => take(drop(xs)(i))(_minus_(j)(i));
}

// signature: ((filter2.x -> Atom) -> (filter2.x -> ([filter2.x] -> [filter2.x])))
export function filter2(g) {
  return (w) => (ws) => concat((
    g(w) ? /* [filter2.x] */ [w] : /* [filter2.x] */ []
  ))(filter(g)(ws));
}

// signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
export function filter(f) {
  return (xs) => (
    _eq_(xs)([]) ? xs : filter2(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (Num -> Num)
export function neg(x) {
  return _minus_(0.0)(x);
}

// signature: [Num]
function $empty() {
  return /* [Num] */ [];
}

// signature: ([Num] -> [Num])
export function msort(ns) {
  return msort2(length(ns))(ns);
}

// signature: (Num -> ([Num] -> [Num]))
export function msort2(len) {
  return (ns) => (
    _lte_(len)(1.0) ? ns : merge(msort(take(ns)(_div_(len)(2.0))))(msort(drop(ns)(_div_(len)(2.0))))
  );
}

// signature: (Num -> ([Num] -> (Num -> ([Num] -> [Num]))))
export function merge3(x) {
  return (xs) => (y) => (ys) => (
    _lte_(x)(y) ? _cons_(x)(merge(xs)(_cons_(y)(ys))) : _cons_(y)(merge(_cons_(x)(xs))(ys))
  );
}

// signature: ([Num] -> (Num -> ([Num] -> [Num])))
export function merge2(ys) {
  return (x) => (xs) => (
    _eq_(ys)([]) ? _cons_(x)(xs) : merge3(x)(xs)(ys.at(0))(ys.slice(1))
  );
}

// signature: ([Num] -> ([Num] -> [Num]))
export function merge(xs) {
  return (ys) => (
    _eq_(xs)([]) ? ys : merge2(ys)(xs.at(0))(xs.slice(1))
  );
}

// signature: (Num -> (Num -> Num))
export function avg(a) {
  return (b) => _mult_(0.5)(_plus_(a)(b));
}

// signature: ([Num] -> Num)
export function mean(xs) {
  return _div_(total(xs))(length(xs));
}

// signature: (Num -> ([Num] -> Num))
export function _total(x) {
  return (xs) => _plus_(x)(total(xs));
}

// signature: ([Num] -> Num)
export function total(xs) {
  return (
    _eq_(xs)([]) ? 0.0 : _total(xs.at(0))(xs.slice(1))
  );
}

// signature: ((compose.b -> compose.c) -> ((compose.a -> compose.b) -> (compose.a -> compose.c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}

// signature: (Atom -> (Atom -> [Atom Atom]))
export function rankPet(p1) {
  return (p2) => (
    _eq_(Chipmunk)(p1) ? [p1, p2] : [p2, p1]
  );
}

// signature: (Num -> (Num -> Atom))
export function lt(x) {
  return (y) => _gt_(x)(y);
}

// signature: (Num -> (Num -> Atom))
export function gte(x) {
  return (y) => _lte_(x)(y);
}

// signature: (Num -> ([Num] -> [Num]))
export function qsortp(x) {
  return (xs) => concat(qsort(filter(lt(x))(xs)))(_cons_(x)(qsort(filter(gte(x))(xs))));
}

// signature: ([Num] -> [Num])
export function qsort(xs) {
  return (
    _eq_(xs)([]) ? xs : qsortp(xs.at(0))(xs.slice(1))
  );
}

const False = 0;
const True = 1;
const Chipmunk = 2;

export const empty = $empty()

export const pipes = [];
