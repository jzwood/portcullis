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

// signature: (id1.x -> id1.x)
export function id1(x) {
  return x;
}

// signature: ((compose.b -> compose.c) -> ((compose.a -> compose.b) -> (compose.a -> compose.c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}

// signature: (Num -> Num)
export function double(x) {
  return _mult_(2.0)(x);
}

// signature: (Num -> Num)
export function quadruple(n) {
  return compose(double)(double)(n);
}

// signature: (id2.x -> id2.x)
export function id2(x) {
  return compose(id1)(id1)(x);
}

// signature: Num
function $one1() {
  return 1.0;
}

// signature: Num
function $one2() {
  return compose(id1)(id1)(one1);
}

// signature: (id3.z -> ([id3.z] -> [id3.z]))
export function id3(x) {
  return (xs) => xs;
}

// signature: ([tail.t] -> [tail.t])
export function tail(xs) {
  return (
    _eq_(xs)([]) ? xs : id3(xs.at(0))(xs.slice(1))
  );
}

// signature: ((a.h -> a.h) -> (a.h -> a.h))
export function a(fx) {
  return (x) => fx(x);
}

// signature: (b.q -> b.q)
export function b(w) {
  return w;
}

// signature: (c.p -> c.p)
export function c(y) {
  return a(b)(y);
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

// signature: ((filter2.x -> Atom) -> (filter2.x -> ([filter2.x] -> [filter2.x])))
export function filter2(g) {
  return (w) => (ws) => concat((
    g(w) ? /* [x] */ [w] : /* [x] */ []
  ))(filter(g)(ws));
}

// signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
export function filter(f) {
  return (xs) => (
    _eq_(xs)([]) ? xs : filter2(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (eq.a -> (eq.a -> Atom))
export function eq(x) {
  return (y) => _eq_(x)(y);
}

// signature: ([Num] -> [Num])
export function seven(xs) {
  return filter(eq(7.0))(xs);
}

// signature: Num
function $eight() {
  return _plus_(3.0)(5.0);
}

// signature: [Atom]
function $atoms() {
  return /* [Atom] */ [One, Two, Three, Four, Five];
}

const False = 0;
const True = 1;
const One = 2;
const Two = 3;
const Three = 4;
const Four = 5;
const Five = 6;

export const one1 = $one1()
export const one2 = $one2()
export const eight = $eight()
export const atoms = $atoms()

export const pipes = [];
