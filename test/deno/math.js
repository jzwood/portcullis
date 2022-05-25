const False = 0
const True = 1
const Chipmunk = 2

export const empty = $empty()

// function "tailPlusOne" has type (a -> ([a] -> Num))
export function tailPlusOne(x) {
  return (xs) => (1.0 + length(xs));
}
// function "length" has type ([a] -> Num)
export function length(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ 0.0 :
    /* else */ tailPlusOne(xs.at(0))(xs.slice(1))
  );
}
// function "push" has type ([h] -> (h -> ([h] -> [h])))
export function push(ys) {
  return (x) => (xs) => [x, ...concat(xs)(ys)];
}
// function "concat" has type ([a] -> ([a] -> [a]))
export function concat(xs) {
  return (ys) => (
    /* if */ equal(xs, []) ?
    /* then */ ys :
    /* else */ push(ys)(xs.at(0))(xs.slice(1))
  );
}
// function "identity2" has type (x -> ([x] -> [x]))
export function identity2(x) {
  return (xs) => xs;
}
// function "tail" has type ([p] -> [p])
export function tail(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ identity2(xs.at(0))(xs.slice(1))
  );
}
// function "drop" has type ([g] -> (Num -> [g]))
export function drop(xs) {
  return (n) => (
    /* if */ (n <= 0.0) ?
    /* then */ xs :
    /* else */ drop(tail(xs))((n - 1.0))
  );
}
// function "take2" has type (Num -> (f -> ([f] -> [f])))
export function take2(n) {
  return (x) => (xs) => (
    /* if */ (n <= 0.0) ?
    /* then */ /* [f] */ [] :
    /* else */ [x, ...take(xs)((n - 1.0))]
  );
}
// function "take" has type ([k] -> (Num -> [k]))
export function take(xs) {
  return (n) => (
    /* if */ equal(xs, []) ?
    /* then */ /* [k] */ [] :
    /* else */ take2(n)(xs.at(0))(xs.slice(1))
  );
}
// function "slice" has type ([q] -> (Num -> (Num -> [q])))
export function slice(xs) {
  return (i) => (j) => take(drop(xs)(i))((j - i));
}
// function "filter2" has type ((x -> Atom) -> (x -> ([x] -> [x])))
export function filter2(g) {
  return (w) => (ws) => concat((
    /* if   */ g(w) ?
    /* then */ /* [x] */ [w] :
    /* else */ /* [x] */ []
  ))(filter(g)(ws));
}
// function "filter" has type ((j -> Atom) -> ([j] -> [j]))
export function filter(f) {
  return (xs) => (
    /* if   */ equal(xs, []) ?
    /* then */ xs :
    /* else */ filter2(f)(xs.at(0))(xs.slice(1))
  );
}
// function "neg" has type (Num -> Num)
export function neg(x) {
  return (0.0 - x);
}
// function "empty" has type [Num]
function $empty() {
  return /* [Num] */ [];
}
// function "avg" has type (Num -> (Num -> Num))
export function avg(a) {
  return (b) => (0.5 * (a + b));
}
// function "mean" has type ([Num] -> Num)
export function mean(xs) {
  return (sum(xs) / length(xs));
}
// function "sum2" has type (Num -> ([Num] -> Num))
export function sum2(x) {
  return (xs) => (x + sum(xs));
}
// function "sum" has type ([Num] -> Num)
export function sum(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ 0.0 :
    /* else */ sum2(xs.at(0))(xs.slice(1))
  );
}
// function "compose" has type ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}
// function "rankPet" has type (Atom -> (Atom -> [Atom Atom]))
export function rankPet(p1) {
  return (p2) => (
    /* if */ equal(Chipmunk, p1) ?
    /* then */ [p1, p2] :
    /* else */ [p2, p1]
  );
}
// function "hofBad" has type ((a -> Atom) -> ([a] -> [a]))
export function hofBad(f) {
  return (xs) => (
    /* if */ f(xs) ?
    /* then */ xs :
    /* else */ xs
  );
}
// function "equal" has type (a -> (a -> Atom))
export function equal(a, b) {
  if (a === b) {
    return +true;
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length === 0 && b.length === 0) return +true;
    if (a.length !== b.length) return +false;
    const [aHead, ...aTail] = a
    const [bHead, ...bTail] = b
    return equal(aHead, bHead) && equal(aTail, bTail)
  }
  return +false;
}
