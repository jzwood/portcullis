const False = 0
const True = 1
const Chipmunk = 2

export const empty = $empty()

// function "neg" has type (Num -> Num)
export function neg(x) {
  return (0.0 - x);
}
// function "tail" has type ([a] -> [a])
export function tail(xs) {
  return (xs.slice(1.0, xs.length));
}
// function "empty" has type [Num]
function $empty() {
  return /* [Num] */ [];
}
// function "msort" has type ([Num] -> [Num])
export function msort(ns) {
  return msort2(ns.length)(ns);
}
// function "msort2" has type (Num -> ([Num] -> [Num]))
export function msort2(len) {
  return (ns) => (
    /* if */ (len <= 1.0) ?
    /* then */ ns :
    /* else */ merge(msort((ns.slice(0.0, (len / 2.0)))))(msort((ns.slice((len / 2.0), len))))
  );
}
// function "cmpHead" has type ([Num] -> ([Num] -> [[Num] [Num]]))
export function cmpHead(xs) {
  return (ys) => (
    /* if */ ((xs.at(0.0) ?? 0.0) < (ys.at(0.0) ?? 0.0)) ?
    /* then */ [xs, ys] :
    /* else */ [ys, xs]
  );
}
// function "merge" has type ([Num] -> ([Num] -> [Num]))
export function merge(xs) {
  return (ys) => (
    /* if */ equal(0.0, xs.length) ?
    /* then */ ys :
    /* else */ (
      /* if */ equal(0.0, ys.length) ?
      /* then */ xs :
      /* else */ merge2(cmpHead(xs)(ys))
    )
  );
}
// function "merge2" has type ([[Num] [Num]] -> [Num])
export function merge2(xys) {
  return Array.prototype.concat.call((xys[0].slice(0.0, 1.0)), merge(tail(xys[0]))(xys[1]));
}
// function "avg" has type (Num -> (Num -> Num))
export function avg(a) {
  return (b) => (0.5 * (a + b));
}
// function "mean" has type ([Num] -> Num)
export function mean(xs) {
  return (sum(xs) / xs.length);
}
// function "sum" has type ([Num] -> Num)
export function sum(xs) {
  return (
    /* if */ equal(0.0, xs.length) ?
    /* then */ 0.0 :
    /* else */ ((xs.at(0.0) ?? 0.0) + sum(tail(xs)))
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
