const False = 0
const True = 1


// function "filter" has type (([a] -> Atom) -> ([a] -> [a]))
export function filter(f) {
  return (xs) => (
    /* if */ equal(0.0, xs.length) ?
    /* then */ xs :
    /* else */ (
      /* if */ f(xs) ?
      /* then */ Array.prototype.concat.call((xs.slice(0.0, 1.0)), filter(f)(tail(xs))) :
      /* else */ filter(f)(tail(xs))
    )
  );
}
// function "tail" has type ([a] -> [a])
export function tail(xs) {
  return (xs.slice(1.0, xs.length));
}
// function "lt" has type (Num -> ([Num] -> Atom))
export function lt(p) {
  return (xs) => ((xs.at(0.0) ?? 0.0) < p);
}
// function "eq" has type (Num -> ([Num] -> Atom))
export function eq(p) {
  return (xs) => equal((xs.at(0.0) ?? 0.0), p);
}
// function "gt" has type (Num -> ([Num] -> Atom))
export function gt(p) {
  return (xs) => ((xs.at(0.0) ?? 0.0) > p);
}
// function "qsort" has type ([Num] -> [Num])
export function qsort(xs) {
  return (
    /* if */ (1.0 >= xs.length) ?
    /* then */ xs :
    /* else */ qsortP((xs.at(0.0) ?? 0.0))(xs)
  );
}
// function "qsortP" has type (Num -> ([Num] -> [Num]))
export function qsortP(pivot) {
  return (xs) => Array.prototype.concat.call(qsort(filter(lt(pivot))(xs)), Array.prototype.concat.call(filter(eq(pivot))(xs), qsort(filter(gt(pivot))(xs))));
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
