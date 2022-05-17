const False = 0
const True = 1


// function "and" has type (Atom -> (Atom -> Atom))
export function and(a) {
  return (b) => (
    /* if */ equal(True, a) ?
    /* then */ (
      /* if */ equal(True, b) ?
      /* then */ True :
      /* else */ False
    ) :
    /* else */ False
  );
}
// function "fizzbuzz" has type (Num -> Num)
export function fizzbuzz(n) {
  return (
    /* if */ and(equal(0.0, (n % 3.0)))(equal(0.0, (n % 5.0))) ?
    /* then */ (0.0 - 3.0) :
    /* else */ (
      /* if */ equal(0.0, (n % 5.0)) ?
      /* then */ (0.0 - 2.0) :
      /* else */ (
        /* if */ equal(0.0, (n % 3.0)) ?
        /* then */ (0.0 - 1.0) :
        /* else */ n
      )
    )
  );
}
// function "fizzbuzzN" has type (Num -> [Num])
export function fizzbuzzN(n) {
  return (
    /* if */ equal(n, 0.0) ?
    /* then */ /* [Num] */ [0.0] :
    /* else */ [...fizzbuzzN((n - 1.0)), fizzbuzz(n)]
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
