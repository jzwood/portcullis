const False = 0
const True = 1
const Chipmunk = 2

export const empty = $empty()

/*  UNSTANDARD LIB  */
// ((a -> (a -> Atom)) -> (a -> (a -> Atom)))
export function not(f) {
  return (a) => (b) => (
    /* if */ equal(False, f(a)(b)) ?
    /* then */ True :
    /* else */ False
  );
}

// (a -> ([a] -> Num))
export function _length(x) {
  return (xs) => (1.0 + length(xs));
}

// ([a] -> Num)
export function length(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ 0.0 :
    /* else */ _length(xs.at(0))(xs.slice(1))
  );
}

// ([h] -> (h -> ([h] -> [h])))
export function push(ys) {
  return (x) => (xs) => [x, ...concat(xs)(ys)];
}

// ([a] -> ([a] -> [a]))
export function concat(xs) {
  return (ys) => (
    /* if */ equal(xs, []) ?
    /* then */ ys :
    /* else */ push(ys)(xs.at(0))(xs.slice(1))
  );
}

// (x -> ([x] -> [x]))
export function identity2(x) {
  return (xs) => xs;
}

// ([p] -> [p])
export function tail(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ identity2(xs.at(0))(xs.slice(1))
  );
}

// ([g] -> (Num -> [g]))
export function drop(xs) {
  return (n) => (
    /* if */ (n <= 0.0) ?
    /* then */ xs :
    /* else */ drop(tail(xs))((n - 1.0))
  );
}

// (Num -> (f -> ([f] -> [f])))
export function _take(n) {
  return (x) => (xs) => (
    /* if */ (n <= 0.0) ?
    /* then */ /* [f] */ [] :
    /* else */ [x, ...take(xs)((n - 1.0))]
  );
}

// ([k] -> (Num -> [k]))
export function take(xs) {
  return (n) => (
    /* if */ equal(xs, []) ?
    /* then */ /* [k] */ [] :
    /* else */ _take(n)(xs.at(0))(xs.slice(1))
  );
}

// ([q] -> (Num -> (Num -> [q])))
export function slice(xs) {
  return (i) => (j) => take(drop(xs)(i))((j - i));
}

// ((x -> Atom) -> (x -> ([x] -> [x])))
export function filter2(g) {
  return (w) => (ws) => concat((
    /* if */ g(w) ?
    /* then */ /* [x] */ [w] :
    /* else */ /* [x] */ []
  ))(filter(g)(ws));
}

// ((j -> Atom) -> ([j] -> [j]))
export function filter(f) {
  return (xs) => (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ filter2(f)(xs.at(0))(xs.slice(1))
  );
}

/*  TEST BELOW  */
// (Num -> Num)
export function neg(x) {
  return (0.0 - x);
}

// [Num]
function $empty() {
  return /* [Num] */ [];
}

// ([Num] -> [Num])
export function msort(ns) {
  return msort2(length(ns))(ns);
}

// (Num -> ([Num] -> [Num]))
export function msort2(len) {
  return (ns) => (
    /* if */ (len <= 1.0) ?
    /* then */ ns :
    /* else */ merge(msort(take(ns)((len / 2.0))))(msort(drop(ns)((len / 2.0))))
  );
}

// (Num -> ([Num] -> (Num -> ([Num] -> [Num]))))
export function merge3(x) {
  return (xs) => (y) => (ys) => (
    /* if */ (x <= y) ?
    /* then */ [x, ...merge(xs)([y, ...ys])] :
    /* else */ [y, ...merge([x, ...xs])(ys)]
  );
}

// ([Num] -> (Num -> ([Num] -> [Num])))
export function merge2(ys) {
  return (x) => (xs) => (
    /* if */ equal(ys, []) ?
    /* then */ [x, ...xs] :
    /* else */ merge3(x)(xs)(ys.at(0))(ys.slice(1))
  );
}

// ([Num] -> ([Num] -> [Num]))
export function merge(xs) {
  return (ys) => (
    /* if */ equal(xs, []) ?
    /* then */ ys :
    /* else */ merge2(ys)(xs.at(0))(xs.slice(1))
  );
}

// (Num -> (Num -> Num))
export function avg(a) {
  return (b) => (0.5 * (a + b));
}

// ([Num] -> Num)
export function mean(xs) {
  return (sum(xs) / length(xs));
}

// (Num -> ([Num] -> Num))
export function sum2(x) {
  return (xs) => (x + sum(xs));
}

// ([Num] -> Num)
export function sum(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ 0.0 :
    /* else */ sum2(xs.at(0))(xs.slice(1))
  );
}

// ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}

/*  this is a comment  */
// (Atom -> (Atom -> [Atom Atom]))
export function rankPet(p1) {
  return (p2) => (
    /* if */ equal(Chipmunk, p1) ?
    /* then */ [p1, p2] :
    /* else */ [p2, p1]
  );
}

// (Num -> (Num -> Atom))
export function lt(x) {
  return (y) => (x > y);
}

// (Num -> (Num -> Atom))
export function gte(x) {
  return (y) => (x <= y);
}

// (Num -> ([Num] -> [Num]))
export function qsortp(x) {
  return (xs) => concat(qsort(filter(lt(x))(xs)))([x, ...qsort(filter(gte(x))(xs))]);
}

// ([Num] -> [Num])
export function qsort(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ qsortp(xs.at(0))(xs.slice(1))
  );
}

/*  BAD  */
// ((a -> Atom) -> ([a] -> [a]))
export function hofBad(f) {
  return (xs) => (
    /* if */ f(xs) ?
    /* then */ xs :
    /* else */ xs
  );
}

/*  end comment  */
// function "equal" has type (a -> (a -> Atom))
function equal(a, b) {
  if (a === b) {
    return +true;
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length === 0 && b.length === 0) return +true;
    if (a.length !== b.length) return +false;
    const [aHead, ...aTail] = a;
    const [bHead, ...bTail] = b;
    return equal(aHead, bHead) && equal(aTail, bTail);
  }
  return +false;
}

function addPipe(fxn, inQueueNames, outQueueName) {
  const queueMap = inQueueNames.reduce((acc, queueName) => ({
    ...acc,
    [queueName]: { queue: new BroadcastChannel(queueName), buffer: [] },
  }));
  const outQueue = new BroadcastChannel(outQueueName);
  inQueueNames.forEach((queueName) => {
    const { queue, buffer } = queueMap[queueName];
    queue.onmessage = (data) => {
      buffer.push(data);
      if (buffers.every((buff) => buff.length > 0)) {
        const args = buffers.map((buff) => buff.shift());
        const result = apply(fxn, args);
        outQueue.postMessage(result);
      }
    };
  });
}

export const _equal = equal; // for testing
