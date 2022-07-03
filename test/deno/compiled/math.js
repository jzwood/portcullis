const False = 0
const True = 1
const Chipmunk = 2

export const empty = $empty()

export function getTopology(){
  return [];
}

// Signature: ((a -> (a -> Atom)) -> (a -> (a -> Atom)))
export function not(f) {
  return (a) => (b) => (
    /* if */ equal(False, f(a)(b)) ?
    /* then */ True :
    /* else */ False
  );
}

// Signature: (a -> ([a] -> Num))
export function _length(x) {
  return (xs) => (1.0 + length(xs));
}

// Signature: ([a] -> Num)
export function length(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ 0.0 :
    /* else */ _length(xs.at(0))(xs.slice(1))
  );
}

// Signature: ([h] -> (h -> ([h] -> [h])))
export function push(ys) {
  return (x) => (xs) => [x, ...concat(xs)(ys)];
}

// Signature: ([a] -> ([a] -> [a]))
export function concat(xs) {
  return (ys) => (
    /* if */ equal(xs, []) ?
    /* then */ ys :
    /* else */ push(ys)(xs.at(0))(xs.slice(1))
  );
}

// Signature: (x -> ([x] -> [x]))
export function identity2(x) {
  return (xs) => xs;
}

// Signature: ([p] -> [p])
export function tail(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ identity2(xs.at(0))(xs.slice(1))
  );
}

// Signature: ([g] -> (Num -> [g]))
export function drop(xs) {
  return (n) => (
    /* if */ (n <= 0.0) ?
    /* then */ xs :
    /* else */ drop(tail(xs))((n - 1.0))
  );
}

// Signature: (Num -> (f -> ([f] -> [f])))
export function _take(n) {
  return (x) => (xs) => (
    /* if */ (n <= 0.0) ?
    /* then */ /* [f] */ [] :
    /* else */ [x, ...take(xs)((n - 1.0))]
  );
}

// Signature: ([k] -> (Num -> [k]))
export function take(xs) {
  return (n) => (
    /* if */ equal(xs, []) ?
    /* then */ /* [k] */ [] :
    /* else */ _take(n)(xs.at(0))(xs.slice(1))
  );
}

// Signature: ([q] -> (Num -> (Num -> [q])))
export function slice(xs) {
  return (i) => (j) => take(drop(xs)(i))((j - i));
}

// Signature: ((x -> Atom) -> (x -> ([x] -> [x])))
export function filter2(g) {
  return (w) => (ws) => concat((
    /* if */ g(w) ?
    /* then */ /* [x] */ [w] :
    /* else */ /* [x] */ []
  ))(filter(g)(ws));
}

// Signature: ((j -> Atom) -> ([j] -> [j]))
export function filter(f) {
  return (xs) => (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ filter2(f)(xs.at(0))(xs.slice(1))
  );
}

// Signature: (Num -> Num)
export function neg(x) {
  return (0.0 - x);
}

// Signature: [Num]
function $empty() {
  return /* [Num] */ [];
}

// Signature: ([Num] -> [Num])
export function msort(ns) {
  return msort2(length(ns))(ns);
}

// Signature: (Num -> ([Num] -> [Num]))
export function msort2(len) {
  return (ns) => (
    /* if */ (len <= 1.0) ?
    /* then */ ns :
    /* else */ merge(msort(take(ns)((len / 2.0))))(msort(drop(ns)((len / 2.0))))
  );
}

// Signature: (Num -> ([Num] -> (Num -> ([Num] -> [Num]))))
export function merge3(x) {
  return (xs) => (y) => (ys) => (
    /* if */ (x <= y) ?
    /* then */ [x, ...merge(xs)([y, ...ys])] :
    /* else */ [y, ...merge([x, ...xs])(ys)]
  );
}

// Signature: ([Num] -> (Num -> ([Num] -> [Num])))
export function merge2(ys) {
  return (x) => (xs) => (
    /* if */ equal(ys, []) ?
    /* then */ [x, ...xs] :
    /* else */ merge3(x)(xs)(ys.at(0))(ys.slice(1))
  );
}

// Signature: ([Num] -> ([Num] -> [Num]))
export function merge(xs) {
  return (ys) => (
    /* if */ equal(xs, []) ?
    /* then */ ys :
    /* else */ merge2(ys)(xs.at(0))(xs.slice(1))
  );
}

// Signature: (Num -> (Num -> Num))
export function avg(a) {
  return (b) => (0.5 * (a + b));
}

// Signature: ([Num] -> Num)
export function mean(xs) {
  return (sum(xs) / length(xs));
}

// Signature: (Num -> ([Num] -> Num))
export function sum2(x) {
  return (xs) => (x + sum(xs));
}

// Signature: ([Num] -> Num)
export function sum(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ 0.0 :
    /* else */ sum2(xs.at(0))(xs.slice(1))
  );
}

// Signature: ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}

// Signature: (Atom -> (Atom -> [Atom Atom]))
export function rankPet(p1) {
  return (p2) => (
    /* if */ equal(Chipmunk, p1) ?
    /* then */ [p1, p2] :
    /* else */ [p2, p1]
  );
}

// Signature: (Num -> (Num -> Atom))
export function lt(x) {
  return (y) => (x > y);
}

// Signature: (Num -> (Num -> Atom))
export function gte(x) {
  return (y) => (x <= y);
}

// Signature: (Num -> ([Num] -> [Num]))
export function qsortp(x) {
  return (xs) => concat(qsort(filter(lt(x))(xs)))([x, ...qsort(filter(gte(x))(xs))]);
}

// Signature: ([Num] -> [Num])
export function qsort(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ qsortp(xs.at(0))(xs.slice(1))
  );
}

// Signature: ((a -> Atom) -> ([a] -> [a]))
export function hofBad(f) {
  return (xs) => (
    /* if */ f(xs) ?
    /* then */ xs :
    /* else */ xs
  );
}

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

export function makeGrah(topology, domain = "") {
  topology.forEach(([fxn, inQueues, outQueueName]) => {
    makeEdge(domain, fxn, inQueues, outQueueName);
  });
}

export function makeEdge(domain, fxn, inQueues, outQueueName) {
  const apply = (fxn, [head, ...tail]) => {
    if (typeof (head) === "undefined") return fxn;
    return apply(fxn(head), tail);
  };
  const fmtName = (name) => [domain, name].filter(Boolean).join("/");
  const outQueue = new BroadcastChannel(fmtName(outQueueName));
  const buffers = [];
  inQueues.forEach(([queueName, bufferSize]) => {
    const queue = new BroadcastChannel(fmtName(queueName));
    const buffer = [];
    buffers.push(buffer);
    queue.onmessage = ({ data }) => {
      buffer.unshift(data);
      buffer.splice(bufferSize);
      if (buffers.every((buff) => buff.length > 0)) {
        const args = buffers.map((args) => args.pop());
        const result = apply(fxn, args);
        outQueue.postMessage(result);
      }
    };
  });
}

// for testing
export const _makeEdge = makeEdge;
export const _equal = equal;
