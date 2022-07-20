const False = 0
const True = 1

export const one1 = $one1()
export const one2 = $one2()

// signature: (x -> x)
export function id(x) {
  return x;
}

// signature: ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}

// signature: (Num -> Num)
export function double(x) {
  return (2.0 * x);
}

// signature: (Num -> Num)
export function quadruple(n) {
  return compose(double)(double)(n);
}

// signature: (x -> x)
export function id2(x) {
  return compose(id)(id)(x);
}

// signature: Num
function $one1() {
  return 1.0;
}

// signature: Num
function $one2() {
  return compose(id)(id)(one1);
}

// signature: (z -> ([z] -> [z]))
export function id3(x) {
  return (xs) => xs;
}

// signature: ([t] -> [t])
export function tail(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ id3(xs.at(0))(xs.slice(1))
  );
}

// signature: ((h -> h) -> (h -> h))
export function a(fx) {
  return (x) => fx(x);
}

// signature: (q -> q)
export function b(w) {
  return w;
}

// signature: (p -> p)
export function c(y) {
  return a(b)(y);
}

// signature: ([h] -> (h -> ([h] -> [h])))
export function push(ys) {
  return (x) => (xs) => [x, ...concat(xs)(ys)];
}

// signature: ([a] -> ([a] -> [a]))
export function concat(xs) {
  return (ys) => (
    /* if */ equal(xs, []) ?
    /* then */ ys :
    /* else */ push(ys)(xs.at(0))(xs.slice(1))
  );
}

// signature: ((x -> Atom) -> (x -> ([x] -> [x])))
export function filter2(g) {
  return (w) => (ws) => concat((
    /* if */ g(w) ?
    /* then */ /* [x] */ [w] :
    /* else */ /* [x] */ []
  ))(filter(g)(ws));
}

// signature: ((j -> Atom) -> ([j] -> [j]))
export function filter(f) {
  return (xs) => (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ filter2(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (a -> (a -> Atom))
export function eq(x) {
  return (y) => equal(x, y);
}

// signature: ([Num] -> [Num])
export function seven(xs) {
  return filter(eq(7.0))(xs);
}

export function getTopology(){
  return [];
}

// INTERNAL
function equal(a, b) {
  if (a === b) {
    return +true;
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length === 0 && b.length === 0) return +true;
    if (a.length !== b.length) return +false;
    return equal(a.at(0), b.at(0)) && equal(a.slice(1), b.slice(1));
  }
  return +false;
}

// UTILS FOR BUILDING DATAFLOWS
export function makeGraph(topology, domain = "") {
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
export const _equal = equal;
