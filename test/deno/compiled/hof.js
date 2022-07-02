const False = 0
const True = 1

export const one1 = $one1()
export const one2 = $one2()


// Signature: (x -> x)
export function id(x) {
  return x;
}

// Signature: ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}

// Signature: (Num -> Num)
export function double(x) {
  return (2.0 * x);
}

// Signature: (Num -> Num)
export function quadruple(n) {
  return compose(double)(double)(n);
}

// Signature: (x -> x)
export function id2(x) {
  return compose(id)(id)(x);
}

// Signature: Num
function $one1() {
  return 1.0;
}

// Signature: Num
function $one2() {
  return compose(id)(id)(one1);
}

// Signature: (z -> ([z] -> [z]))
export function id3(x) {
  return (xs) => xs;
}

// Signature: ([t] -> [t])
export function tail(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ id3(xs.at(0))(xs.slice(1))
  );
}

// Signature: ((h -> h) -> (h -> h))
export function a(fx) {
  return (x) => fx(x);
}

// Signature: (q -> q)
export function b(w) {
  return w;
}

// Signature: (p -> p)
export function c(y) {
  return a(b)(y);
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

// Signature: (a -> (a -> Atom))
export function eq(x) {
  return (y) => equal(x, y);
}

// Signature: ([Num] -> [Num])
export function seven(xs) {
  return filter(eq(7.0))(xs);
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

function makePipe(domain, fxn, inQueues, outQueueName) {
  const apply = (fxn, [head, ...tail]) => {
    if (typeof(head) === 'undefined') return fxn;
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
export const _makePipe = makePipe;
export const _equal = equal;
