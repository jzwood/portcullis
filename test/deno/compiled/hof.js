const False = 0
const True = 1

export const one1 = $one1()
export const one2 = $one2()

// (x -> x)
export function id(x) {
  return x;
}

/* 
filter ...
 */
// ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}

// (Num -> Num)
export function double(x) {
  return (2.0 * x);
}

// (Num -> Num)
export function quadruple(n) {
  return compose(double)(double)(n);
}

// (x -> x)
export function id2(x) {
  return compose(id)(id)(x);
}

// Num
function $one1() {
  return 1.0;
}

// Num
function $one2() {
  return compose(id)(id)(one1);
}

// (z -> ([z] -> [z]))
export function id3(x) {
  return (xs) => xs;
}

// ([t] -> [t])
export function tail(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ id3(xs.at(0))(xs.slice(1))
  );
}

// ((h -> h) -> (h -> h))
export function a(fx) {
  return (x) => fx(x);
}

// (q -> q)
export function b(w) {
  return w;
}

// (p -> p)
export function c(y) {
  return a(b)(y);
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

// (a -> (a -> Atom))
export function eq(x) {
  return (y) => equal(x, y);
}

// ([Num] -> [Num])
export function seven(xs) {
  return filter(eq(7.0))(xs);
}

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
