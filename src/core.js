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

function initPipe(fxn, inQueueNames, outQueueName) {
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
