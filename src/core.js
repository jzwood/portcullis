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

function extendPipeline(fxn, inQueueNames, outQueueName) {
  const apply = (fxn, [head, ...tail]) => {
    if (head == undefined) return fxn;
    return apply(fxn(head), tail);
  };
  const outQueue = new BroadcastChannel(outQueueName);
  const buffers = [];
  inQueueNames.forEach((queueName) => {
    const queue = new BroadcastChannel(queueName);
    const buffer = [];
    buffers.push(buffer);
    queue.onmessage = ({ data }) => {
      buffer.push(data);
      if (buffers.every((buff) => buff.length > 0)) {
        const args = buffers.map((buff) => buff.shift());
        const result = apply(fxn, args);
        outQueue.postMessage(result);
      }
    };
  });
}

// for testing
export const _extendPipeline = extendPipeline;
export const _equal = equal;
