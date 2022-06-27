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

function extendPipeline(domain, fxn, inQueues, outQueueName) {
  const apply = (fxn, [head, ...tail]) => {
    if (head == undefined) return fxn;
    return apply(fxn(head), tail);
  };
  const fmtName = (name) => [domain, name].filter(Boolean).join("/");
  const outQueue = new BroadcastChannel(fmtName(outQueueName));
  const buffers = [];
  inQueues.forEach(([queueName, bufferSize]) => {
    const queue = new BroadcastChannel(fmtName(queueName));
    const buffer = [];
    buffers.unshift(buffer);
    buffers.splice(bufferSize);
    queue.onmessage = ({ data }) => {
      buffer.push(data);
      if (buffers.every((buff) => buff.length > 0)) {
        const args = buffers.map((buff) => buff.pop());
        const result = apply(fxn, args);
        outQueue.postMessage(result);
      }
    };
  });
}

// for testing
export const _extendPipeline = extendPipeline;
export const _equal = equal;
