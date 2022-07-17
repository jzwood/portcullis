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
