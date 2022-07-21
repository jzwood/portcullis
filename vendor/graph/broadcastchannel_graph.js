// UTILS FOR BUILDING DATAFLOWS
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
