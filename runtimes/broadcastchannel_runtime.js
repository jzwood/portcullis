export function makeGrah(topology, namespace = "") {
  topology.forEach(([fxn, inStreams, outStreamName]) => {
    makeEdge(namespace, fxn, inStreams, outStreamName);
  });
}

function apply(fxn, [head, ...tail]) {
  return typeof (head) === "undefined" ? fxn : apply(fxn(head), tail);
}

export function makeEdge(namespace, fxn, inStreams, outStreamName) {
  const fmtName = (name) => [namespace, name].filter(Boolean).join("/");
  const outStream = new BroadcastChannel(fmtName(outStreamName));
  const buffers = [];
  inStreams.forEach(([streamName, bufferSize]) => {
    const stream = new BroadcastChannel(fmtName(streamName));
    const buffer = [];
    buffers.push(buffer);
    stream.onmessage = ({ data }) => {
      buffer.unshift(data);
      buffer.splice(bufferSize);
      if (buffers.every((buff) => buff.length > 0)) {
        const args = buffers.map((args) => args.pop());
        const result = apply(fxn, args);
        outStream.postMessage(result);
      }
    };
  });
}
