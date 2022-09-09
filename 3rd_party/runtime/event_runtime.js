export function makeGrah(topology, namespace = "") {
  topology.forEach(([fxn, inAddresses, outAddressName]) => {
    makeEdge(namespace, fxn, inAddresses, outAddressName);
  });
}

function apply(fxn, [head, ...tail]) {
  return typeof (head) === "undefined" ? fxn : apply(fxn(head), tail);
}

export function makeEdge(namespace, fxn, inAddresses, outAddressName) {
  const fmtName = (name) => [namespace, name].filter(Boolean).join("/");
  const buffers = [];
  inAddresses.forEach(([addressName, bufferSize]) => {
    const buffer = [];
    buffers.push(buffer);
    document.addEventListener(fmtName(addressName), ({ detail }) => {
      buffer.unshift(detail);
      buffer.splice(bufferSize);
      if (buffers.every((buff) => buff.length > 0)) {
        const args = buffers.map((args) => args.pop());
        const result = apply(fxn, args);
        const event = new CustomEvent(fmtName(outAddressName), {
          detail: result,
        });
        document.dispatchEvent(event);
      }
    });
  });
}
