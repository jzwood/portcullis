export function makeGrah(topology, domain = "") {
  topology.forEach(([fxn, inAddresses, outAddressName]) => {
    makeEdge(domain, fxn, inAddresses, outAddressName);
  });
}

export function makeEdge(domain, fxn, inAddresses, outAddressName) {
  const apply = (fxn, [head, ...tail]) => {
    if (typeof (head) === "undefined") return fxn;
    return apply(fxn(head), tail);
  };
  const fmtName = (name) => [domain, name].filter(Boolean).join("/");
  //const outAddress = new BroadcastChannel(fmtName(outAddressName));
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
