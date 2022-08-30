//const pipes = [
//[add, [["counter", 1], ["add", 100]], "counter"],
//[sub, [["counter", 1], ["sub", 100]], "counter"]
//]

function dedupe(xs) {
  if (xs.length <= 1) return xs;
  return xs.slice(0, 1).concat(dedupe(xs.slice(1).filter((x) => x !== xs[0])));
}

function removeAllChildren(elem) {
  while (elem.firstChild) {
    elem.removeChild(elem.firstChild);
  }
}

export function visualize(elem, pipes) {
  console.log("IT IS WOKRING");
  removeAllChildren(elem);
  const svgns = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(svgns, "svg");
  elem.appendChild(svg);
  const width = 200;
  const height = 100;
  const rand = (dim, border) => border + Math.random() * (dim - 2 * border);
  svg.setAttribute("viewBox", `0 0 ${width} ${height}`);

  const queues = dedupe(
    pipes.flatMap(([_, inQueues, queueName]) =>
      inQueues.map(([queue, _]) => queue).concat(queueName)
    ),
  );
  const border = 5;
  queues.forEach((queueName) => {
    const queueNode = document.createElementNS(svgns, "text");
    const queueLabel = "&" + queueName;
    const label = document.createTextNode(queueLabel);
    queueNode.setAttribute("x", rand(width, border));
    queueNode.setAttribute("y", rand(height, border));
    queueNode.id = queueLabel;
    queueNode.style.fontFamily = "Courier New";
    queueNode.style.fontSize = "5px";
    queueNode.appendChild(label);
    svg.appendChild(queueNode);
  });
  pipes.forEach(([fxn, inQueues, outQueueName]) => {
    const functionNode = document.createElementNS(svgns, "text");
    const functionLabel = "λ." + fxn.name;
    const outQueueLabel = "&" + outQueueName;
    const label = document.createTextNode(functionLabel);
    functionNode.setAttribute("x", rand(width, border));
    functionNode.setAttribute("y", rand(height, border));
    functionNode.id = functionLabel;
    functionNode.style.fontFamily = "Courier New";
    functionNode.style.fontSize = "5px";
    functionNode.appendChild(label);
    svg.appendChild(functionNode);

    const line = document.createElementNS(svgns, "line");
    line.dataset.src = functionLabel;
    line.dataset.target = outQueueLabel;
    svg.appendChild(line);
    line.setAttribute('stroke', 'black')
    line.setAttribute('stroke-width', '0.25')

    inQueues.map(([inQueueName, _]) => {
      const inQueueLabel = '&' + inQueueName
      const line = document.createElementNS(svgns, "line");
      line.setAttribute('stroke', 'black')
      line.setAttribute('stroke-width', '0.25')
      line.dataset.src = inQueueLabel;
      line.dataset.target = functionLabel;
      svg.appendChild(line);
    });
  });

  //setInterval(() => {
    //const textNodes = svg.getElementsByTagNameNS(svgns, "text");
    const lines = svg.getElementsByTagNameNS(svgns, "line");
    Array.from(lines).forEach((line) => {
      const { src, target } = line.dataset
      const srcNode = svg.getElementById(src)
      const targetNode = svg.getElementById(target)
      line.setAttribute("x1", srcNode.getAttribute('x'));
      line.setAttribute("y1", srcNode.getAttribute('y'));
      line.setAttribute("x2", targetNode.getAttribute('x'));
      line.setAttribute("y2", targetNode.getAttribute('y'));
    });
  //}, 10000);
}
