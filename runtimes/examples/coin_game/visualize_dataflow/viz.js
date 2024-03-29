import { force } from "./force.js";

function magnitude(...nums) {
  return Math.sqrt(nums.reduce((mag, num) => mag + num * num, 0));
}

function unitVector({ x1, x2, y1, y2 }) {
  const dx = x2 - x1;
  const dy = y2 - y1;
  const mag = magnitude(dx, dy);
  return {
    vx: dx / mag,
    vy: dy / mag,
  };
}

function trimLine({ x1, y1, x2, y2, trimStart = 0, trimEnd = 0 }) {
  const { vx, vy } = unitVector({ x1, y1, x2, y2 });
  return {
    x1: x1 + trimStart * vx,
    y1: y1 + trimStart * vy,
    x2: x2 - trimEnd * vx,
    y2: y2 - trimEnd * vy,
  };
}

function dedupe(xs) {
  if (xs.length <= 1) return xs;
  return xs.slice(0, 1).concat(dedupe(xs.slice(1).filter((x) => x !== xs[0])));
}

export function removeAllChildren(elem) {
  while (elem.firstChild) {
    elem.removeChild(elem.firstChild);
  }
}

export function visualize(elem, pipes) {
  removeAllChildren(elem);
  const svgns = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(svgns, "svg");
  svg.style.border = "1px solid silver";
  elem.appendChild(svg);
  const defs = document.createElementNS(svgns, "defs");
  svg.appendChild(defs);
  const marker = document.createElementNS(svgns, "marker");
  marker.setAttribute("id", "arrow");
  marker.setAttribute("viewBox", "0 0 10 10");
  marker.setAttribute("refX", "5");
  marker.setAttribute("refY", "5");
  marker.setAttribute("markerWidth", "6");
  marker.setAttribute("markerHeight", "6");
  marker.setAttribute("orient", "auto-start-reverse");
  defs.appendChild(marker);
  const arrow = document.createElementNS(svgns, "path");
  arrow.setAttribute("d", "M 0 0 L 10 5 L 0 10 z");
  marker.appendChild(arrow);

  const width = 200;
  const height = 100;
  const rand = (dim, border) => border + Math.random() * (dim - 2 * border);
  svg.setAttribute("viewBox", `0 0 ${width} ${height}`);

  const streams = dedupe(
    pipes.flatMap(([_, inStreams, streamName]) =>
      inStreams.map(([stream, _]) => stream).concat(streamName)
    ),
  );
  const border = 5;
  streams.forEach((streamName) => {
    const streamNode = document.createElementNS(svgns, "text");
    const streamLabel = streamName;
    const label = document.createTextNode(streamLabel);
    streamNode.setAttribute("x", rand(width, border));
    streamNode.setAttribute("dx", 2);
    streamNode.setAttribute("y", rand(height, border));
    streamNode.id = streamLabel;
    streamNode.style.fontFamily = "Courier New";
    streamNode.style.fontSize = "5px";
    streamNode.style.dominantBaseline = "central";
    streamNode.appendChild(label);
    svg.appendChild(streamNode);
  });

  pipes.forEach(([fxn, inStreams, outStreamName]) => {
    const functionNode = document.createElementNS(svgns, "text");
    const functionLabel = "λ." + fxn.name;
    const outStreamLabel = outStreamName;
    const label = document.createTextNode(functionLabel);
    functionNode.setAttribute("x", rand(width, border));
    functionNode.setAttribute("dx", 2);
    functionNode.setAttribute("y", rand(height, border));
    functionNode.id = functionLabel;
    functionNode.style.fontFamily = "Courier New";
    functionNode.style.fontSize = "5px";
    functionNode.style.dominantBaseline = "central";
    functionNode.appendChild(label);
    svg.appendChild(functionNode);

    const line = document.createElementNS(svgns, "line");
    line.dataset.src = functionLabel;
    line.dataset.target = outStreamLabel;
    svg.appendChild(line);
    line.setAttribute("stroke", "rgba(0,0,0,0.5)");
    line.setAttribute("stroke-width", "0.25");
    line.setAttribute("marker-end", "url(#arrow)");

    inStreams.map(([inStreamName, _]) => {
      const line = document.createElementNS(svgns, "line");
      line.setAttribute("stroke", "rgba(0,0,0,0.5)");
      line.setAttribute("stroke-width", "0.25");
      line.setAttribute("marker-end", "url(#arrow)");
      line.dataset.src = inStreamName;
      line.dataset.target = functionLabel;
      svg.appendChild(line);
    });
  });

  // TRANFORM PIPES AND STREAMS INTO GRAPH
  const graphNodes = pipes.map(([fxn]) => "λ." + fxn.name).concat(streams);
  const graphEdges = pipes.flatMap(([fxn, inStreams, outStreamName]) =>
    inStreams.map(([inStreamName]) => ({
      source: inStreamName,
      target: "λ." + fxn.name,
    })).concat({ source: "λ." + fxn.name, target: outStreamName })
  );
  // CALCULATE AND SET FORCE DIRECTED LAYOUT FOR GRAPH
  const newNodes = force({
    width,
    height,
    nodes: graphNodes,
    edges: graphEdges,
    iterations: 120,
  });
  newNodes.forEach(({ name, x, y }) => {
    const node = svg.getElementById(name);
    node.setAttribute("x", x);
    node.setAttribute("y", y);
  });

  // SET LINES BETWEEN NODES
  const lines = svg.getElementsByTagNameNS(svgns, "line");
  Array.from(lines).forEach((line) => {
    const { src, target } = line.dataset;
    const srcNode = svg.getElementById(src);
    const targetNode = svg.getElementById(target);
    const x1 = parseFloat(srcNode.getAttribute("x"));
    const y1 = parseFloat(srcNode.getAttribute("y"));
    const x2 = parseFloat(targetNode.getAttribute("x"));
    const y2 = parseFloat(targetNode.getAttribute("y"));
    const xy = trimLine({
      x1,
      y1,
      x2,
      y2,
      trimStart: 3,
      trimEnd: 3,
    });
    line.setAttribute("x1", xy.x1);
    line.setAttribute("y1", xy.y1);
    line.setAttribute("x2", xy.x2);
    line.setAttribute("y2", xy.y2);

    const dot = document.createElementNS(svgns, "circle");
    dot.setAttribute("cx", x1);
    dot.setAttribute("cy", y1);
    dot.setAttribute("r", 0.75);
    svg.appendChild(dot);
  });
}
