// https://faculty.washington.edu/joelross/courses/archive/s13/cs261/lab/k/
function magnitude({ x, y }) {
  return Math.sqrt(x * x + y * y);
}

function fxy(f, { x, y }) {
  return { x: f * x, y: f * y };
}

function norm({ x, y }) {
  const mag = magnitude({ x, y });
  return fxy(1 / mag, { x, y });
}

function xyBinop(op, { x: x1, y: y1 }, { x: x2, y: y2 }) {
  return { x: op(x1, x2), y: op(y1, y2) };
}

const add = (a, b) => a + b;
const sub = (a, b) => a - b;

function clamp(lower, higher, value) {
  return Math.min(higher, Math.max(value, lower));
}

export function force({ width, height, nodes, edges, iterations = 10 }) {
  if (edges.length === 0) {
    return nodes; // seems to go wonky when there are is only 1 node and no edges
  }
  const area = width * height;

  const k = Math.sqrt(area / nodes.length);
  const fa = (x) => (x * x) / k;
  const fr = (x) => (k * k) / x;
  const border = 20;
  const clampH = (y) => clamp(border, height - 2 * border, y);
  const clampW = (x) => clamp(border, width - border, x);
  const t = 1;

  const attrNodes = nodes.map(({ id, position }) => ({
    id,
    disp: { x: 0, y: 0 },
    pos: { ...position },
  }));

  const attrNodeLookup = attrNodes.reduce((acc, val) => {
    acc[val.id] = val;
    return acc;
  }, {});

  const attrEdges = edges
    .filter(({ source, target }) => source !== target)
    .map(({ source, target }) => ({
      node1: attrNodeLookup[source],
      node2: attrNodeLookup[target],
    }));

  const deltaToFaTerm = (delta) => {
    const term1 = norm(delta);
    const term2 = fa(magnitude(delta));
    return fxy(term2, term1);
  };

  const deltaToFrTerm = (delta) => {
    const term1 = norm(delta);
    const term2 = fr(magnitude(delta));
    return fxy(term2, term1);
  };

  Array.from(Array(iterations))
    .forEach(() => {
      // calculate repulsive forces
      attrNodes.forEach((node1) => {
        node1.disp.x = 0;
        node1.disp.y = 0;
        attrNodes.forEach((node2) => {
          if (node1.id !== node2.id) {
            const delta = xyBinop(sub, node1.pos, node2.pos);
            const frTerm = deltaToFrTerm(delta);
            const disp = xyBinop(add, node1.disp, frTerm);
            node1.disp = disp;
          }
        });
      });
      // calculate attractive forces
      attrEdges.forEach(({ node1, node2 }) => {
        const delta = xyBinop(sub, node1.pos, node2.pos);
        const faTerm = deltaToFaTerm(delta);
        const disp1 = xyBinop(sub, node1.disp, faTerm);
        const disp2 = xyBinop(add, node2.disp, faTerm);
        node1.disp = disp1;
        node2.disp = disp2;
      });
      // limit displacement to temp t and keep displacement inside frame
      attrNodes.forEach((node) => {
        const delta = node.disp;
        const normal = norm(delta);
        const min = Math.min(magnitude(delta), t);
        const term = fxy(min, normal);
        const pos = xyBinop(add, node.pos, term);
        node.pos = pos;
        node.pos.x = clampW(node.pos.x);
        node.pos.y = clampH(node.pos.y);
      });
    });

  const newNodes = nodes.map((node) => {
    const { pos } = attrNodeLookup[node.id];
    return { ...node, position: pos };
  });
  return newNodes;
}
