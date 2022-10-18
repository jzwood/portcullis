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
const rand = (dim, border) => border + Math.random() * (dim - 2 * border);

function clamp(lower, higher, value) {
  return Math.min(higher, Math.max(value, lower));
}

function clampEllipse(width, height, pos) {
  const { x: x0, y: y0 } = pos
  const sign = (n) => (n >= 0 ? 1 : -1)
  const f =
    (width * height) /
    Math.sqrt(width * width * y0 * y0 + height * height * x0 * x0)
  const x = sign(x0) * Math.abs(f * x0)
  const y = sign(y0) * Math.abs(f * y0)
  return magnitude(pos) < magnitude({ x, y }) ? pos : { x, y }
}

export function force({ width, height, nodes, edges, iterations = 10 }) {
  if (edges.length === 0) {
    return nodes; // seems to go wonky when there are is only 1 node and no edges
  }
  const area = width * height;

  const k = Math.sqrt(area / nodes.length);
  const fa = (x) => (x * x) / k;
  const fr = (x) => (k * k) / x;
  const wBorder = 0.1 * width;
  const hBorder = 0.1 * height;
  const clampE = ({ x: x0, y: y0 }) => {
    const half_width = 0.5 * width
    const half_height = 0.5 * height
    const { x, y } = clampEllipse(half_width, half_height, {
      x: x0 - half_width,
      y: half_height - y0
    })
    return { x: x + half_width, y: -1 * (y - half_height) }
  }
  const clampH = (y) => clamp(0.05 * height, height - 0.05 * height, y);
  const clampW = (x) => clamp(0.025 * width, width - 0.15 * width, x);
  const t = 1;

  const attrNodes = nodes.map((id) => ({
    id,
    disp: { x: 0, y: 0 },
    pos: { x: rand(width, wBorder), y: rand(height, hBorder) },
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
        Object.assign(node.pos, clampE(node.pos))
        node.pos.x = clampW(node.pos.x);
        node.pos.y = clampH(node.pos.y);
      });
    });

  return attrNodes.map(({ id, pos: { x, y } }) => ({ name: id, x, y }));
}
