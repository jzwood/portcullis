
//const pipes = [
  //[_add, [["counter", 1], ["add", 100]], "counter"],
  //[_sub, [["counter", 1], ["sub", 100]], "counter"]
//]

function dedupe(xs) {
  if (xs.length <= 1) return xs
  return xs.slice(0, 1).concat(dedupe(xs.slice(1).filter((x) => x !== xs[0])))
}

function removeAllChildren(elem) {
  while (elem.firstChild) {
    elem.removeChild(elem.firstChild)
  }
}

export function visualize(elem, pipes) {
  console.log("IT IS WOKRING")
  removeAllChildren(elem)
  const  svgns = "http://www.w3.org/2000/svg"
  const svg = document.createElementNS(svgns, 'svg')
  elem.appendChild(svg)
  const width = 200
  const height = 100
  const rand = (dim, border) => border + Math.random() * (dim - 2 * border)
  svg.setAttribute('viewBox', `0 0 ${width} ${height}`)

  const queues = dedupe(pipes.flatMap(([_, inQueues, queueName]) => inQueues.map(([queue, _]) => queue).concat(queueName)))
  const border = 5
  queues.forEach((queue) => {
    const node = document.createElementNS(svgns, 'text')
    //const label = document.createTextNode('λ.' + queue)
    const label = document.createTextNode('&' + queue)
    node.setAttribute('x', rand(width, border))
    node.setAttribute('y', rand(height, border))
    node.dataset.queue = queue
    node.style.fontFamily = 'Courier New'
    node.style.fontSize = '5px'
    node.appendChild(label)
    svg.appendChild(node)
  })
  pipes.forEach(([fxn, inQueues, outQueueName]) => {
    const node = document.createElementNS(svgns, 'text')
    const label = document.createTextNode('λ.' + fxn.name)
    node.setAttribute('x', rand(width, border))
    node.setAttribute('y', rand(height, border))
    node.dataset.fxn = fxn.name
    node.style.fontFamily = 'Courier New'
    node.style.fontSize = '5px'
    node.appendChild(label)
    svg.appendChild(node)
  })
}
