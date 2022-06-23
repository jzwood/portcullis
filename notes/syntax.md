
add1 -> Num Num
add1 n = + n 1

\_
|> add1

<!--data Stack = Stack Name Buffer TypeExpr-->
data Queue = Queue Name Buffer TypeExpr
data Pipe = Pipe Func [Queue] Queue

&num1 4 Num
&num2 4 Num
&result 2 Num

|> sum [&num1 &num2] &result

export function main() {
  (() => {
    const num1 = new BroadcastChannel('num1');
    const num2 = new BroadcastChannel('num2');
    const result = new BroadcastChannel('result');
    const num1Queue = [];
    const num2Queue = [];
    const num1Buffer = 3;
    const num2Buffer = 3;
    num1.onmessage = (data) => {
      num1.push(data);
      if (num1.length && num2.length) {
        const a = num1.shift();
        const b = num2.shift();
        result.postMessage(sum(a)(b));
      }
      num1.splice(num1Buffer);
    };
    num2.onmessage = (data) => {
      num2.push(data);
      const a = num1.shift(); // more complicated than this
      const b = num2.shift();
      result.postMessage(sum(a)(b))
    };
  })();
}

|> sum [&num1 &num2] &result

function apply(fxn, [head, ...tail]) {
  if (head == undefined) return fxn
  return apply(fxn(head), tail)
}

function(fxn, inQueueNames, outQueueName) {
  const queueMap = inQueueNames.reduce((acc, queueName) => ({...acc, [queueName]: {queue: new BroadcastChannel(queueName), buffer: []}), {})
  const outQueue = new BroadcastChannel(outQueueName)
  inQueueNames.forEach(queueName => {
    const {queue, buffer} = queueMap[queueName]
    queue.onmessage = (data) => {
      buffer.push(data)
      if (buffers.every(buff => buff.length > 0)) {
        const args = buffers.map(buff => buff.shift())
        const result = apply(fxn, args)
        outQueue.postMessage(result)
      }
    }
  })
}

    const num1 = new BroadcastChannel('num1');
    const num2 = new BroadcastChannel('num2');
    const result = new BroadcastChannel('result');
    const num1Queue = [];
    const num2Queue = [];
    const num1Buffer = 3;
    const num2Buffer = 3;
    num1.onmessage = (data) => {
      num1.push(data);
      if (num1.length && num2.length) {
        const a = num1.shift();
        const b = num2.shift();
        result.postMessage(sum(a)(b));
      }
      num1.splice(num1Buffer);
    };
    num2.onmessage = (data) => {
      num2.push(data);
      const a = num1.shift(); // more complicated than this
      const b = num2.shift();
      result.postMessage(sum(a)(b))
    };
  })();
