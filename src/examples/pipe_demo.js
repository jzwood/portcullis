import { _extendPipeline as extendPipeline } from "./src/core.js";
// deno repl --unstable --eval-file=src/examples/pipe_demo.js

extendPipeline((a) => (b) => a + b, ["a", "b"], "c");
var a = new BroadcastChannel("a");
var b = new BroadcastChannel("b");
var c = new BroadcastChannel("c");
c.onmessage = ({ data }) => {
  console.log("SENT TO C", data);
};
//a.postMessage(30)
//b.postMessage(20)
