import { _makePipe as makePipe } from "./src/core.js";
// deno repl --unstable --eval-file=src/examples/pipe_demo.js

makePipe("", (a) => (b) => a + b, [["a", 3], ["b", 2]], "c");
var a = new BroadcastChannel("a");
var b = new BroadcastChannel("b");
var c = new BroadcastChannel("c");
c.onmessage = ({ data }) => {
  console.log("SENT TO C", data);
};
//a.postMessage(30)
//b.postMessage(20)
