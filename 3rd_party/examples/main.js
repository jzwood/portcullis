import { removeAllChildren, visualize } from "./viz.js";
import { pipes } from "./todo_app.js";
import { makeGrah } from "../runtime/event_runtime.js";

document.addEventListener("DOMContentLoaded", main);

function main() {
  const viz = document.getElementById("viz");
  visualize(viz, pipes);
  makeGrah(pipes);

  document.addEventListener("&todo", ({ detail: todos }) => {
    const todoElem = document.getElementById("todo");
    removeAllChildren(todoElem);
    Array.from(todos).forEach((todo) => {
      const item = document.createElement("li");
      const label = document.createTextNode(todo.join(''));
      item.appendChild(label);
      todoElem.appendChild(item);
    });
  });

  const initTodo = new CustomEvent("&todo", {
    detail: [],
  });
  document.dispatchEvent(initTodo);
}
