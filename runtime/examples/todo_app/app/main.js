import { pipes } from "../todo_app.js";
import { makeGrah } from "./event_runtime.js";

document.addEventListener("DOMContentLoaded", main);

function removeAllChildren(elem) {
  while (elem.firstChild) {
    elem.removeChild(elem.firstChild);
  }
}

function initTodo() {
  const initTodo = new CustomEvent("&todo", {
    detail: [],
  });
  document.dispatchEvent(initTodo);
}

function initTodoListener() {
  document.addEventListener("&todo", ({ detail: todos }) => {
    const todoElem = document.getElementById("todo");
    removeAllChildren(todoElem);
    Array.from(todos).reverse().forEach((todo) => {
      const del = document.createElement("button");
      const X = document.createTextNode("Delete");
      del.appendChild(X);
      del.style.marginLeft = "1rem";
      del.addEventListener("click", (event) => {
        const note = event.target.previousSibling.textContent;
        const customEvent = new CustomEvent("&done", {
          detail: note.split(""),
        });
        document.dispatchEvent(customEvent);
      });
      const item = document.createElement("li");
      const label = document.createTextNode(todo.join(""));
      item.appendChild(label);
      item.appendChild(del);
      todoElem.appendChild(item);
    });
  });
}

function main() {
  makeGrah(pipes);
  initTodoListener();
  initTodo();
}
