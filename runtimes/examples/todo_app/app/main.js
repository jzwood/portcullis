import { pipes } from "../todo_app.js";
import { makeGrah } from "./event_runtime.js";

document.addEventListener("DOMContentLoaded", main);

function main() {
  makeGrah(pipes);
  document.addEventListener("&todo", handleTodoEvents);
  dispatch("&todo", []);
}

// HELPERS

function removeAllChildren(elem) {
  while (elem.firstChild) {
    elem.removeChild(elem.firstChild);
  }
}

function dispatch(name, data) {
  const event = new CustomEvent(name, {
    detail: data,
  });
  document.dispatchEvent(event);
}

function handleTodoEvents({ detail: todos }) {
  const todoElem = document.getElementById("todo");
  removeAllChildren(todoElem);
  Array.from(todos).reverse().forEach((todo) => {
    const del = document.createElement("button");
    const X = document.createTextNode("Delete");
    del.appendChild(X);
    del.style.marginLeft = "1rem";
    del.addEventListener("click", (event) => {
      const task = event.target.previousSibling.textContent.split("");
      dispatch("&done", task);
    });
    const item = document.createElement("li");
    const label = document.createTextNode(todo.join(""));
    item.appendChild(label);
    item.appendChild(del);
    todoElem.appendChild(item);
  });
}
