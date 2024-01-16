import { readFileSync } from "fs";
import { day1a } from "./day1.js";

const food = readFileSync("food.txt");

const result = day1a(food);

console.info(result);
