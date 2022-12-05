#!/usr/bin/env -S deno run --allow-read

// deno run --allow-read ./main.js
import { day1a } from "./day1.js";

const food = await Deno.readFile("food.txt");

const result = day1a(food);

console.info(result);
