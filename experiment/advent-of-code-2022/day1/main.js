#!/usr/bin/env -S deno run --allow-read

/*
 * Deno doesn't do tail-call optimization so this solution results in a stack overflow
 */

// deno run --allow-read ./main.js
import { day1a } from "./day1.js";

const food = await Deno.readFile("food.txt");

const result = day1a(food);

console.info(result);
