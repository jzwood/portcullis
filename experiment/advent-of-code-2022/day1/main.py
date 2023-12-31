#!/usr/bin/env python3

# python3 main.js
import sys
from day1 import *

# sys.setrecursionlimit(21000)  # required to prevent stackoverflow b/c python doesn't do tail recursion

file = open('food.txt', 'rb')
food = file.read()
file.close()

res = day1a(list(food))
print(res)
