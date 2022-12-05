#!/usr/bin/env python3

# python3 main.js
import sys
from day1 import *

sys.setrecursionlimit(5000)

file = open('food.txt', 'rb')
food = file.read()
file.close()

res = day1a(list(food))
print(res)
