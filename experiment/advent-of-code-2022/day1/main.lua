#!/usr/bin/env lua

-- lua main.lua
require "day1"

local function read_file(path)
    local file = io.open(path, "rb") -- r read mode and b binary mode
    if not file then return nil end
    local content = file:read "*a" -- *a or *all reads the whole file
    file:close()
    return content
end

local food = read_file("food.txt")

function stringToList(str)
    if str == "" then
        return {}
    else
        return { head = string.byte(string.sub(str, 1, 1)), tail = stringToList(string.sub(str, 2)) }
    end
end

foodList = stringToList(food)
res = day1a(foodList)
print(res)
