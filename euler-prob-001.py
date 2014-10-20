#! /usr/bin/python

# euler-prob-001.py
# author: Ray Wach
# date: 2014-10-02
# info: Python script to find the solution for Project Euler problem 1.

import sys

if len(sys.argv) != 2:
    print 'Usage:', sys.argv[0], '<ceiling>' 
    exit(1)

cap = int(sys.argv[1])

fives = range(5,cap,5)

threes = range(3,cap,3)

common = range(15,cap,15)

result = sum(fives) + sum(threes) - sum(common)

print result
