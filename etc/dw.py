#!/usr/bin/env python
import sys

with open(sys.argv[1], encoding="utf-8") as f:
    words = f.read().lower().split(None)

count = 0
for i in range(0, len(words) - 1):
    if words[i] == words[i + 1]:
        print(words[i])
        count = count + 1
if count > 0:
    print(sys.argv[1], count)
