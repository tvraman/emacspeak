#!/usr/bin/env python
import sys

with open(sys.argv[1], encoding="utf-8") as f:
    words = f.read().lower().split(None)

for i in range(0, len(words) - 1):
    if words[i] == words[i + 1]:
        print(words[i])
