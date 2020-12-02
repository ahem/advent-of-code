#!/usr/bin/env python3
import re
from dataclasses import dataclass

with open('./2.input.txt') as f:
    lines = [x.strip() for x in f.readlines()]

pattern = re.compile(r'^(\d+)-(\d+) (\w): (.*)$')
valid_count = 0

for line in lines:
    [low, high, letter, password] = pattern.match(line).groups()

    if int(low) <= password.count(letter) <= int(high):
        valid_count += 1

print("part 1: ", valid_count)

valid_count = 0
for line in lines:
    [idx1, idx2, letter, password] = pattern.match(line).groups()
    first = password[int(idx1) - 1]
    second = password[int(idx2) - 1]
    if first != second and (first == letter or second == letter):
        valid_count += 1

print("part 2: ", valid_count)
