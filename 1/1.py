from itertools import permutations

input = [int(x) for x in open('./input.txt').readlines()]

for (x,y) in permutations(input, 2):
    if x + y == 2020:
        print(x, y, x * y)
        break

for (x,y,z) in permutations(input, 3):
    if x + y + z == 2020:
        print(x, y, z, x * y * z)
        break
