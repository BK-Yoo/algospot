from operator import ixor

for _ in range(int(input())):
    a = [0, 0]
    for i in range(3):
        a = list(map(ixor, a, map(int, input().split())))
    print(*a)
