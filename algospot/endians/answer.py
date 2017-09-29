import sys
rl = lambda: int(sys.stdin.readline())
for l in range(rl()):
    n = [num for num in '{:08x}'.format(int(rl()))][::-1]
    for i in range(0, len(n)-1, 2):
        n[i], n[i+1] = n[i+1], n[i]
    print(int(''.join(n), 16))

