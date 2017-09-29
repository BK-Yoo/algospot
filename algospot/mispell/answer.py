for _ in range(int(input())):
    p, s = input().split()
    print(_ + 1, s[:int(p) - 1] + s[int(p):])
