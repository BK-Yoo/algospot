da = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday']
mx = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

for _ in range(int(input())):
    m, d, dw = input().split()
    m, d = map(int, (m, d))
    i = da.index(dw)
    r = []
    for h in range(len(da)):
        n_d = d + h - i
        if n_d <= 0:
            n_d += mx[(m - 2) % 12]
        elif n_d > mx[(m - 1) % 12]:
            n_d -= mx[(m - 1) % 12]
        r.append(n_d)
    print(*r)
