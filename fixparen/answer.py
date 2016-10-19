b = {'(': ')', '{': '}', '[': ']', '<': '>'}
for _ in range(int(input())):
    i, p_l = map(list, input().split())
    p_r = [b[k] for k in p_l]
    i = [[c] for c in i]
    s = []
    for c in i:
        if s and c[0] in p_r:
            mn = min(p_l.index(s[-1][0]), p_r.index(c[0]))
            s[-1][0], c[0] = p_l[mn], p_r[mn]
            s.pop()
        else:
            s.append(c)
    print(''.join(map(lambda e: e[0], i)))
