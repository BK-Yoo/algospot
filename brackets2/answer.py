b = {')': '(', '}': '{', ']': '['}

for _ in range(int(input())):
    s = []
    for c in input():
        if s and c in b and s[-1] == b[c]:
            s.pop()
        else:
            s.append(c)
    print('NO' if s else 'YES')
