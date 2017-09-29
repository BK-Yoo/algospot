import random
from itertools import combinations
from subprocess import Popen, PIPE

itr = [0, 1, 2, 3]
ops = [(1, 0), (-1, 0), (0, 1), (0, -1)]


def i_v_p(map, st, ed, ct, path):
    if st == ed:
        return True
    else:
        for i in itr:
            mv = x, y = st[0] + ops[i][0], st[1] + ops[i][1]
            # TODO tuning point1: mv not in path -> needs big changes
            if w > x >= 0 and h > y >= 0 and map[y][x] == '.' and mv not in path:
                n_path = path + [mv]
                l_c = len(n_path) < 3 or n_path[-3][0] == x or n_path[-3][1] == y

                if ct == 1 and i_v_p(map, mv, ed, ct if l_c else ct + 1, n_path):
                    return True

                elif ct == 2 and (l_c or x == ed[0] or y == ed[1]) and i_v_p(map, mv, ed, ct if l_c else ct + 1,
                                                                             n_path):
                    return True

                elif ct == 3 and l_c and (x == ed[0] or y == ed[1]) and i_v_p(map, mv, ed, ct, n_path):
                    return True

            elif mv == ed:
                if ct <= 2:
                    return True
                elif ct == 3 and (path[-2][0] == x or path[-2][1] == y):
                    return True
                else:
                    return False

            else:
                continue

        return False


def m_complex_row():
    chs = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']
    m_complex = []

    for i in range(50):
        m_complex.append('.')

    for j in range(10):
        ri = random.randint(0, 49)
        m_complex[ri] = chs[ri % len(chs)]

    return m_complex


for _ in range(int(input())):
    h, w = map(int, input().split())
    m = []
    c_p = {}
    for y in range(h):
        m.append([])
        # r = input()
        r = m_complex_row()
        for x in range(len(r)):
            c = r[x]
            m[y].append(c)
            if c != '.':
                if c in c_p:
                    c_p[c].append((x, y))
                else:
                    c_p[c] = [(x, y)]
    dif = 0
    for k in c_p:
        # TODO tuning point2: combinations -> is there any efficient way to find path candidates?
        for st, ed in combinations(c_p[k], 2):
            if i_v_p(m, st, ed, 1, [st]):
                dif += 1
    print(dif)

    p = Popen(['./other'], stdout=PIPE, stdin=PIPE, stderr=PIPE)
    input_str = '1\n'
    input_str += ' '.join([str(h), str(w), '\n'])
    for row in m:
        input_str += ''.join(row) + '\n'

    stdout_data = p.communicate(input=input_str.encode())
    print('c', stdout_data[0].decode())
