import operator

n_c = {'zero': 0, 'one': 1, 'two': 2, 'three': 3, 'four': 4, 'five': 5, 'six': 6, 'seven': 7, 'eight': 8, 'nine': 9,
       'ten': 10}
n_s = {''.join(sorted(k)): v for k, v in n_c.items()}
op = {'*': operator.mul, '+': operator.add, '-': operator.sub}
for _ in range(int(input())):
    a, o, b, __, c = input().split()
    r = ''.join(sorted(c))
    print('Yes' if r in n_s and op[o](n_c[a], n_c[b]) == n_s[r] else 'No')
