def is_n_w_from_mine(dif, divs):
    if dif == 0 or dif in divs:
        return True
    else:
        n_divs = [n for n in divs if n < dif]
        for i in range(len(n_divs)):
            if is_n_w_from_mine(dif - n_divs[i], n_divs[0:i] + n_divs[i + 1:]):
                return True
    return False


def is_n_w(dif, div, lm):
    if dif == 0:
        return True
    else:
        for i in div:
            if i <= dif and i < lm and is_n_w(dif - i, div, i):
                return True
    return False


for _ in range(int(input())):
    t = int(input())
    div = [n for n in range(int(t / 2), 0, -1) if t % n == 0]
    dif = sum(div) - t
    print('not weird' if dif < 0 or is_n_w(dif, div, t) else 'weird')
