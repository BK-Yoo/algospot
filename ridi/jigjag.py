def add_tuple(a, b):
    return a[0] + b[0], a[1] + b[1]


def over_range(idx, n):
    return not (1 <= idx[0] <= n) or not (1 <= idx[1] <= n)


def solution(n, r, c):
    next_line = (0, 1)
    next_item = (-1, 1)
    value = 1
    x, y = 1, 1
    for _ in range(n * n):
        if x == r and y == c:
            return value

        move_next_item = x + next_item[0], y + next_item[1]

        if over_range(move_next_item, n):
            if x == n and y == 1:
                next_line = (0, 1)

            x += next_line[0]
            y += next_line[1]
            next_item = next_item[0] * -1, next_item[1] * -1
            next_line = (0, 1) if next_line[0] else (1, 0)

        else:
            x, y = move_next_item

        value += 1

    return value


print(solution(6, 5, 4))
