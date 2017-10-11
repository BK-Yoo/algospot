t1, t2, n = (map(int, input().split(" ")))
memory = {1: t1, 2: t2}


def get_num(nth):
    if nth not in memory:
        memory[nth] = get_num(nth - 2) + get_num(nth - 1) ** 2
    return memory[nth]


print(get_num(n))
