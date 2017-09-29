def find_power_sum(num, po, limit=0):
    if num < 0:
        return 0

    elif num <= 1:
        return 1

    else:
        comb = 0
        max_possible_element = limit if limit else int(num ** (1. / po))
        for candidate in range(max_possible_element, 1, -1):
            comb += find_power_sum(num - candidate ** po, po, candidate - 1)
        return comb


print(find_power_sum(int(input()), int(input())))
