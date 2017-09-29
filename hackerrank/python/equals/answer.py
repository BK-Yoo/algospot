needed_step_list = {0: 0, 1: 1, 2: 1, 3: 2, 4: 2, 5: 1}


def needed_stop(diff):
    if diff in needed_step_list:
        return needed_step_list[diff]

    else:
        min_step = int(diff / 5) + needed_stop(diff % 5)
        needed_step_list[diff] = min_step
        return min_step


def equalize(everyone, baseline):
    total_step = 0
    for element in everyone:
        total_step += needed_stop(element - baseline)
    return total_step


def start(everyone):
    minimum = min(everyone)
    baselines = set(minimum - step for step in range(0, 5))
    min_step = min([equalize(everyone, baseline) for baseline in baselines])
    return min_step


for run in range(0, int(input())):
    num_of_people = int(input())
    input_dist = list(map(int, input().split(" ")))
    min_step = start(input_dist)
    print(min_step)
