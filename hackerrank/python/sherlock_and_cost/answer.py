def max_diff(arr):
    case_min = 0
    case_max = 0

    for idx in range(0, len(arr) - 1):
        max_to_min = abs(arr[idx] - 1)
        min_to_max = abs(1 - arr[idx+1])
        max_to_max = abs(arr[idx] - arr[idx + 1])

        case_min, case_max = max(case_min, case_max + max_to_min), max(case_min + min_to_max, case_max + max_to_max)

    return max(case_min, case_max)


for run in range(0, int(input())):
    length = int(input())
    init_memory = [[-1] * length] * length
    input_dist = list(map(int, input().split(" ")))
    print(max_diff(input_dist))


# if __name__ == '__main__':
#     with open("./test_case.txt") as f:
#         run = int(f.readline())
#         for _ in range(0, run):
#             length = int(f.readline())
#             init_memory = [[-1] * length] * length
#             input_dist = list(map(int, f.readline().split(" ")))
#             print(max_diff(input_dist))
