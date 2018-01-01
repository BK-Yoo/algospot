divide_by = 1000000009


def highway_construct(n, k):
    modulo = 0
    for num in range(2, n):
        modulo += pow(num, k, divide_by)
    return modulo % divide_by


if __name__ == "__main__":
    q = int(input().strip())
    for _ in range(q):
        n, k = input().strip().split(' ')
        n, k = [int(n), int(k)]
        print(highway_construct(n, k))

# if __name__ == "__main__":
#     with open("testcase.txt") as f:
#         with open("answer.txt") as f_a:
#             q = int(f.readline())
#             for _ in range(q):
#                 n, k = f.readline().strip().split(' ')
#                 n, k = [int(n), int(k)]
#                 result = highway_construct(n, k)
#                 answer = int(f_a.readline())
#                 print(result, answer)
#                 assert result == answer
