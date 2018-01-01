#!/bin/python3


def explore(matrix, i, j, k, *start):
    if 0 <= i < len(matrix) and 0 <= j < len(matrix[0]):
        if matrix[i][j] is None:
            if i == j:
                matrix[i][j] = start[0] if start else matrix[i - 1][j - 1] + k
                explore(matrix, i + 1, j + 1, k)
            elif i > j:
                matrix[i][j] = matrix[i - 1][j] - 1
            else:
                matrix[i][j] = matrix[i][j - 1] - 1
            explore(matrix, i + 1, j, k)
            explore(matrix, i, j + 1, k)


def triple_recursion(n, m, k):
    matrix = [[None for _ in range(n)] for _ in range(n)]
    explore(matrix, 0, 0, k, m)
    for row in matrix:
        print(' '.join(map(str, row)))


if __name__ == "__main__":
    triple_recursion(*list(map(int, input().strip().split(' '))))
