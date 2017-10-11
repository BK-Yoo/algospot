def input_to_list():
    return list(map(int, input().split(" ")))


def divide_edge(start, h, w):
    edge = [start]
    for i in range(1, h):
        edge.append((start[0] + i, start[1]))
    left_end = edge[-1]
    for i in range(1, w):
        edge.append((left_end[0], left_end[1] + i))
    edge_end = edge[-1]
    for i in range(1, h):
        edge.append((edge_end[0] - i, edge_end[1]))
    right_end = edge[-1]
    for i in range(1, w - 1):
        edge.append((right_end[0], right_end[1] - i))
    return edge


def get_edge_group(h, w):
    edge_group = []
    origin = (0, 0)
    for _ in range(1, max(h, w)):
        edge = divide_edge(origin, h, w)
        edge_group.append(edge)
        origin = (origin[0] + 1, origin[1] + 1)
        if origin in edge:
            break
        else:
            h, w = h - 2, w - 2
    return edge_group


def print_mat(matrix):
    for row in matrix:
        print(' '.join(map(str, row)))


def rotate_edge(matrix, edge, rot):
    actual = rot % len(edge)
    before = [matrix[h][w] for h, w in edge]
    after = [before[idx] for idx in range(-actual, len(edge) - actual)]
    for (h, w), idx in zip(edge, range(0, len(after))):
        matrix[h][w] = after[idx]


def rotate_matrix(matrix, rot):
    edge_group = get_edge_group(m, n)
    for edge in edge_group:
        rotate_edge(matrix, edge, rot)
    print_mat(matrix)


if __name__ == '__main__':
    m, n, rot = input_to_list()
    mat = [input_to_list() for _ in range(0, m)]
    rotate_matrix(mat, rot)

# if __name__ == '__main__':
#     with open('./test_case.txt') as f:
#         m, n, rot = list(map(int, f.readline().split(" ")))
#         mat = [list(map(int, f.readline().split(" "))) for _ in range(0, m)]
#         rotate_matrix(mat, rot)
