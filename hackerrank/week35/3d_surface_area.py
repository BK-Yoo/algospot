def get_surfaces(blocks, h, w, i, j):
    surfaces = blocks[i][j] * 4 + 2
    if j - 1 >= 0:
        surfaces -= blocks[i][j] if blocks[i][j] <= blocks[i][j - 1] else blocks[i][j - 1]
    if i - 1 >= 0:
        surfaces -= blocks[i][j] if blocks[i][j] <= blocks[i - 1][j] else blocks[i - 1][j]
    if i + 1 < h:
        surfaces -= blocks[i][j] if blocks[i][j] <= blocks[i + 1][j] else blocks[i + 1][j]
    if j + 1 < w:
        surfaces -= blocks[i][j] if blocks[i][j] <= blocks[i][j + 1] else blocks[i][j + 1]
    return surfaces


def surface_area(blocks, h, w):
    areas = 0
    for row in range(h):
        for col in range(w):
            areas += get_surfaces(blocks, h, w, row, col)
    return areas


if __name__ == "__main__":
    height, width = input().strip().split(' ')
    height, width = [int(height), int(width)]
    total_blocks = []
    for _ in range(height):
        row_blocks = [int(block) for block in input().strip().split(' ')]
        total_blocks.append(row_blocks)
    result = surface_area(total_blocks, height, width)
    print(result)
