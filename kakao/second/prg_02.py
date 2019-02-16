def get_v(pos, board):
  x, y = pos
  if 0 <= x < len(board) and 0 <= y < len(board[x]):
    return board[x][y]
  return None


def move(curr_pos, passed, board, jumped):
  passed = set(passed)
  passed.add(curr_pos)
  curr_v = get_v(curr_pos, board)
  i, j = curr_pos

  available_steps = [(i + 1, j), (i - 1, j), (i, j - 1), (i, j + 1)]
  valid_steps = [(pos, get_v(pos, board)) for pos in available_steps
                 if pos not in passed and get_v(pos, board)]

  paths = []
  for next_pos, next_v, in valid_steps:
    if next_v > curr_v:
      paths.append(move(next_pos, passed, board, jumped))
    elif not jumped and next_v < curr_v:
      paths.append(move(next_pos, passed, board, True))
    else:
      paths.append(len(passed))

  return max(paths) if paths else len(passed)


def solution(board):
  answers = []
  for i in range(len(board)):
    for j in range(len(board[i])):
      init_pos = (i, j)
      passed = set()
      answers.append(move(init_pos, passed, board, False))
  return max(answers)