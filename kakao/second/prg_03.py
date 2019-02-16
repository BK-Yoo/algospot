from collections import defaultdict

MAX_D = 1000000


def find_shortest_path(i, j, maps, prev):
  if i == j:
    return 0

  elif j in maps[i]:
    print(f'move {i} to {j}')
    return maps[i][j]

  else:
    short_paths = []
    prev.add(i)
    for next_node in maps[i]:
      if next_node in prev:
        continue
      print(f'move {i} to {next_node}')
      short_paths.append(maps[i][next_node] + find_shortest_path(next_node, j, maps, set(prev)))
    return min(short_paths) if short_paths else MAX_D


def solution(V, road, branch):
  maps = defaultdict(dict)
  for i, j, dist in road:
    maps[i][j] = dist
    maps[j][i] = dist

  answers = {k: 0 for k in range(1, V + 1)}
  for k in range(1, V + 1):
    paths = []
    for br in branch:
      paths.append((find_shortest_path(k, br, maps, set()), br))
    answers[k] = sorted(paths)[0][1]
  return [answers[k] for k in sorted(answers.keys())]
