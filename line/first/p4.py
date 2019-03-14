import sys
from collections import defaultdict


def is_cycle(graph, origin, paths):
  if origin not in graph:
    return False

  cycle = False

  # O(n)
  for next_node in graph[origin]:

    # O(1)
    if next_node in paths:
      cycle = True
    else:
      paths.add(next_node)
      cycle |= is_cycle(graph, next_node, paths)

  return cycle


def find_cycle(graph):
  # O(n)
  for node in graph:
    paths = set()
    if is_cycle(graph, node, paths):
      return True

  return False


def main():
  graph = defaultdict(set)
  num_of_nodes = int(sys.stdin.readline())

  # O(n)
  for _ in range(num_of_nodes):
    input_str = sys.stdin.readline()
    if not input_str:
      break

    fm_node, to_node = map(int, input_str.strip().split(' '))

    # O(1 + n)
    graph[fm_node].add(to_node)

  # O(n^2)
  print('true' if find_cycle(graph) else 'false')


# O(n^2)
main()