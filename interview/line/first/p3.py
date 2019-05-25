import sys


def main():
  # O(n)
  sticks = list(map(int, sys.stdin.readline().strip().split(' ')))

  # O(nlog_n)
  sticks.sort()

  # O(n)
  st_set = set(sticks)

  if len(st_set) != len(sticks):
    raise Exception('assumption violated')

  target = int(sys.stdin.readline())

  # total: O(n * 1)
  for stick in sticks:
    pair_num = target - stick

    # O(1)
    if pair_num > 0 and pair_num in st_set:
      print(f'{stick} {pair_num}' if stick < pair_num else f'{pair_num} {stick}')
      return

  print(-1)


# n: the number of sticks
# total complexity: O(3n + nlog_n) -> O(nlog_n)
main()