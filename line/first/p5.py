import sys
from collections import deque


def main():
  w_len_limit = int(sys.stdin.readline())

  q = deque(maxlen=w_len_limit)

  current_max = -3000000001
  current_max_pos = -1
  current_pos = -1

  while True:
    try:
      num = int(sys.stdin.readline())
      current_pos += 1
      q.appendleft(num)

      # O(W)
      if len(q) == w_len_limit and current_max_pos < current_pos - w_len_limit + 1:
        current_max = -3000000001

        for i, e in enumerate(q):
          if current_max <= e:
            current_max = e
            current_max_pos = (current_pos - len(q) + 1) + i

      elif num >= current_max:
        current_max = num
        current_max_pos = current_pos

      if len(q) == w_len_limit:
        print(current_max)

    except Exception:
      break


# O(W * S)
main()