import sys


class Slot(object):
  def __init__(self, pre, nex, val):
    self.pre = pre
    self.nex = nex
    self.val = val


class LineQueue(object):

  def __init__(self, limit):
    self.head, self.tail = None, None
    self.limit = limit
    self.length = 0

  def _is_full(self):
    return self.length == self.limit

  def _is_empty(self):
    return self.length == 0

  def execute(self, command, *args):
    l_command = command.lower()
    if 'offer' == l_command:
      return self.offer(args[0])

    elif 'take' == l_command:
      return self.take()

    elif 'size' == l_command:
      return self.size()

    else:
      return None

  def offer(self, val):
    if self._is_full():
      return 'false'

    else:
      new_slot = Slot(None, self.head, val)
      self.head = new_slot

      if self._is_empty():
        self.tail = new_slot

      elif self.length == 1:
        self.tail.pre = new_slot

      self.length += 1
      return 'true'

  def take(self):
    if self._is_empty():
      return None

    else:
      tail_val = self.tail.val
      self.tail = self.tail.pre
      if self.tail:
        self.tail.nex = None

      self.length -= 1
      return tail_val

  def size(self):
    return self.length


def main():
  num_of_c, q_len = map(int, sys.stdin.readline().strip().split(' '))
  line_queue = LineQueue(q_len)

  # O(n)
  for _ in range(num_of_c):
    commands = sys.stdin.readline().strip().split(' ')
    result = line_queue.execute(*commands)
    if result is not None:
      print(result)


# n: the number of command
# O(n)
main()