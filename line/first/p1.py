import sys


def getArea(width, height) -> int:
  # O(1)
  return width * height


def main():
  width, height = map(int, sys.stdin.readline().strip().split(' '))
  print(getArea(width, height))


# O(1)
main()