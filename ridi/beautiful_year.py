def is_beautiful_year(year):
    num = [False for _ in range(10)]
    for char in str(year):
        num_char = int(char)
        if not num[num_char]:
            num[num_char] = True
        else:
            return False
    return True


def solution(p):
    for year in range(int(p) + 1, 20001):
        if is_beautiful_year(year):
            return year


if __name__ == '__main__':
    input_year = input()
    print(solution(input_year))
