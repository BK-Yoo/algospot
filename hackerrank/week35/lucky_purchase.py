#!/bin/python3

def is_right_price(price):
    fours = 0
    sevens = 0
    for num in price:
        if num == '4':
            fours += 1
        elif num == '7':
            sevens += 1
        else:
            return False
    return fours == sevens


if __name__ == '__main__':
    n = int(input().strip())
    minimum = ('', pow(10, 9) + 1)
    for _ in range(n):
        s, num = input().strip().split(' ')
        if is_right_price(num):
            num_as_int = int(num)
            if minimum[1] > num_as_int:
                minimum = (s, num_as_int)

    if minimum[0]:
        print(minimum[0])
    else:
        print(-1)
