def solution(prices):
    curr_profit = -1
    curr_min = 10000001
    for price in prices:
        if curr_min > price:
            curr_min = price

        if price - curr_min > curr_profit:
            curr_profit = price - curr_min

    return curr_profit

