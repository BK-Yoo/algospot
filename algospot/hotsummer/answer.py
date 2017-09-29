for _ in range(int(input())):
    print('YES' if int(input()) >= sum(map(int, input().split())) else 'NO')
