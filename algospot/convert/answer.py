for _ in range(int(input())):
    a = {'kg': (2.2046, 'lb'), 'lb': (0.4536, 'kg'), 'l': (0.2642, 'g'), 'g': (3.7854, 'l')}
    q, u = input().split()
    print(_ + 1, ' '.join(["%0.4f" % (float(q) * a[u][0]), a[u][1]]))
