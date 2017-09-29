sc = {'%20': ' ', '%21': '!', '%24': '$', '%28': '(', '%29': ')', '%2a': '*', '%25': '%'}
for _ in range(int(input())):
    s = input()
    for k, v in sc.items():
        s = s.replace(k, v)
    print(s)
