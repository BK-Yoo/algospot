import re

for _ in range(int(input())):
    print(''.join(sorted(re.findall('..', input()))))
