for _ in range(int(input())):
    c = [int(n) for n in input()]
    i_t = (c[3] ^ c[4] ^ c[5] ^ c[6]) << 2 | (c[1] ^ c[2] ^ c[5] ^ c[6]) << 1 | (c[0] ^ c[2] ^ c[4] ^ c[6])
    if i_t: c[i_t - 1] ^= 1
    print(''.join(map(str, [c[2], c[4], c[5], c[6]])))
