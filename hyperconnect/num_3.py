class ReadWriteExecute:
    perm_dict = {'r': 4, 'w': 2, 'x': 1, '-': 0}

    # param perm_string String, UNIX permission string
    # Returns integer, numerical representation of given permissions
    @staticmethod
    def symbolic_to_octal(perm_string):
        per_sum = ''
        for cut in range(0, len(perm_string), 3):
            block = perm_string[cut : cut + 3]
            per_sum += str(sum([ReadWriteExecute.perm_dict[key] for key in block]))
        return int(per_sum)


print(ReadWriteExecute.symbolic_to_octal('rwxr-x-w-'))
