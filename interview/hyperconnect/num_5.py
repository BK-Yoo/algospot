import string

conv_dict = {}
for i in range(len(string.ascii_uppercase)):
    conv_dict[i+1] = string.ascii_uppercase[i]

class NumbersToText:
    @staticmethod
    def numbers_to_letters(s):
        num_letters = s.split('+')
        letters = []
        for num_letter in num_letters:
            letter = ''.join([conv_dict[int(k)] for k in num_letter.split()])
            letters.append(letter)
        return ' '.join(letters)


print(NumbersToText.numbers_to_letters('20 5 19 20+4 15 13 5'))
