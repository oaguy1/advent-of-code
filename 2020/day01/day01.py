import sys

def day01(lst):
    for idx, elem in enumerate(lst[:-1]):
        for elem2 in lst[idx+1:]:
            if elem + elem2 == 2020:
                return elem * elem2


def day01_extra(lst):
    for idx, elem1 in enumerate(lst[:-2]):
        for elem2 in lst[idx+1:-1]:
            for elem3 in lst[idx+2:]:
                if elem1 + elem2 + elem3 == 2020:
                    return elem1 * elem2 * elem3

if __name__ == '__main__':
    file_name = sys.argv[1]

    with open(file_name) as f:
        raw_lst = f.readlines()
        lst = list(map(int, raw_lst))

    print(day01(lst))
    print(day01_extra(lst))