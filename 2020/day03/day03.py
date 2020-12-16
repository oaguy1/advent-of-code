import sys
from functools import reduce


def day03(lst, to_the_right):
    tree_count = 0

    for idx, ln in enumerate(lst[1:]):

        circular_idx = (idx + 1) * to_the_right % len(ln)
        if ln[circular_idx] == '#':
            tree_count += 1

    return tree_count


def day03_prime(lst, to_the_right, down):
    tree_count = 0
    idx1 = to_the_right
    idx2 = down

    while idx2 < len(lst):
        circular_idx = idx1 % len(lst[0])

        if lst[idx2][circular_idx] == '#':
            tree_count += 1
        
        idx2 += down
        idx1 += to_the_right

    return tree_count


def day03_extra(lst, movement):
    trees = [day03_prime(lst, to_the_right, down) for to_the_right, down in movement]
    return reduce(lambda x,y: x*y, trees)


if __name__ == '__main__':
    file_name = sys.argv[1]

    with open(file_name) as f:
        raw_lst = f.read()
        lst = raw_lst.splitlines()

    print(day03(lst, 3))
    print(day03_extra(lst, [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]))