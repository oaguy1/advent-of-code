import sys


def occ_count(letter, passwd):
    count = 0
    
    for char in passwd:
        if char == letter:
            count += 1

    return count


def day02(lst):
    valid_count = 0

    for ln in lst:
        raw_counts, raw_letter, passwd = ln.split(' ')
        letter = raw_letter[0]
        min_occ, max_occ = list(map(int, raw_counts.split('-')))

        if min_occ <= occ_count(letter, passwd) <= max_occ:
            valid_count += 1

    return valid_count


def day02_extra(lst):
    valid_count = 0

    for ln in lst:
        raw_indexes, raw_letter, passwd = ln.split(' ')
        letter = raw_letter[0]
        idx1, idx2 = list(map(lambda x: int(x) - 1, raw_indexes.split('-')))

        if (passwd[idx1] == letter or passwd[idx2] == letter) and not (passwd[idx1] == letter and passwd[idx2] == letter):
            valid_count += 1

    return valid_count

        
if __name__ == '__main__':
    file_name = sys.argv[1]

    with open(file_name) as f:
        raw_lst = f.read()
        lst = raw_lst.splitlines()
    
    print(day02(lst))
    print(day02_extra(lst))