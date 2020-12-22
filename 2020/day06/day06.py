import sys


def day06(lst):
    total = 0

    ans = []
    for idx, ln in enumerate(lst):
        if ln != '':
            ans += list(ln)
        
        if ln == '' or idx == len(lst) - 1:
            total += len(set(ans))
            ans = []

    return total

def day06_prime(lst):
    total = 0

    ans_count = {}
    group_size = 0
    for idx, ln in enumerate(lst):
        if ln != '':
            group_size += 1
            for item in list(ln):
                if item in ans_count.keys():
                    ans_count[item] += 1
                else:
                    ans_count[item] = 1
        
        if ln == '' or idx == len(lst) - 1:
            total += len([ key for key, value in ans_count.items() if value == group_size ])
            ans_count = {}
            group_size = 0

    return total


if __name__ == '__main__':
    file_name = sys.argv[1]

    with open(file_name) as f:
        raw_lst = f.read()
        lst = raw_lst.splitlines()
    
    print(day06(lst))
    print(day06_prime(lst))