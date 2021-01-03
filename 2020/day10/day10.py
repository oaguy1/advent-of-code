import sys


def day10(lst):
    starting_jolts = 0
    curr_jolts = starting_jolts
    
    # add built-in adapter jolts to adapter list
    lst.append(max(lst) + 3)

    diff_1 = []
    diff_3 = []

    while lst:
        next_jolts = min(lst)
        
        if next_jolts - curr_jolts == 1:
            diff_1.append(next_jolts)
        elif next_jolts - curr_jolts == 3:
            diff_3.append(next_jolts)
        else:
            raise ValueError("I wasn't supposed to get here")

        curr_jolts = next_jolts
        lst.remove(curr_jolts) 

    return len(diff_1) * len(diff_3)


if __name__ == '__main__':
    file_name = sys.argv[1]

    with open(file_name) as f:
        raw_lst = f.read()
        str_lst = raw_lst.splitlines()
        lst = list(map(int, str_lst))

    print(day10(lst))