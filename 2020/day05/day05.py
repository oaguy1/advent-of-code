import sys


def parse_pass(boarding_pass):
    # row is first 7 chars, then convert into binary and parse as int
    raw_row = boarding_pass[:7]
    row = int(raw_row.replace('F', '0').replace('B', '1'), 2)

    # col is last 3, then convert into binary and parse as int
    raw_col = boarding_pass[7:]
    col = int(raw_col.replace('L', '0').replace('R', '1'), 2)

    return row * 8 + col

def day05(lst):
    seat_ids = [ parse_pass(boarding_pass) for boarding_pass in lst ]
    return max(seat_ids)

def day05_prime(lst):
    seat_ids = [ parse_pass(boarding_pass) for boarding_pass in lst ]
    seat_ids.sort()
    
    for idx in range(len(seat_ids) - 1):
        left, right = seat_ids[idx], seat_ids[idx+1]
        if right - left > 1:
            return left + 1


if __name__ == '__main__':
    file_name = sys.argv[1]

    with open(file_name) as f:
        raw_lst = f.read()
        lst = raw_lst.splitlines()

    print(day05(lst))
    print(day05_prime(lst))