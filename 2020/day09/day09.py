import sys
from functools import reduce


def is_valid(key, buffer):
    for num in buffer:
        clean_buffer = list(buffer)
        clean_buffer.remove(num)
        
        if key-num in clean_buffer:
            return True

    return False

def day09(lst, buffer_size):
    buffer_range = range(len(lst)-buffer_size)
    data_range = range(buffer_size, len(lst))

    for buffer_start_idx, data_idx in zip(buffer_range, data_range):
        curr_buffer = lst[buffer_start_idx:buffer_start_idx+buffer_size]
        curr_num = lst[data_idx]
        
        if not is_valid(curr_num, curr_buffer):
            return curr_num

def day09_prime(lst, buffer_size):
    missing_num = day09(lst, buffer_size)
    
    for start_idx in range(len(lst)):
        for end_idx in range(start_idx+2, len(lst)):
            if reduce(lambda x,y: x+y, lst[start_idx:end_idx]) == missing_num:
                smallest = min(lst[start_idx:end_idx])
                largest = max(lst[start_idx:end_idx])
                return smallest + largest


if __name__ == '__main__':
    file_name = sys.argv[1]
    buffer_size = int(sys.argv[2]) if len(sys.argv) > 2 else 25

    with open(file_name) as f:
        raw_lst = f.read()
        str_lst = raw_lst.splitlines()
        lst = list(map(int, str_lst))


    print(day09(lst, buffer_size))
    print(day09_prime(lst, buffer_size))