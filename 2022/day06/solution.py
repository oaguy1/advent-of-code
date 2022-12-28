import sys

from typing import List

HELP_TXT = """
    USAGE: solution.py [input_file]
"""

def find_sop_idx(input_str: str, unique_len: int):
    sop_idx = -1
    for start_idx in range(len(input_str[:-1])-unique_len):
        sub_str = line[start_idx:start_idx+unique_len]
        valid_sop = True
        curr_idx = 0

        while curr_idx <= len(sub_str)-2:
            key = sub_str[curr_idx]
            if key in sub_str[curr_idx+1:]:
                valid_sop = False
                break
            curr_idx += 1

        if valid_sop:
            sop_idx = start_idx
            break
        else:
            continue

    return sop_idx + unique_len


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(HELP_TXT)
        exit(1)

    # sop = Start of Packet
    with open(sys.argv[1], 'r') as f:
        for line in f.readlines():
            print("PART 1:", find_sop_idx(line, 4))
            print("PART 2:", find_sop_idx(line, 14))
