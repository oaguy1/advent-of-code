import sys

from typing import List

HELP_TXT = """
    USAGE: solution.py [input_file]
"""


def get_counted(bag_half: str):
    counted = [False] * 26 * 2
    
    for char in bag_half:
        char_num = ord(char)
        
        if char_num >= 97 and char_num <= 122:
            char_idx = char_num - 97
        elif char_num >= 65 and char_num <= 90:
            char_idx = char_num - 65 + 26

        if not counted[char_idx]:
            counted[char_idx] = True

    return counted


def get_letters(counts: List[bool]):
    letters = []
    for idx, bool_value in enumerate(counts):
        if bool_value:
            if idx < 26:
                letters.append(chr(idx+97))
            else:
                letters.append(chr(idx+65))
    
    return letters


def get_priorities(counts: List[int]):
    return [ idx * count for idx, count in enumerate(counts, 1) if count ]


def and_list(list_a: List[bool], list_b: List[bool]):
    return [ x and y for x, y in zip(list_a, list_b) ]


def or_list(list_a: List[bool], list_b: List[bool]):
    return [ x or y for x, y in zip(list_a, list_b) ]


def add_counts(list_a: List[bool], counts: List[int]):
    return [ x + 1 if b else x for b, x in zip(list_a, counts) ]


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(HELP_TXT)
        exit(1)

    global_counted = [0] * 26 * 2
    
    with open(sys.argv[1], 'r') as f:
        for line in f.readlines():
            first_half = line[:len(line)//2]
            second_half = line[len(line)//2:-1]

            counted_1 = get_counted(first_half)
            counted_2 = get_counted(second_half)

            combined_counted = and_list(counted_1, counted_2)
            global_counted = add_counts(combined_counted, global_counted)

    print("Part 1:", sum(get_priorities(global_counted)))
