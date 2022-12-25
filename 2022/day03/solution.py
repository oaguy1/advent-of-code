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

    global_counted = [0] * 26 * 2

    with open(sys.argv[1], 'r') as f:
        lines = f.readlines()

        while lines:
            elf_1 = get_counted(lines[0])
            elf_2 = get_counted(lines[1])
            elf_3 = get_counted(lines[2])


            combined_counted = and_list(elf_1, elf_2)
            combined_counted = and_list(elf_3, combined_counted)
            global_counted = add_counts(combined_counted, global_counted)

            lines.pop(0)
            lines.pop(0)
            lines.pop(0)

    print("Part 2:", sum(get_priorities(global_counted)))
