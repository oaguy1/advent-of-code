import sys

from typing import List

HELP_TXT = """
    USAGE: solution.py [input_file]
"""


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(HELP_TXT)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        count_1 = 0
        count_2 = 0
        
        for line in f.readlines():
            elves = line[:-1].split(',')
            hours = [ hours.split('-') for hours in elves ]
            sched = [ [int(start), int(finish)] for start, finish in hours ]
            first_elf, second_elf = sched

            if first_elf[0] >= second_elf[0] and first_elf[1] <= second_elf[1]:
                count_1 += 1
            elif second_elf[0] >= first_elf[0] and second_elf[1] <= first_elf[1]:
                count_1 += 1

            if first_elf[1] >= second_elf[0] and first_elf[0] <= second_elf[1]:
                count_2 += 1
        
        print('PART 1:', count_1)
        print('PART 2:', count_2)
