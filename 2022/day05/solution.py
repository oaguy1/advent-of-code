import sys

from typing import List

HELP_TXT = """
    USAGE: solution.py [input_file]
"""


def parse_stacks(stacks_input: List[str]):
        # find number of stacks by getting number of the last stack
        col_labels = stacks_input[-1]
        stack_num = int(col_labels[-3])
        
        # make a list of lists
        stacks = []
        for _ in range(0, stack_num):
            stacks.append([])

        # starting from the bottom of each stack...
        for line in reversed(stacks_input[:-1]):
            stack_idx = 0

            # ...give me the char at each column index...
            for char_idx in range(1, len(line[:-1]), 4):

                # ...and append to the current stack if not whitespace
                if line[char_idx] !=  ' ':
                    stacks[stack_idx].append(line[char_idx])
                
                stack_idx += 1

        return stacks


def move_stacks(stacks: List[List[str]], moves: List[str]):
    for line in moves:
        # Parse moves from strings into vars
        split = line[:-1].split(' ')
        move = [ int(split[1]), int(split[3]), int(split[5]) ]
        num_moved, start_col, end_col = move

        # Perform moves as described in part 1
        for _ in range(num_moved):
            crate = stacks[start_col-1].pop()
            stacks[end_col-1].append(crate)

    return stacks


def move_stacks_part_2(stacks: List[List[str]], moves: List[str]):
    for line in moves:
        # Parse moves from strings into vars
        split = line[:-1].split(' ')
        move = [ int(split[1]), int(split[3]), int(split[5]) ]
        num_moved, start_col, end_col = move

        # Perform moves as described in part 2
        crates = stacks[start_col-1][-1*num_moved:]
        stacks[end_col-1] += crates
        for _ in range(num_moved):
            stacks[start_col-1].pop()

    return stacks


def print_stacks(stacks: List[List[str]]):
    for idx, stack in enumerate(stacks, 1):
        print(idx, stack)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(HELP_TXT)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        lines = f.readlines()
        split = lines.index('\n')

        stacks_input = lines[:split]
        moves_input = lines[split+1:]

        stacks = parse_stacks(stacks_input)
        final_stacks = move_stacks(stacks, moves_input)

        msg = ""
        for stack in final_stacks:
            msg += stack[-1]

        print("PART 1:", msg)
        
        stacks = parse_stacks(stacks_input)
        final_stacks2 = move_stacks_part_2(stacks, moves_input)

        msg = ""
        for stack in final_stacks2:
            msg += stack[-1]

        print("PART 2:", msg)
