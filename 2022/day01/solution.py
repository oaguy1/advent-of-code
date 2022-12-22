import sys

HELP_TXT = """
    USAGE: solution.py [input_file]
"""


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(HELP_TXT)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        elves = []
        curr_elf_idx = 0
        curr_elf_calories = 0

        for line in f.readlines():
            if line == '\n':
                elves += [curr_elf_calories]
                curr_elf_calories = 0
            else:
                curr_elf_calories += int(line)

        print("PART 1:", max(elves))

        elves.sort(reverse=True)

        print("PART 2:", sum(elves[:3]))
