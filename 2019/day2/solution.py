"""
Day 2 of Advent of Code (AoC)
"""
from typing import List

inputs = [1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 9, 1, 19, 1, 9, 19,
          23, 1, 23, 5, 27, 2, 27, 10, 31, 1, 6, 31, 35, 1, 6, 35, 39, 2, 9, 39,
          43, 1, 6, 43, 47, 1, 47, 5, 51, 1, 51, 13, 55, 1, 55, 13, 59, 1, 59,
          5, 63, 2, 63, 6, 67, 1, 5, 67, 71, 1, 71, 13, 75, 1, 10, 75, 79, 2,
          79, 6, 83, 2, 9, 83, 87, 1, 5, 87, 91, 1, 91, 5, 95, 2, 9, 95, 99, 1,
          6, 99, 103, 1, 9, 103, 107, 2, 9, 107, 111, 1, 111, 6, 115, 2, 9, 115,
          119, 1, 119, 6, 123, 1, 123, 9, 127, 2, 127, 13, 131, 1, 131, 9, 135,
          1, 10, 135, 139, 2, 139, 10, 143, 1, 143, 5, 147, 2, 147, 6, 151, 1,
          151, 5, 155, 1, 2, 155, 159, 1, 6, 159, 0, 99, 2, 0, 14, 0]


def cpu(mem: List[int], curr: int):
    """
    A functional representation of the CPU from the AoC Day 2
    """
    if mem[curr] == 99:
        return mem
    elif mem[curr] == 1:
        mem[mem[curr + 3]] = mem[mem[curr + 1]] + mem[mem[curr + 2]]
        return cpu(mem, curr + 4)
    elif mem[curr] == 2:
        mem[mem[curr + 3]] = mem[mem[curr + 1]] * mem[mem[curr + 2]]
        return cpu(mem, curr + 4)
    else:
        return NotImplementedError


print("test1", cpu([1, 0, 0, 0, 99], 0) == [2, 0, 0, 0, 99])
print("test2", cpu([2, 3, 0, 3, 99], 0) == [2, 3, 0, 6, 99])
print("test3", cpu([2, 4, 4, 5, 99, 0], 0) == [2, 4, 4, 5, 99, 9801])
print("test4",
    cpu([1, 1, 1, 4, 99, 5, 6, 0, 99], 0) == [30, 1, 1, 4, 2, 5, 6, 0, 99]
)

"""
Once you have a working computer, the first step is to restore the
gravity assist program (your puzzle input) to the "1202 program alarm"
state it had just before the last computer caught fire. To do this,
before running the program, replace position 1 with the value 12 and 
replace position 2 with the value 2.
"""
inputs1 = inputs.copy()
inputs1[1] = 12
inputs1[2] = 2
print("star1", cpu(inputs1, 0)[0])

""" 
We want to modify the inputs for the program (at addresses 1 and 2) to 
create the output 19690720
"""
inputs2 = inputs.copy()
inputs2[1] = 40
inputs2[2] = 19
run2 = cpu(inputs2, 0)[0]
print("star2", run2)
print("needed:", 19690720 - run2)
