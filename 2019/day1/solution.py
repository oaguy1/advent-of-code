"""
Day 1 of Advent of Code (AoC)
"""
from functools import reduce
from math import floor


inputs = [106985, 113927, 107457, 106171, 69124, 59906, 66420, 149336, 73783, 120127, 139486, 108698, 104091, 103032, 108609, 136293, 144735, 55381, 98823, 103981, 140684, 114482, 133925, 111247, 110833, 92252, 87396, 79730, 61395, 82572, 72403, 140763, 57088, 63457, 65523, 50148, 134758, 93447, 85513, 132927, 139159, 141579, 94444, 56997, 137128, 107930, 67607, 108837, 120206, 79441, 99839, 137404, 140502, 67274, 108736, 97302, 76561, 107804, 134306, 52820, 89632, 101473, 65001, 57399, 82858, 60577, 82043, 144783, 101606, 138900, 68246, 118774, 129919, 99394, 80009, 107404, 121503, 119232, 108157, 117965, 112025, 139205, 126336, 143985, 58895, 93020, 136732, 100535, 144090, 134414, 109049, 105714, 111654, 50677, 77622, 53398, 133851, 71166, 115935, 94067]


def calc_fuel_needed(mass: int):
    """
    Given a mass, return the amount of fuel required
    """
    return floor(mass/3) - 2


def add_fuel_fuel(mass: int):
    """
    Recursive function to calculate the fuel needed to carry both a given mass and its fuel
    """
    if calc_fuel_needed(mass) < 0:
        return 0
    else:
        added_fuel = calc_fuel_needed(mass)
        return added_fuel + add_fuel_fuel(added_fuel)


# star 1
base_fuel_requirements = reduce(lambda acc, x: calc_fuel_needed(x) + acc, inputs, 0)
print('star1:', base_fuel_requirements)


# star 2
total_fuel_requirements = reduce(lambda acc, x: add_fuel_fuel(x) + acc, inputs, 0)
print('star2:', total_fuel_requirements) 


print('test1:', add_fuel_fuel(14) == 2)
print('test2:', add_fuel_fuel(1969) == 966)