import sys
import re

from functools import reduce

def parse_rules(lst):
    restrictions_regex = re.compile(r"(\w+\s+\w+)\s+bag")
    rules = {}
    for rule in lst:
        raw_subject, raw_restrictions = rule.split("contain")
        subject = " ".join(raw_subject.split(" ")[:2])
        
        restrictions = []
        for match in restrictions_regex.finditer(raw_restrictions):
            if match.group(1) == "no other":
                continue
            restrictions.append(match.group(1))

        rules[subject] = restrictions

    return rules

def parse_rules_with_counts(lst):
    restrictions_regex = re.compile(r"(\d+)\s+(\w+\s+\w+)\s+bag")
    rules = {}
    for rule in lst:
        raw_subject, raw_restrictions = rule.split("contain")
        subject = " ".join(raw_subject.split(" ")[:2])
        
        restrictions = []
        for match in restrictions_regex.finditer(raw_restrictions):
            #import pdb; pdb.set_trace()
            if match.group(2) == "no other":
                continue
            restrictions.append((int(match.group(1)), match.group(2)))

        rules[subject] = restrictions

    return rules

def day07(lst):
    rule_dict = parse_rules(lst)
    can_contain_shiny_gold = []

    curr = ['shiny gold']
    can_contain = []

    while curr:
        for curr_bag_type in curr:
            can_contain += [ bag_type for bag_type, restrictions in rule_dict.items() if curr_bag_type in restrictions ]

        can_contain_shiny_gold += can_contain

        curr = set(can_contain)
        can_contain = []

    return len(set(can_contain_shiny_gold))

def day07_prime(lst):
    rule_dict = parse_rules_with_counts(lst)
    return total_tree_nodes(rule_dict, 'shiny gold')

def total_tree_nodes(rule_dict, curr_rule):
    if not rule_dict[curr_rule]:
        return 0
    else:
        results = [
            count + (count * total_tree_nodes(rule_dict, bag_type))
            for count, bag_type
            in rule_dict[curr_rule]
        ]

        return reduce(lambda x, y: x + y, results)
        

if __name__ == '__main__':
    file_name = sys.argv[1]

    with open(file_name) as f:
        raw_lst = f.read()
        lst = raw_lst.splitlines()

    print(day07(lst))
    print(day07_prime(lst))