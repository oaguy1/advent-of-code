import sys


MAX_ITR = 100000

def day08(lst, allow_loop=False):
    pc = 0
    acc = 0
    visited = [False] * len(lst)
    itr = 0

    while pc < len(lst) and not visited[pc] and itr < MAX_ITR:
        opp, arg = lst[pc].split(' ')

        # only marked as visited if we don't allow looping
        if not allow_loop:
            visited[pc] = True

        if opp == 'nop':
            pc += 1
        elif opp == 'acc':
            acc += int(arg)
            pc += 1
        elif opp == 'jmp':
            pc += int(arg)

        itr += 1

    return acc, itr == MAX_ITR

def day08_prime(lst):

    for idx, orig_instruction in enumerate(lst):
        if 'jmp' in orig_instruction:
            new_instruction = orig_instruction.replace('jmp', 'nop')
        elif 'nop' in orig_instruction:
            new_instruction = orig_instruction.replace('nop', 'jmp')
        else:
            continue

        lst[idx] = new_instruction

        result, timedout = day08(lst, True)

        if not timedout:
            return result
        else:
            lst[idx] = orig_instruction
            
    return None

if __name__ == '__main__':
    file_name = sys.argv[1]

    with open(file_name) as f:
        raw_lst = f.read()
        lst = raw_lst.splitlines()

    print(day08(lst)[0])
    print(day08_prime(lst))