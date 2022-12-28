import sys

from typing import List

HELP_TXT = """
    USAGE: solution.py [input_file]
"""


class InvalidActionException(BaseException):
    pass


class InvalidCodePath(BaseException):
    pass


class FileNotFoundException(BaseException):
    pass


class FSNode(object):
    
    def __init__(self, name:str, is_dir:bool=True, size:int=-1, parent=None):
        self.name = name
        self.is_dir = is_dir
        self.size = size

        if self.is_dir:
            self.children = []
            self.parent = parent

    def add_file(self, name:str, size:int):
        if not self.is_dir:
            raise InvalidActionException("CANNOT ADD FILE TO A FILE")

        self.children.append(FSNode(name, False, size))

    def add_dir(self, name:str):
        if not self.is_dir:
            raise InvalidActionException("CANNOT ADD DIR TO A FILE")

        self.children.append(FSNode(name, parent=self))

    def print_nodes(self, indent:int=0):
        whitespace = " " * 4 * indent
        if self.is_dir:
            print(f"{whitespace} - {self.name} (dir)")
            for child in self.children:
                child.print_nodes(indent+1)
        else:
            print(f"{whitespace} - {self.name} (file, size={self.size})")


def parse_input(lines: List[str]):
    cmds = []
    lines_len = len(lines)
    curr_idx = 0

    while curr_idx < lines_len:
        # if line is user input
        # import pdb; pdb.set_trace()
        if lines[curr_idx][0] == '$':
            # look ahead until you find the next '$'
            look_ahead_idx = curr_idx + 1
            cmd = [lines[curr_idx][:-1]]
            while look_ahead_idx < lines_len and lines[look_ahead_idx][0] != '$':
                cmd.append(lines[look_ahead_idx][:-1])
                look_ahead_idx += 1

            cmds.append(cmd)
            curr_idx += (look_ahead_idx - curr_idx)
        else:
            raise InvalidCodePath("YOU SHOULD NEVER GET HERE")

    return cmds


def exec_cmd(cmd:List[str], root_fs: FSNode, cwd: FSNode=None):
    cmd_input = cmd[0]
    if len(cmd) > 1:
        cmd_output = cmd[1:]

    shell_input = cmd_input.split(' ')
    program = shell_input[1]
    args = shell_input[2:]

    if program == 'cd':
        if args[0] == '/':
            cwd = root_fs
        elif args[0] == '.':
            # change directory to current directory, noop
            pass
        elif args[0] == '..':
            cwd = cwd.parent
        else:
            dir_found = False
            
            for child in cwd.children:
                if child.name == args[0]:
                    dir_found = True
                    cwd = child

            if not dir_found:
                raise FileNotFoundException(f"CANNOT FIND NODE \"{args[0]}\"")
    elif program == 'ls':
        for output_line in cmd_output:
            size_or_dir, node_name = output_line.split(' ')

            if size_or_dir == "dir":
                cwd.add_dir(node_name)
            else:
                cwd.add_file(node_name, int(size_or_dir))

    return root_fs, cwd


def find_files_by_size(node:FSNode, threshold:int):
    if not node.is_dir:
        return node.size
    else:
        recursive_sum = 0
        curr_dir_size = 0
        for child in node.children:
            child_size = find_files_by_size(child, threshold)
            curr_dir_size += child_size

            if child_size < threshold:
                recursive_sum += child_size
        
        if curr_dir_size > threshold:
            return recursive_sum
        else:
            return curr_dir_size + recursive_sum


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(HELP_TXT)
        exit(1)

    with open(sys.argv[1], 'r') as f: 
        lines = f.readlines()
        cmds = parse_input(lines)

    root_fs = cwd = FSNode('/')

    for cmd in cmds:
        root_fs, cwd = exec_cmd(cmd, root_fs, cwd)

    print("PART 1:", find_files_by_size(root_fs, 100000))
