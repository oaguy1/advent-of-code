import sys

HELP_TXT = """
    USAGE: solution.py [input_file]
"""


ROCK = "rock"
PAPER = "paper"
SCISSORS = "scissors"

moves = {
        ROCK: 1,
        PAPER: 2,
        SCISSORS: 3
        }

my_moves = {
        "X": ROCK,
        "Y": PAPER,
        "Z": SCISSORS
        }

my_moves_round_2 = {
        "X": 0,
        "Y": -1,
        "Z": 1
        }

opp_moves = {
        "A": ROCK,
        "B": PAPER,
        "C": SCISSORS
        }

rules = {
        f"{ROCK}{SCISSORS}": 0,
        f"{ROCK}{PAPER}": 1,
        f"{ROCK}{ROCK}": -1,
        f"{PAPER}{ROCK}": 0,
        f"{PAPER}{SCISSORS}": 1,
        f"{PAPER}{PAPER}": -1,
        f"{SCISSORS}{ROCK}": 1,
        f"{SCISSORS}{PAPER}": 0,
        f"{SCISSORS}{SCISSORS}": -1
        }

rules_round_2 = {
        f"{ROCK}0": SCISSORS,
        f"{ROCK}-1": ROCK,
        f"{ROCK}1": PAPER,
        f"{PAPER}0": ROCK,
        f"{PAPER}-1": PAPER,
        f"{PAPER}1": SCISSORS,
        f"{SCISSORS}0": PAPER,
        f"{SCISSORS}-1": SCISSORS,
        f"{SCISSORS}1": ROCK
        }


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(HELP_TXT)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        score = 0

        for line in f.readlines():
            opp_move, my_move = line[:-1].split(" ")
            move_score = 0

            winner = rules[f"{opp_moves[opp_move]}{my_moves[my_move]}"]
            if winner == 1:
                move_score += 6
            elif winner == -1:
                move_score += 3
                
            move_score +=  moves[my_moves[my_move]]
            score += move_score

        print("PART 1:", score)

    with open(sys.argv[1], 'r') as f:
        score = 0

        for line in f.readlines():
            opp_move, my_result = line[:-1].split(" ")
            move_score = 0

            my_move = rules_round_2[f"{opp_moves[opp_move]}{my_moves_round_2[my_result]}"]
            if my_moves_round_2[my_result] == 1:
                move_score += 6
            elif my_moves_round_2[my_result] == -1:
                move_score += 3
            
            move_score +=  moves[my_move]
            score += move_score

        print("PART 2:", score)
