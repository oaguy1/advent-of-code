import sys


class Passport():
    """
    Class representation of a passport
    """

    def __init__(
        self,
        birth_year=None,
        issue_year=None,
        exp_year=None,
        height=None,
        hair_color=None,
        eye_color=None,
        passport_id=None,
        country_id=None
    ):
        self.birth_year = birth_year
        self.issue_year = issue_year
        self.exp_year = exp_year
        self.height = height
        self.hair_color = hair_color
        self.eye_color = eye_color
        self.passport_id = passport_id
        self.country_id = country_id
    
    def __repr__(self):
        return f"{self.__class__}: {self.__dict__}"

    def is_valid(self):
        return self.birth_year != None \
                and self.issue_year != None \
                and self.exp_year != None \
                and self.height != None \
                and self.hair_color != None \
                and self.eye_color != None \
                and self.passport_id != None

    @staticmethod
    def from_dict(input_dict):
        return Passport(
            input_dict.get('byr', None),
            input_dict.get('iyr', None),
            input_dict.get('eyr', None),
            input_dict.get('hgt', None),
            input_dict.get('hcl', None),
            input_dict.get('ecl', None),
            input_dict.get('pid', None),
            input_dict.get('cid', None)
        )


def day04(lst):
    valid_count = 0
    passport_count = 0

    attrs = []
    for idx, ln in enumerate(lst):
        if ln != '':
            attrs += [ attr.split(':') for attr in ln.split(' ') ]
        
        if ln == '' or idx == len(lst) - 1:
            passport = Passport.from_dict(dict(attrs))
            passport_count += 1
           
            if passport.is_valid():
                valid_count += 1

            attrs = []

    return valid_count


if __name__ == '__main__':
    file_name = sys.argv[1]

    with open(file_name) as f:
        raw_lst = f.read()
        lst = raw_lst.splitlines()

    print(day04(lst))
