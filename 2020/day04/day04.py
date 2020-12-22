import sys
import re


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

        self._hair_regex = re.compile("^#[0-9a-zA-Z]{6}$")
        self._passport_id_regex = re.compile("^\d{9}$")
    
    def __repr__(self):
        return f"{self.__class__}: {self.__dict__}"

    def is_valid(self):
        return self.birth_year\
                and self.issue_year\
                and self.exp_year\
                and self.height\
                and self.hair_color\
                and self.eye_color\
                and self.passport_id
    
    def is_valid_extended(self):
        return self._valid_year(self.birth_year, 1920, 2002) \
                and self._valid_year(self.issue_year, 2010, 2020) \
                and self._valid_year(self.exp_year, 2020, 2030) \
                and self._valid_height() \
                and self._valid_hair_color() \
                and self._valid_eye_color() \
                and self._valid_passport_id()
    
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

    def _valid_year(self, year, start, end):
        return year and start <= int(year) <= end
    
    def _valid_height(self):
        if not self.height:
            return False

        if 'cm' in self.height:
            height = int(self.height.split("cm")[0])
            return 150 <= height <= 193
        elif 'in' in self.height:
            height = int(self.height.split("in")[0])
            return 59 <= height <= 76
        else:
            return False
    
    def _valid_hair_color(self):
        return self.hair_color and self._hair_regex.match(self.hair_color)
    
    def _valid_eye_color(self):
        return self.eye_color in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']

    def _valid_passport_id(self):
        return self.passport_id and self._passport_id_regex.match(self.passport_id)

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

def day04_prime(lst):
    valid_count = 0
    passport_count = 0

    attrs = []
    for idx, ln in enumerate(lst):
        if ln != '':
            attrs += [ attr.split(':') for attr in ln.split(' ') ]
        
        if ln == '' or idx == len(lst) - 1:
            passport = Passport.from_dict(dict(attrs))
            passport_count += 1
           
            if passport.is_valid_extended():
                valid_count += 1

            attrs = []

    return valid_count


if __name__ == '__main__':
    file_name = sys.argv[1]

    with open(file_name) as f:
        raw_lst = f.read()
        lst = raw_lst.splitlines()

    print(day04(lst))
    print(day04_prime(lst))
