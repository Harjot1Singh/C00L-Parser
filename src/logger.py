# Simple logging functions with colour support


# Sourced and modified from https://stackoverflow.com/questions/287871/print-in-terminal-with-colors-using-python
class BackgroundColours:
    BLUE = '\033[94m'
    GREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


def info(*string):
    print(*string)


def error(*string):
    print(BackgroundColours.FAIL, *string, BackgroundColours.ENDC)


def success(*string):
    print(BackgroundColours.GREEN, *string, BackgroundColours.ENDC)


def header(*string):
    print(BackgroundColours.BLUE, *string, BackgroundColours.ENDC)

