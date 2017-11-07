# Simple logging functions with colour support
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
    print(BackgroundColours.FAIL, end='')
    print(*string)
    print(BackgroundColours.ENDC, end='')


def success(*string):
    print(BackgroundColours.GREEN, end='')
    print(*string)
    print(BackgroundColours.ENDC, end='')


def header(*string):
    print(BackgroundColours.BLUE, end='')
    print(*string)
    print(BackgroundColours.ENDC, end='')


# Method to dump a class and its attributes recursively
def dump(obj, level=0):
    for attr in vars(obj):
        val = getattr(obj, attr)

        if isinstance(val, (int, float, str)):
            info(level * ' ', attr, '->', val)

        elif isinstance(val, list):
            for v in val:
                if isinstance(v, (int, float, str)):
                    info(level * ' ', attr, '->', val)
                else:
                    dump(v, level + 1)
        else:
            print(level * ' ', attr, '->')
