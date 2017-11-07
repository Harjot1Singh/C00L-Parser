import sys


# Simple logging functions with colour support
class BackgroundColours:
    BLUE = '\033[94m'
    GREEN = '\033[92m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'


def info(*string, end='\n'):
    sys.stdout.write(' '.join(string))
    sys.stdout.write(end)


def error(*string, end='\n'):
    sys.stderr.write(BackgroundColours.FAIL)
    sys.stderr.write(' '.join(string))
    sys.stderr.write(BackgroundColours.ENDC)
    sys.stderr.write(end)


def success(*string, end='\n'):
    sys.stdout.write(BackgroundColours.GREEN)
    sys.stdout.write(' '.join(string))
    sys.stdout.write(BackgroundColours.ENDC)
    sys.stdout.write(end)


def header(*string, end='\n'):
    sys.stdout.write(BackgroundColours.BLUE)
    sys.stdout.write(' '.join(string))
    sys.stdout.write(BackgroundColours.ENDC)
    sys.stdout.write(end)


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
