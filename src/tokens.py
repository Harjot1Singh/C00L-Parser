import re


# Base token class that stores the value, line number, and column
class BaseToken:
    def __init__(self, value, line, column):
        self.value = value
        self.line = line
        self.column = column

    # Return human-readable token with name, value, and line + column
    def __str__(self):
        return self.__class__.__name__ + '(' + self.value + ')' + ' [' + self.line + ':' + self.column + ']'


# Token for a boolean value
# true or false with every character but the first being case-insensitive
# (t(r|R)(u|U)(e|E)|f(a|A)(l|L)(s|S)(e|E))
class BooleanToken(BaseToken):
    pass


# Token for a string value
# Groups of characters of A-Z or a-z
# [a-zA-Z]+
class StringToken(BaseToken):
    pass


# Token for an integer value
# Groups of characters of 0-9
# [0-9]+
class IntegerToken:
    pass


# Dummy token for ignored characters
# Groups of spaces or tabs
# (\t| )+
class SkipToken:
    pass


# Token to capture the newline character
# \n
class NewlineToken:
    pass


# Token for any keywords
# case-insensitive class, else, fi, if, in, inherits, isvoid, let, loop, pool, then, while, case, esac, new, of, not
# (?i)(class|inherits|else|fi|if|in|isvoid|let|loop|pool|then|while|case|esac|new|of|not)
class KeywordToken:
    pass


# Token to regular expression mappings, in order of precedence
tokens = [
    (KeywordToken, r'(?i)(class|inherits|else|fi|if|in|isvoid|let|loop|pool|then|while|case|esac|new|of|not)'),
    (BooleanToken, r'(t(r|R)(u|U)(e|E)|f(a|A)(l|L)(s|S)(e|E))'),
    (IntegerToken, r'\d+'),
    (StringToken, r'[a-zA-Z]+'),
]


# Converts a string into a list of tokens
def tokenise(string):
    # Make sure we record the line and column number for each token
    line = 1
    column = 0


