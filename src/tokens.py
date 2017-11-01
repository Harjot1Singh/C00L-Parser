import re


# Returns an ugly case-insensitive regex for the given lowercase string
def create_insensitive_regex(string):
    return ''.join('[%s%s]' % (char, char.upper()) for char in string)


# Returns a group of case-insensitive matches, given a list of lowercase strings
def create_insensitive_regex_group(strings):
    return '|'.join('(%s)' % create_insensitive_regex(string) for string in strings)


# Base token class that stores the value, line number, and column
class BaseToken:
    regex = ''

    def __init__(self, value, line, column):
        self.value = value
        self.line = line
        self.column = column

    # Return human-readable token with name, value, and line + column
    def __str__(self):
        return self.__class__.__name__ + '(' + self.value + ')' + ' [' + str(self.line) + ':' + str(self.column) + ']'


# Token for a boolean value
# true or false with every character but the first being case-insensitive
# (t(r|R)(u|U)(e|E)|f(a|A)(l|L)(s|S)(e|E))
class BooleanToken(BaseToken):
    regex = r'(t%s|f%s)' % (create_insensitive_regex('rue'), create_insensitive_regex('alse'))


# Token for any reserved cool keywords
# (self|SELF_TYPE)
class ReservedToken(BaseToken):
    regex = r'(self|SELF_TYPE)'


# Token for a type identifier
# Letters, digits, underscores, must start with capital letter
# [A-Z][0-9a-zA-Z_]*
class TypeIdToken(BaseToken):
    regex = r'[A-Z][0-9a-zA-Z_]*'


# Token for an object identifier
# Letters, digits, underscores, must start with lowercase letter
# [a-z][0-9a-zA-Z_]*
class ObjectIdToken(BaseToken):
    regex = r'[a-z][0-9a-zA-Z_]*'


# Token for string
# Anything that's not \b, \t, \n, \f
# Strings are found in double-quotes, but we don't want to capture these
class StringToken(BaseToken):
    regex = r'(?!.*(\\b|\\t|\\n|\\f))(?:").*(?:")'


# Token for an integer value
# Groups of characters of 0-9
# [0-9]+
class IntegerToken(BaseToken):
    regex = r'\d+'


# Token to capture any whitespace
# ascii 32, \f \t \v \r
class WhitespaceToken(BaseToken):
    regex = r'( |\f|\t|\v|\r)'


# Token to capture newline tokens
# \n
class NewlineToken(BaseToken):
    regex = r'\n'


# Token for any keywords
# case-insensitive class, else, fi, if, in, inherits, let, loop, pool, then, while, case, esac, new, of
# (?i)(class|inherits|else|fi|if|in|let|loop|pool|then|while|case|esac|new|of)
class KeywordToken(BaseToken):
    regex = create_insensitive_regex_group(['class', 'inherits', 'else', 'fi', 'if', 'in', 'let', 'loop', 'pool', 'then'
                                            'while', 'case', 'esac', 'new', 'of'])


# Token to match any of the unary operators
# (~|isvoid|not)
class UnaryOperatorToken(BaseToken):
    regex = create_insensitive_regex_group(['~', 'isvoid', 'not'])


# Token to match any of the dispatch tokens
# (\.|@)
class DispatchToken(BaseToken):
    regex = r'(\.|@)'


# Token to match an assignment operation
# <-
class AssignmentToken(BaseToken):
    regex = '<-'


# Token that matches any binary operators
# [+\-*/]
class BinaryOperatorToken(BaseToken):
    regex = r'[+\-*/]'


# Token to match any comparators
# (<=|<|=)
class ComparatorToken(BaseToken):
    regex = r'(<=|<|=)'


# Token for anything that doesn't match
# Match anything that's not whitespace
class UnknownToken(BaseToken):
    regex = r'\S'


# Token for any forms of brackets
# [\(\){}]
class BracketToken(BaseToken):
    regex = r'[\(\){}]'


# Token to match a colon
class ColonToken(BaseToken):
    regex = r':'


# Token to match a semi-colon
class SemiColonToken(BaseToken):
    regex = r';'


# Token to match => used in a case statement
class ArrowToken(BaseToken):
    regex = r'=>'


# Token to match comma
class CommaToken(BaseToken):
    regex = r','


# Token to match double quotes
class DoubleQuoteToken(BaseToken):
    regex = r'"'


# List of token classes in order of required regex matching precedence
token_types = [StringToken, BooleanToken, ReservedToken, KeywordToken, IntegerToken, UnaryOperatorToken,
               BinaryOperatorToken, NewlineToken, ObjectIdToken, TypeIdToken, DispatchToken, ComparatorToken,
               AssignmentToken, BracketToken, ColonToken, SemiColonToken, ArrowToken, CommaToken, DoubleQuoteToken,
               WhitespaceToken]


# Converts a string into a list of tokens
def tokenise(string):
    line = 1        # Current line number
    line_start = 0  # Line start "index", based on newlines

    # Combine regex into a master regex, with named regular expression groups
    master_regex = '|'.join('(?P<%s>%s)' % (token_type.__name__, token_type.regex) for token_type in token_types)

    # Iterate through all regex matches
    for match in re.finditer(master_regex, string):
        column = 0
        name = match.lastgroup

        if name == 'NewlineToken':
            # Bump up the line, and set the start index of the new line
            line += 1
            line_start = match.end()
        else:
            column = match.start() - line_start + 1

        # Get the actual match
        value = match.group(name)
        # Instantiate the token class with eval of the named match :-(
        token = eval(name)(value, line, column)
        print(token)



# TODO: Change to ordered dict? Eval is ugly