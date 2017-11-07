import re


# Returns an ugly case-insensitive regex for the given lowercase string
def create_insensitive_regex(string):
    return ''.join('[{}{}]'.format(char, char.upper()) for char in string)


# Returns a group of case-insensitive matches, given a list of lowercase strings
def create_insensitive_regex_group(strings):
    return '|'.join(r'({})\b'.format(create_insensitive_regex(string)) for string in strings)


# Base token class that stores the value, line number, and column
class BaseToken:
    name = ''
    regex = ''

    def __init__(self, value, line, column):
        self.value = value
        self.line = line
        self.column = column

    # Returns the stored value
    def val(self):
        return self.value

    # Return human-readable token with name, value, and line + column
    def __str__(self):
        return self.__class__.__name__ + '(' + self.val() + ')' + ' [' + str(self.line) + ':' + str(self.column) + ']'


# Token for a boolean value
# true or false with every character but the first being case-insensitive
# (t(r|R)(u|U)(e|E)|f(a|A)(l|L)(s|S)(e|E))
class BooleanToken(BaseToken):
    name = 'bool'
    regex = r'(t{}|f{})'.format(create_insensitive_regex('rue'), create_insensitive_regex('alse'))

    # Case-insensitive
    def val(self):
        return super().val().lower()


# Token for a type identifier
# Letters, digits, underscores, must start with capital letter
# [A-Z][0-9a-zA-Z_]*
class TypeIdToken(BaseToken):
    name = 'type identifier'
    regex = r'[A-Z][0-9a-zA-Z_]*'


# Token for an object identifier
# Letters, digits, underscores, must start with lowercase letter
# [a-z][0-9a-zA-Z_]*
class ObjectIdToken(BaseToken):
    name = 'object identifier'
    regex = r'[a-z][0-9a-zA-Z_]*'


# Token for string
# Anything that's not \0
# Strings are found in double-quotes, but we don't want to capture these
class StringToken(BaseToken):
    name = 'string'
    regex = r'(?!(\0))(?:")(.|\\\n)*?(?:")'


# Token for an integer value
# Groups of characters of 0-9
# [0-9]+
class IntegerToken(BaseToken):
    name = 'integer'
    regex = r'\d+'


# Token to capture any whitespace
# ascii 32, \f \t \v \r
class WhitespaceToken(BaseToken):
    name = 'whitespace'
    regex = r'[ \f\t\v\r]+'


# Token to capture newline tokens
# \n
class NewlineToken(BaseToken):
    name = 'newline'
    regex = r'\n'


# Token for any keywords
# case-insensitive class, else, fi, if, in, inherits, let, loop, pool, then, while, case, esac, new, of
# (?i)(class|inherits|else|fi|if|in|let|loop|pool|then|while|case|esac|new|of)
class KeywordToken(BaseToken):
    name = 'keyword'
    regex = create_insensitive_regex_group(['class', 'inherits', 'else', 'fi', 'if', 'in', 'let', 'loop', 'pool',
                                            'then', 'while', 'case', 'esac', 'new', 'of'])

    # Case-insensitive
    def val(self):
        return super().val().lower()


# Token to match any of the unary operators
# (~|isvoid|not)
class UnaryOperatorToken(BaseToken):
    name = 'unary'
    regex = '(~|{}|{})'.format(create_insensitive_regex('isvoid'), create_insensitive_regex('not'))


# Token to match any of the dispatch tokens
# [\.@]
class DispatchToken(BaseToken):
    name = 'dispatch'
    regex = r'[\.@]'


# Token to match an assignment operation
# <-
class AssignmentToken(BaseToken):
    name = '<-'
    regex = '<-'


# Token that matches any binary operators
# [+\-*/]
class BinaryOperatorToken(BaseToken):
    name = 'binary'
    regex = r'[+\-*/]'


# Token to match any comparators
# (<=|<|=)
class ComparatorToken(BaseToken):
    name = 'comparator'
    regex = r'(<=|<|=)'


# Token for anything that doesn't match
# Match anything that's not whitespace
class UnknownToken(BaseToken):
    name = 'unknown'
    regex = r'\S+'


# Token for any forms of brackets
# [\(\){}]
class BracketToken(BaseToken):
    name = 'bracket'
    regex = r'[\(\){}]'


# Token to match a colon
class ColonToken(BaseToken):
    name = ':'
    regex = r':'


# Token to match a semi-colon
class SemiColonToken(BaseToken):
    name = ';'
    regex = r';'


# Token to match => used in a case statement
class ArrowToken(BaseToken):
    name = '=>'
    regex = r'=>'


# Token to match comma
class CommaToken(BaseToken):
    name = ','
    regex = r','


# Token to match double quotes
class DoubleQuoteToken(BaseToken):
    name = '"'
    regex = r'"'


# Token to represent the end of file
class EOFToken(BaseToken):
    name = 'EOF'
    pass


# List of token classes in order of required regex matching precedence
token_types = [StringToken, BooleanToken, KeywordToken, IntegerToken, UnaryOperatorToken, AssignmentToken,
               BinaryOperatorToken, NewlineToken, ObjectIdToken, TypeIdToken, DispatchToken, ArrowToken,
               BracketToken, ColonToken, SemiColonToken, ComparatorToken, CommaToken, DoubleQuoteToken, WhitespaceToken,
               UnknownToken]


# Converts a string into a list of tokens
def tokenise(string):
    tokens = []     # List of lexed tokens
    errors = []     # Any encountered errors
    line = 1        # Current line number
    line_start = 0  # Line start "index", based on newlines

    # Combine regex into a master regex, with named regular expression groups
    master_regex = '|'.join('(?P<%s>%s)' % (token_type.__name__, token_type.regex) for token_type in token_types)

    # Iterate through all regex matches
    for match in re.finditer(master_regex, string):
        name = match.lastgroup      # Name of token that was matched
        column = match.start() - line_start + 1

        # Get the actual match
        value = match.group(name)

        if name == 'NewlineToken':
            # Bump up the line, and set the start index of the new line
            line += 1
            line_start = match.end()
            # Skip newline tokens
            continue
        elif name == 'WhitespaceToken':
            # Skip whitespace tokens
            continue
        elif name == 'UnknownToken':
            # Counts as a lexing error, report and ignore
            errors.append('[{}:{}] {}'.format(line, column, 'Unrecognised symbol "{}"'.format(value)))
            continue

        # Get the token class from the named match and instantiate it :-(
        token_class = globals()[name]
        token = token_class(value, line, column)

        # Finally, append it
        tokens.append(token)

    # Append the EOF token
    tokens.append(EOFToken('$', line + 1, 0))
    return tokens, errors
