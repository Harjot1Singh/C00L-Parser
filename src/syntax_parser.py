import tokens


# Iterator wrapper that allows peeking
class PeekableIterator:
    def __init__(self, lst):
        self.iterator = iter(lst)
        self.lookahead_val = None
        self.current_val = None

    # Looks ahead to the next value in the iterator, without moving it
    def lookahead(self):
        # If there no current lookahead val, store the next value
        if self.lookahead_val is None:
            self.lookahead_val = next(self.iterator)

        return self.lookahead_val

    # Returns the next element of the iterator
    def next(self):
        # Return the lookahead value if lookahead was called, otherwise call next directly
        self.current_val = self.lookahead_val if self.lookahead_val is not None else next(self.iterator)
        self.lookahead_val = None
        return self.current_val

    # Returns the current element
    def current(self):
        return self.current_val


# Entry point for syntax parsing
def parse(token_lst):
    # Setup iterator with lookahead functionality
    iterator = PeekableIterator(token_lst)

    parse_program(iterator)

    return [], []


def parse_program(token_iter):
    token = token_iter.next()

    if isinstance(token, tokens.KeywordToken) and token.val() == 'class':
        parse_program(token_iter)
        token = token_iter.next()

        if isinstance(token, tokens.SemiColonToken):
            parse_program(token_iter)

    else:
        print('not matched')


def parse_class(token_iter):
    token = token_iter.next()
