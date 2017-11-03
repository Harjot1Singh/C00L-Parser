import tokens


# Iterator wrapper that allows peeking
class PeekableIterator:
    def __init__(self, lst):
        self.iterator = iter(lst)
        self.lookahead_val = None

    # Looks ahead to the next value in the iterator, without moving it
    def lookahead(self):
        # If there no current lookahead val, store the next value
        if self.lookahead_val is None:
            self.lookahead_val = next(self.iterator)

        return self.lookahead_val

    # Returns the next element of the iterator
    def next(self):
        # Return the lookahead value if lookahead was called, otherwise call next directly
        val = self.lookahead_val if self.lookahead_val is not None else next(self.iterator)
        self.lookahead_val = None
        return val


# Entry point for syntax parsing
def parse(tokens):
    # Setup iterator with lookahead functionality
    iterator = PeekableIterator(tokens)

    parse_program(iterator)

    return [], []


def parse_program(token_iter):
    first = token_iter.next()
    second = token_iter.lookahead()

    if first is tokens.KeywordToken and