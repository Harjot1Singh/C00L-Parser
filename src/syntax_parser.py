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


def parse_predict(predict_dict):
    pass


def parse_program(token_iter):
    token = token_iter.next()

    if isinstance(token, tokens.KeywordToken) and token.val() == 'class':
        parse_program(token_iter)
        token = token_iter.next()

        if isinstance(token, tokens.SemiColonToken):
            parse_program(token_iter)

    else:
        print('not matched')


def parse_programs(token_iter):
    pass


def parse_class_def(token_iter):
    pass


def parse_class_detail(token_iter):
    pass


def parse_bracket_feature(token_iter):
    pass


def parse_optional_bracket_feature(token_iter):
    pass


def parse_class_feature(token_iter):
    pass


def parse_class_features(token_iter):
    pass


def parse_feature(token_iter):
    pass


def parse_feature_details(token_iter):
    pass


def parse_optional_features(token_iter):
    pass


def parse_optional_expression(token_iter):
    pass


def parse_feature_formal(token_iter):
    pass


def parse_feature_formals(token_iter):
    pass


def parse_formal(token_iter):
    pass


def parse_expr(token_iter):
    pass


def parse_expr_rr(token_iter):
    pass


def parse_id_type_arrow(token_iter):
    pass


def parse_id_type_arrows(token_iter):
    pass


def parse_id_type_expr(token_iter):
    pass


def parse_id_type_exprs(token_iter):
    pass


def parse_optional_comma_expr(token_iter):
    pass


def parse_comma_expr(token_iter):
    pass


def parse_comma_exprs(token_iter):
    pass


def parse_semicolon_expr(token_iter):
    pass


def parse_semicolon_exprs(token_iter):
    pass


def parse_assign_term(token_iter):
    pass


def parse_not_term(token_iter):
    pass


def parse_compare_term(token_iter):
    pass


def parse_compare_term_rr(token_iter):
    pass


def parse_add_term(token_iter):
    pass


def parse_add_term_rr(token_iter):
    pass


def parse_multi_term(token_iter):
    pass


def parse_multi_term_rr(token_iter):
    pass


def parse_isvoid_term(token_iter):
    pass


def parse_tilde_term(token_iter):
    pass


def parse_factor(token_iter):
    pass


def parse_factor_id(token_iter):
    pass

