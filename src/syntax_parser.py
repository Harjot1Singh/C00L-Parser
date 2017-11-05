import tokens


# Iterator wrapper that allows looking ahead at the next item
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

    program(iterator)

    return [], []


def parse_next(predict_dict, token_iter):
    next_token = token_iter.lookahead()

    for predict_set in predict_dict:
        try:
            # Check if the token is in the set
            next(required_token for required_token in predict_set if compare_token(next_token, required_token))

            # Pull next productions as list of expected tokens
            production = predict_dict[predict_set]

            # Do stuff with empty productions
            if production is None:
                token_iter.next()
                return

            for terminal in production:
                if isinstance(terminal, tuple):
                    token = token_iter.next()

                    if not compare_token(token, terminal):
                        print('oh no, expected', terminal, 'got', token)
                else:
                    terminal(token_iter)

            return
        except StopIteration:
            continue

    print('No match found wtf raise an error')


# Compares a token to a required token
# required is a tuple of (tokenType, requiredValue)
def compare_token(token, required):
    required_token_type, required_value = required

    if required_value is None:
        return isinstance(token, required_token_type)
    else:
        return isinstance(token, required_token_type) and token.val() == required_value


def parse_next_decorate(production):
    def wrapper(token_iter):
        return parse_next(production(), token_iter)

    return wrapper


#####################################################################################
# Each of the following productions will be commented with a tabular list, denoting:#
# Production                                                First+ Set              #
#                                                                                   #
# The functions all return a first+ set to production mapping, which the decorator  #
# function then uses to consume and check the tokens                                #
#####################################################################################

# class_def ; programs                      class
@parse_next_decorate
def program():
    return {
        frozenset({(tokens.KeywordToken, 'class')}): [class_def, (tokens.SemiColonToken, None), programs]
    }


# program                                   class
# ε                                         $
@parse_next_decorate
def programs():
    return {
        frozenset({(tokens.KeywordToken, 'class')}): [program],
        frozenset({(tokens.EOFToken, None)}): None
    }


# class TYPE class_detail                   class
@parse_next_decorate
def class_def():
    return {
        frozenset({(tokens.KeywordToken, 'class')}): [(tokens.KeywordToken, 'class'),
                                                      (tokens.TypeIdToken, None),
                                                      class_detail]
    }


# bracket_feature                           {
# inherits TYPE bracket_feature             inherits
@parse_next_decorate
def class_detail():
    return {
        frozenset({(tokens.BracketToken, '{')}): [bracket_feature],
        frozenset({(tokens.KeywordToken, 'inherits')}): [(tokens.KeywordToken, 'inherits'),
                                                         (tokens.TypeIdToken, None),
                                                         bracket_feature]
    }


# { optional_bracket_feature }              {
@parse_next_decorate
def bracket_feature():
    return {
        frozenset({(tokens.BracketToken, '{')}): [(tokens.BracketToken, '{'), optional_bracket_feature]
    }


# class_feature	                            ID
# ε	                                        }
@parse_next_decorate
def optional_bracket_feature():
    return {
        frozenset({(tokens.TypeIdToken, None)}): [class_feature],
        frozenset({(tokens.BracketToken, '}')}): None
    }


# feature ; class_features                  ID
@parse_next_decorate
def class_feature():
    return {
        frozenset({})
    }


# class_feature	                            ID
# ε	                                        }
@parse_next_decorate
def class_features():
    return {}


# ID feature_details                        ID
@parse_next_decorate
def feature():
    return {}


# ( optional_features ) : TYPE { expr }     (
# : TYPE optional_expr                      :
@parse_next_decorate
def feature_details():
    return {}


# feature_formal                            ID
# ε	                                        )
@parse_next_decorate
def optional_features():
    return {}


# <- expr                                   <-
# ε                                         ,, in, ;
@parse_next_decorate
def optional_expression():
    return {}


# formal feature_formals                    ID
@parse_next_decorate
def feature_formal():
    return {}


# , feature_formal	                        ,
# ε                                         )
@parse_next_decorate
def feature_formals():
    return {}


# ID : TYPE                                 ID
@parse_next_decorate
def formal():
    return {}


# if expr then expr else expr fi expr_rr    if
# while expr loop expr pool expr_rr         while
# { semicolon_expr } expr_rr                {
# let id_type_expr in expr expr_rr          let
# case expr of id_type_arrow esac expr_rr   case
# new TYPE expr_rr                          new
# ( expr ) expr_rr                          (
# assign_term expr_rr                       ID, not, isvoid, ~, integer, string, true, false
@parse_next_decorate
def expr():
    return {}


# @TYPE.ID( optional_comma_expr ) expr_rr   @
# . ID ( optional_comma_expr ) expr_rr      .
# ε                                         ;, ,, then, else, fi, loop, pool, @, ., of, ), }, in, *, /, +, -, <=, <, =
@parse_next_decorate
def expr_rr():
    return {}


# formal => expr ; id_type_arrows           ID
@parse_next_decorate
def id_type_arrow():
    return {}


# id_type_arrow                             ID
# ε                                         esac
@parse_next_decorate
def id_type_arrows():
    return {}


# formal optional_expr id_type_exprs        ID
@parse_next_decorate
def id_type_expr():
    return {}


# , id_type_expr                            ,
# ε                                         in
@parse_next_decorate
def id_type_exprs():
    return {}


# comma_expr                        if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
# ε                                 )
@parse_next_decorate
def optional_comma_expr():
    return {}


# expr comma_exprs                  if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
@parse_next_decorate
def comma_expr():
    return {}


# , comma_expr                              ,
# ε                                         )
@parse_next_decorate
def comma_exprs():
    return {}


# expr ; semicolon_exprs            if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
@parse_next_decorate
def semicolon_expr():
    return {}


# semicolon_expr	                if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
# ε                                 }
@parse_next_decorate
def semicolon_exprs():
    return {}


# ID <- not_term                            ID
# not_term                                  not, isvoid, ~, ID, integer, string, true, false
@parse_next_decorate
def assign_term():
    return {}


# not compare_term                          not
# compare_term                              isvoid, ~, ID, integer, string, true, false
@parse_next_decorate
def not_term():
    return {}


# add_term compare_term_rr                  isvoid, ~, ID, integer, string, true, false
@parse_next_decorate
def compare_term():
    return {}


# <= add_term compare_term_rr               <=
# < add_term compare_term_rr                <
# = add_term compare_term_rr                =
# ε                                         @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@parse_next_decorate
def compare_term_rr():
    return {}


# multi_term add_term_rr                    isvoid, ~, ID, integer, string, true, false
@parse_next_decorate
def add_term():
    return {}


# + multi_term add_term_rr                  +
# - multi_term add_term_rr                  -
# ε                                         <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@parse_next_decorate
def add_term_rr():
    return {}


# isvoid_term multi_term_rr                 isvoid, ~, ID, integer, string, true, false
@parse_next_decorate
def multi_term():
    return {}


# * isvoid_term multi_term_rr	            *
# / isvoid_term multi_term_rr	            /
# ε	                                        +, -, <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@parse_next_decorate
def multi_term_rr():
    return {}


# isvoid tilde_term                         isvoid
# tilde_term                                ~, ID, integer, string, true, false
@parse_next_decorate
def isvoid_term():
    return {}


# ~ factor                                  ~
# factor                                    ID, integer, string, true, false
@parse_next_decorate
def tilde_term():
    return {}


# ID factor_id                              ID
# integer	                                integer
# string	                                string
# true	                                    TRUE
# false                                     FALSE
@parse_next_decorate
def factor():
    return {}


# ( optional_comma_expr ) expr_rr           (
# ε                                         *, /, +, -, <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@parse_next_decorate
def factor_id():
    return {}
