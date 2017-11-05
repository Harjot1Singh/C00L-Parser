from tokens import *


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
                        print('oh no, expected', terminal, 'got', token, 'for production', production)
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
        print(production.__name__)
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
        frozenset({(KeywordToken, 'class')}): [class_def, (SemiColonToken, None), programs]
    }


# program                                   class
# ε                                         $
@parse_next_decorate
def programs():
    return {
        frozenset({(KeywordToken, 'class')}): [program],
        frozenset({(EOFToken, None)}): None
    }


# class TYPE class_detail                   class
@parse_next_decorate
def class_def():
    return {
        frozenset({(KeywordToken, 'class')}): [(KeywordToken, 'class'), (TypeIdToken, None), class_detail]
    }


# bracket_feature                           {
# inherits TYPE bracket_feature             inherits
@parse_next_decorate
def class_detail():
    return {
        frozenset({(BracketToken, '{')}): [bracket_feature],
        frozenset({(KeywordToken, 'inherits')}): [(KeywordToken, 'inherits'), (TypeIdToken, None), bracket_feature]
    }


# { optional_bracket_feature }              {
@parse_next_decorate
def bracket_feature():
    return {
        frozenset({(BracketToken, '{')}): [(BracketToken, '{'), optional_bracket_feature]
    }


# class_feature	                            ID
# ε	                                        }
@parse_next_decorate
def optional_bracket_feature():
    return {
        frozenset({(ObjectIdToken, None)}): [class_feature],
        frozenset({(BracketToken, '}')}): None
    }


# feature ; class_features                  ID
@parse_next_decorate
def class_feature():
    return {
        frozenset({(ObjectIdToken, None)}): [feature, (SemiColonToken, None), class_features]
    }


# class_feature	                            ID
# ε	                                        }
@parse_next_decorate
def class_features():
    return {
        frozenset({(ObjectIdToken, None)}): [class_feature],
        frozenset({(BracketToken, '}')}): None
    }


# ID feature_details                        ID
@parse_next_decorate
def feature():
    return {
        frozenset({(ObjectIdToken, None)}): [(ObjectIdToken, None), feature_details]
    }


# ( optional_features ) : TYPE { expr }     (
# : TYPE optional_expr                      :
@parse_next_decorate
def feature_details():
    return {
        frozenset({(BracketToken, '(')}): [
            (BracketToken, '('),
            optional_features,
            (ColonToken, None),
            (TypeIdToken, None),
            (BracketToken, '{'),
            expr,
            (BracketToken, '}')
        ],
        frozenset({(ColonToken, None)}): [(ColonToken, None), (TypeIdToken, None), optional_expr]
    }


# feature_formal                            ID
# ε	                                        )
@parse_next_decorate
def optional_features():
    return {
        frozenset({(ObjectIdToken, None)}): [feature_formal],
        frozenset({(BracketToken, ')')}): None
    }


# <- expr                                   <-
# ε                                         ,, in, ;
@parse_next_decorate
def optional_expr():
    return {
        frozenset({(AssignmentToken, None)}): [(AssignmentToken, None), expr],
        frozenset({(CommaToken, None), (KeywordToken, 'in'), (SemiColonToken, None)}): None
    }


# formal feature_formals                    ID
@parse_next_decorate
def feature_formal():
    return {
        frozenset({(ObjectIdToken, None)}): [formal, feature_formals]
    }


# , feature_formal	                        ,
# ε                                         )
@parse_next_decorate
def feature_formals():
    return {
        frozenset({(CommaToken, None)}): [(CommaToken, None), feature_formal],
        frozenset({(BracketToken, ')')}): None
    }


# ID : TYPE                                 ID
@parse_next_decorate
def formal():
    return {
        frozenset({(ObjectIdToken, None)}): [(ObjectIdToken, None), (SemiColonToken, None), (TypeIdToken, None)]
    }


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
    return {
        frozenset({(KeywordToken, 'if')}): [
            (KeywordToken, 'if'),
            expr,
            (KeywordToken, 'then'),
            expr,
            (KeywordToken, 'else'),
            expr,
            (KeywordToken, 'fi'),
            expr_rr
        ],
        frozenset({(KeywordToken, 'while')}): [
            (KeywordToken, 'while'),
            expr,
            (KeywordToken, 'loop'),
            expr,
            (KeywordToken, 'pool'),
            expr_rr
        ],
        frozenset({(BracketToken, '{')}): [(BracketToken, '{'), semicolon_expr, (BracketToken, '}'), expr_rr],
        frozenset({(KeywordToken, 'let')}): [(KeywordToken, 'let'), id_type_expr, (KeywordToken, 'in'), expr, expr_rr],
        frozenset({(KeywordToken, 'case')}): [
            (KeywordToken, 'case'),
            expr,
            (KeywordToken, 'of'),
            id_type_arrow,
            (KeywordToken, 'esac'),
            expr_rr
        ],
        frozenset({(KeywordToken, 'new')}): [(KeywordToken, 'new'), (TypeIdToken, None), expr_rr],
        frozenset({(BracketToken, '(')}): [(BracketToken, '('), expr, (BracketToken, ')'), expr_rr],
        frozenset({
            (ObjectIdToken, None),
            (UnaryOperatorToken, None),
            (StringToken, None),
            (IntegerToken, None),
            (BooleanToken, None)}): [assign_term, expr_rr]
    }


# @TYPE.ID( optional_comma_expr ) expr_rr   @
# .ID( optional_comma_expr ) expr_rr        .
# ε                                         ;, ,, then, else, fi, loop, pool, @, ., of, ), }, in, *, /, +, -, <=, <, =
@parse_next_decorate
def expr_rr():
    return {
        frozenset({(DispatchToken, '@')}): [
            (DispatchToken, '@'),
            (TypeIdToken, None),
            (DispatchToken, '.'),
            (ObjectIdToken, None),
            (BracketToken, '('),
            optional_comma_expr,
            (BracketToken, ')'),
            expr_rr
        ],
        frozenset({(DispatchToken, '.')}): [
            (DispatchToken, '.'),
            (ObjectIdToken, None),
            optional_comma_expr,
            expr_rr
        ],
        frozenset({
            (SemiColonToken, None),
            (CommaToken, None),
            (KeywordToken, 'then'),
            (KeywordToken, 'else'),
            (KeywordToken, 'fi'),
            (KeywordToken, 'loop'),
            (KeywordToken, 'pool'),
            (DispatchToken, None),
            (KeywordToken, 'of'),
            (BracketToken, ')'),
            (BracketToken, '}'),
            (KeywordToken, 'in'),
            (BinaryOperatorToken, None),
            (ComparatorToken, None)
        }): None
    }


# formal => expr ; id_type_arrows           ID
@parse_next_decorate
def id_type_arrow():
    return {
        frozenset({(ObjectIdToken, None)}): [formal, (ArrowToken, None), expr, (SemiColonToken, None), id_type_arrows]
    }


# id_type_arrow                             ID
# ε                                         esac
@parse_next_decorate
def id_type_arrows():
    return {
        frozenset({(ObjectIdToken, None)}): [id_type_arrow],
        frozenset({(KeywordToken, 'esac')}): None
    }


# formal optional_expr id_type_exprs        ID
@parse_next_decorate
def id_type_expr():
    return {
        frozenset({(ObjectIdToken, None)}): [formal, optional_expr, id_type_exprs]
    }


# , id_type_expr                            ,
# ε                                         in
@parse_next_decorate
def id_type_exprs():
    return {
        frozenset({(CommaToken, None)}): [(CommaToken, None), id_type_expr],
        frozenset({(KeywordToken, 'in')}): None
    }


# comma_expr                        if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
# ε                                 )
@parse_next_decorate
def optional_comma_expr():
    return {
        frozenset({
            (KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (BracketToken, '{'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '('),
            (UnaryOperatorToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [comma_expr],
        frozenset({(BracketToken, ')')}): None
    }


# expr comma_exprs                  if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
@parse_next_decorate
def comma_expr():
    return {
        frozenset({
            (KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (BracketToken, '{'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '('),
            (UnaryOperatorToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [expr, comma_exprs]
    }


# , comma_expr                              ,
# ε                                         )
@parse_next_decorate
def comma_exprs():
    return {
        frozenset({(CommaToken, None)}): [(CommaToken, None), comma_expr],
        frozenset({(BracketToken, ')')}): None
    }


# expr ; semicolon_exprs            if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
@parse_next_decorate
def semicolon_expr():
    return {
        frozenset({
            (KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (BracketToken, '{'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '('),
            (UnaryOperatorToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [expr, (SemiColonToken, None), semicolon_exprs]
    }


# semicolon_expr	                if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
# ε                                 }
@parse_next_decorate
def semicolon_exprs():
    return {
        frozenset({
            (KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (BracketToken, '{'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '('),
            (UnaryOperatorToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [semicolon_expr],
        frozenset({(BracketToken, '}')}): None
    }


# ID <- not_term                            ID
# not_term                                  not, isvoid, ~, ID, integer, string, true, false
@parse_next_decorate
def assign_term():
    return {
        frozenset({(ObjectIdToken, None)}): [(ObjectIdToken, None), (AssignmentToken, None), not_term],
        frozenset({
            (UnaryOperatorToken, None),
            (ObjectIdToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [not_term]
    }


# not compare_term                          not
# compare_term                              isvoid, ~, ID, integer, string, true, false
@parse_next_decorate
def not_term():
    return {
        frozenset({(UnaryOperatorToken, 'not')}): [(UnaryOperatorToken, 'not'), compare_term],
        frozenset({
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [compare_term]
    }


# add_term compare_term_rr                  isvoid, ~, ID, integer, string, true, false
@parse_next_decorate
def compare_term():
    return {
        frozenset({
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [add_term, compare_term_rr]
    }


# <= add_term compare_term_rr               <=
# < add_term compare_term_rr                <
# = add_term compare_term_rr                =
# ε                                         @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@parse_next_decorate
def compare_term_rr():
    return {
        frozenset({(ComparatorToken, '<=')}): [(ComparatorToken, '<='), compare_term_rr],
        frozenset({(ComparatorToken, '<')}): [(ComparatorToken, '<'), compare_term_rr],
        frozenset({(ComparatorToken, '=')}): [(ComparatorToken, '='), compare_term_rr],
        frozenset({
            (DispatchToken, None),
            (SemiColonToken, None),
            (CommaToken, None),
            (KeywordToken, 'then'),
            (KeywordToken, 'else'),
            (KeywordToken, 'fi'),
            (KeywordToken, 'loop'),
            (KeywordToken, 'pool'),
            (KeywordToken, 'of'),
            (BracketToken, ')'),
            (BracketToken, '}'),
            (KeywordToken, 'in')
        }): None
    }


# multi_term add_term_rr                    isvoid, ~, ID, integer, string, true, false
@parse_next_decorate
def add_term():
    return {
        frozenset({
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [multi_term, add_term_rr]
    }


# + multi_term add_term_rr                  +
# - multi_term add_term_rr                  -
# ε                                         <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@parse_next_decorate
def add_term_rr():
    return {
        frozenset({(BinaryOperatorToken, '+')}): [(BinaryOperatorToken, '+'), multi_term, add_term_rr],
        frozenset({(BinaryOperatorToken, '-')}): [(BinaryOperatorToken, '-'), multi_term, add_term_rr],
        frozenset({
            (ComparatorToken, None),
            (DispatchToken, None),
            (SemiColonToken, None),
            (CommaToken, None),
            (KeywordToken, 'then'),
            (KeywordToken, 'else'),
            (KeywordToken, 'fi'),
            (KeywordToken, 'loop'),
            (KeywordToken, 'pool'),
            (KeywordToken, 'of'),
            (BracketToken, ')'),
            (BracketToken, '}'),
            (KeywordToken, 'in')
        }): None
    }


# isvoid_term multi_term_rr                 isvoid, ~, ID, integer, string, true, false
@parse_next_decorate
def multi_term():
    return {
        frozenset({
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [isvoid_term, multi_term_rr]
    }


# * isvoid_term multi_term_rr	            *
# / isvoid_term multi_term_rr	            /
# ε	                                        +, -, <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@parse_next_decorate
def multi_term_rr():
    return {
        frozenset({(BinaryOperatorToken, '*')}): [(BinaryOperatorToken, '*'), isvoid_term, multi_term_rr],
        frozenset({(BinaryOperatorToken, '/')}): [(BinaryOperatorToken, '/'), isvoid_term, multi_term_rr],
        frozenset({
            (BinaryOperatorToken, '+'),
            (BinaryOperatorToken, '-'),
            (ComparatorToken, None),
            (DispatchToken, None),
            (SemiColonToken, None),
            (CommaToken, None),
            (KeywordToken, 'then'),
            (KeywordToken, 'else'),
            (KeywordToken, 'fi'),
            (KeywordToken, 'loop'),
            (KeywordToken, 'pool'),
            (KeywordToken, 'of'),
            (BracketToken, ')'),
            (BracketToken, '}'),
            (KeywordToken, 'in')
        }): None
    }


# isvoid tilde_term                         isvoid
# tilde_term                                ~, ID, integer, string, true, false
@parse_next_decorate
def isvoid_term():
    return {
        frozenset({(UnaryOperatorToken, 'isvoid')}): [(UnaryOperatorToken, 'isvoid'), tilde_term],
        frozenset({
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [tilde_term]
    }


# ~ factor                                  ~
# factor                                    ID, integer, string, true, false
@parse_next_decorate
def tilde_term():
    return {
        frozenset({(UnaryOperatorToken, '~')}): [(UnaryOperatorToken, '~'), factor],
        frozenset({
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [factor]
    }


# ID factor_id                              ID
# integer	                                integer
# string	                                string
# true	                                    TRUE
# false                                     FALSE
@parse_next_decorate
def factor():
    return {
        frozenset({(ObjectIdToken, None)}): [ObjectIdToken, factor_id],
        frozenset({(IntegerToken, None)}): [(IntegerToken, None)],
        frozenset({(StringToken, None)}): [(StringToken, None)],
        frozenset({(BooleanToken, None)}): [(BooleanToken, None)],
    }


# ( optional_comma_expr ) expr_rr           (
# ε                                         *, /, +, -, <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@parse_next_decorate
def factor_id():
    return {
        frozenset({(BracketToken, ')')}): [(BracketToken, '('), optional_comma_expr, (BracketToken, ')'), expr_rr],
        frozenset({
            (BinaryOperatorToken, None),
            (ComparatorToken, None),
            (DispatchToken, None),
            (SemiColonToken, None),
            (CommaToken, None),
            (KeywordToken, 'then'),
            (KeywordToken, 'else'),
            (KeywordToken, 'fi'),
            (KeywordToken, 'loop'),
            (KeywordToken, 'pool'),
            (KeywordToken, 'of'),
            (BracketToken, ')'),
            (BracketToken, '}'),
            (KeywordToken, 'in'),
        }): None
    }
