import logger
from tokens import *


# Error class for general parse errors
class ParseError(Exception):
    def __init__(self, line, column, message):
        self.message = '[{}:{}] {}'.format(line, column, message)


# Iterator wrapper that allows looking ahead at the next item
class PeekableIterator:
    def __init__(self, lst):
        self._iterator = iter(lst)
        self._lookahead_val = None
        self._current_val = None

    # Looks ahead to the next value in the iterator, without moving it
    def lookahead(self):
        # If there no current lookahead val, store the next value
        if self._lookahead_val is None:
            self._lookahead_val = next(self._iterator)

        return self._lookahead_val

    # Returns the next element of the iterator
    def next(self):
        # Return the lookahead value if lookahead was called, otherwise call next directly
        self._current_val = self._lookahead_val if self._lookahead_val is not None else next(self._iterator)
        self._lookahead_val = None
        return self._current_val

    # Returns the current element
    def current(self):
        return self._current_val


# Entry point for syntax parsing
def parse(token_lst):
    # Setup iterator with lookahead functionality
    iterator = PeekableIterator(token_lst)

    program(iterator)

    return [], []


# Parse and check productions, by choosing the correct
# production using the first+/predict set in `predict_dict`
def parse_next(predict_dict, token_iter, name):
    next_token = token_iter.lookahead()

    logger.header('Entering non-terminal', name)

    for predict_set in predict_dict:
        if None in predict_set:
            continue

        try:
            # Check if lookahead token is in the first+ set, will throw an exception if not
            next(required_token for required_token in predict_set if compare_token(next_token, required_token))

            production = predict_dict[predict_set]
            print('Looking for', next_token)

            # Pull next productions as list of expected tokens
            # Empty productions result in consuming the token and moving on
            if production is None:
                logger.header('Backing out from non-terminal (e)', name)
                return

            # Go through all the possible terminals/non-terminals in the production
            for terminal in production:
                # A tuple means it's a terminal, so consume and compare
                if isinstance(terminal, tuple):

                    token = token_iter.next()
                    logger.success(token, 'consumed')
                    if not compare_token(token, terminal):
                        raise ParseError(token.line, token.column, 'Unexpected token {}, wanted {}'.format(token, terminal))

                # Otherwise it's a non-terminal, so call it
                else:
                    terminal(token_iter)

            logger.header('Backing out from non-terminal', name)
            return
        # No more tokens left in the stream
        except StopIteration:
            continue

    if frozenset({None}) in predict_dict:
        logger.header('Hi, in', name)
        # return

    raise ParseError(next_token.line, next_token.column, 'No matches for {} in non-terminal {}'.format(next_token, name))


# Compares a token to a required token
# required is a tuple of (tokenType, requiredValue)
def compare_token(token, required):
    required_token_type, required_value = required

    if required_value is None:
        return isinstance(token, required_token_type)
    else:
        return isinstance(token, required_token_type) and token.val() == required_value


# Decorator to execute the same functions on each non-terminal production
def nonterminal_decorator(production):
    def wrapper(token_iter):
        try:
            return parse_next(production(), token_iter, production.__name__)
        except ParseError as err:
            logger.error(err.message)

    return wrapper


#####################################################################################
# Each of the following productions will be commented with a tabular list, denoting:#
# Production                                                First+ Set              #
#                                                                                   #
# The functions all return a first+ set to production mapping, which the decorator  #
# function then uses to consume and check the tokens                                #
#####################################################################################

# class_def ; programs                      class
@nonterminal_decorator
def program():
    return {
        frozenset({(KeywordToken, 'class')}): [class_def, (SemiColonToken, None), programs]
    }


# program                                   class
# ε                                         $
@nonterminal_decorator
def programs():
    return {
        frozenset({(KeywordToken, 'class')}): [program],
        frozenset({(EOFToken, None)}): None,
        frozenset({None}): None
    }


# class TYPE class_detail                   class
@nonterminal_decorator
def class_def():
    return {
        frozenset({(KeywordToken, 'class')}): [(KeywordToken, 'class'), (TypeIdToken, None), class_detail]
    }


# bracket_feature                           {
# inherits TYPE bracket_feature             inherits
@nonterminal_decorator
def class_detail():
    return {
        frozenset({(BracketToken, '{')}): [bracket_feature],
        frozenset({(KeywordToken, 'inherits')}): [(KeywordToken, 'inherits'), (TypeIdToken, None), bracket_feature]
    }


# { optional_bracket_feature }              {
@nonterminal_decorator
def bracket_feature():
    return {
        frozenset({(BracketToken, '{')}): [(BracketToken, '{'), optional_bracket_feature, (BracketToken, '}')]
    }


# class_feature	                            ID
# ε	                                        }
@nonterminal_decorator
def optional_bracket_feature():
    return {
        frozenset({(ObjectIdToken, None)}): [class_feature],
        frozenset({(BracketToken, '}')}): None,
        frozenset({None}): None
    }


# feature ; class_features                  ID
@nonterminal_decorator
def class_feature():
    return {
        frozenset({(ObjectIdToken, None)}): [feature, (SemiColonToken, None), class_features]
    }


# class_feature	                            ID
# ε	                                        }
@nonterminal_decorator
def class_features():
    return {
        frozenset({(ObjectIdToken, None)}): [class_feature],
        frozenset({(BracketToken, '}')}): None,
        frozenset({None}): None
    }


# ID feature_details                        ID
@nonterminal_decorator
def feature():
    return {
        frozenset({(ObjectIdToken, None)}): [(ObjectIdToken, None), feature_details]
    }


# ( optional_features ) : TYPE { expr }     (
# : TYPE optional_expr                      :
@nonterminal_decorator
def feature_details():
    return {
        frozenset({(BracketToken, '(')}): [
            (BracketToken, '('),
            optional_features,
            (BracketToken, ')'),
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
@nonterminal_decorator
def optional_features():
    return {
        frozenset({(ObjectIdToken, None)}): [feature_formal],
        frozenset({(BracketToken, ')')}): None,
        frozenset({None}): None
    }


# <- expr                                   <-
# ε                                         ,, in, ;
@nonterminal_decorator
def optional_expr():
    return {
        frozenset({(AssignmentToken, None)}): [(AssignmentToken, None), expr],
        frozenset({(CommaToken, None), (KeywordToken, 'in'), (SemiColonToken, None)}): None,
        frozenset({None}): None
    }


# formal feature_formals                    ID
@nonterminal_decorator
def feature_formal():
    return {
        frozenset({(ObjectIdToken, None)}): [formal, feature_formals]
    }


# , feature_formal	                        ,
# ε                                         )
@nonterminal_decorator
def feature_formals():
    return {
        frozenset({(CommaToken, None)}): [(CommaToken, None), feature_formal],
        frozenset({(BracketToken, ')')}): None,
        frozenset({None}): None
    }


# ID : TYPE                                 ID
@nonterminal_decorator
def formal():
    return {
        frozenset({(ObjectIdToken, None)}): [(ObjectIdToken, None), (ColonToken, None), (TypeIdToken, None)]
    }


# if expr then expr else expr fi expr_rr    if
# while expr loop expr pool expr_rr         while
# { semicolon_expr } expr_rr                {
# let id_type_expr in expr expr_rr          let
# case expr of id_type_arrow esac expr_rr   case
# new TYPE expr_rr                          new
# ( expr ) expr_rr                          (
# assign_term expr_rr                       ID, not, isvoid, ~, integer, string, true, false
@nonterminal_decorator
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
            (BooleanToken, None)}): [not_term, expr_rr]
    }


# @TYPE.ID( optional_comma_expr ) expr_rr   @
# .ID( optional_comma_expr ) expr_rr        .
# ε                                         ;, ,, then, else, fi, loop, pool, @, ., of, ), }, in, *, /, +, -, <=, <, =
@nonterminal_decorator
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
            (BracketToken, '('),
            optional_comma_expr,
            (BracketToken, ')'),
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
            # (DispatchToken, None),
            (KeywordToken, 'of'),
            (BracketToken, ')'),
            (BracketToken, '}'),
            (KeywordToken, 'in'),
            (BinaryOperatorToken, None),
            (ComparatorToken, None)
        }): None,
        frozenset({None}): None
    }


# formal => expr ; id_type_arrows           ID
@nonterminal_decorator
def id_type_arrow():
    return {
        frozenset({(ObjectIdToken, None)}): [formal, (ArrowToken, None), expr, (SemiColonToken, None), id_type_arrows]
    }


# id_type_arrow                             ID
# ε                                         esac
@nonterminal_decorator
def id_type_arrows():
    return {
        frozenset({(ObjectIdToken, None)}): [id_type_arrow],
        frozenset({(KeywordToken, 'esac')}): None,
        frozenset({None}): None
    }


# formal optional_expr id_type_exprs        ID
@nonterminal_decorator
def id_type_expr():
    return {
        frozenset({(ObjectIdToken, None)}): [formal, optional_expr, id_type_exprs]
    }


# , id_type_expr                            ,
# ε                                         in
@nonterminal_decorator
def id_type_exprs():
    return {
        frozenset({(CommaToken, None)}): [(CommaToken, None), id_type_expr],
        frozenset({(KeywordToken, 'in')}): None,
        frozenset({None}): None
    }


# comma_expr                        if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
# ε                                 )
@nonterminal_decorator
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
            (ObjectIdToken, None),
            (UnaryOperatorToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [comma_expr],
        frozenset({(BracketToken, ')')}): None,
        frozenset({None}): None
    }


# expr comma_exprs                  if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
@nonterminal_decorator
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
            (ObjectIdToken, None),
            (UnaryOperatorToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [expr, comma_exprs]
    }


# , comma_expr                              ,
# ε                                         )
@nonterminal_decorator
def comma_exprs():
    return {
        frozenset({(CommaToken, None)}): [(CommaToken, None), comma_expr],
        frozenset({(BracketToken, ')')}): None,
        frozenset({None}): None
    }


# expr ; semicolon_exprs            if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
@nonterminal_decorator
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
            (ObjectIdToken, None),
            (UnaryOperatorToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [expr, (SemiColonToken, None), semicolon_exprs]
    }


# semicolon_expr	                if, while, {, let, case, new, (, ID, not, isvoid, ~, integer, string, true, false
# ε                                 }
@nonterminal_decorator
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
            (ObjectIdToken, None),
            (UnaryOperatorToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [semicolon_expr],
        frozenset({(BracketToken, '}')}): None,
        frozenset({None}): None
    }


# TODO: Prepare to remove?
# ID <- not_term                            ID
# not_term                                  not, isvoid, ~, ID, integer, string, true, false
@nonterminal_decorator
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
@nonterminal_decorator
def not_term():
    return {
        frozenset({(UnaryOperatorToken, 'not')}): [(UnaryOperatorToken, 'not'), expr],
        frozenset({
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [compare_term]
    }


# add_term compare_term_rr                  isvoid, ~, ID, integer, string, true, false
@nonterminal_decorator
def compare_term():
    return {
        frozenset({
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [add_term, compare_term_rr]
    }


# <= add_term compare_term_rr               <=
# < add_term compare_term_rr                <
# = add_term compare_term_rr                =
# ε                                         @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@nonterminal_decorator
def compare_term_rr():
    return {
        frozenset({(ComparatorToken, None)}): [(ComparatorToken, None), expr],
        # frozenset({(ComparatorToken, None)}): [(ComparatorToken, None), add_term, compare_term_rr],
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
        }): None,
        frozenset({None}): None
    }


# multi_term add_term_rr                    isvoid, ~, ID, integer, string, true, false
@nonterminal_decorator
def add_term():
    return {
        frozenset({
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [multi_term, add_term_rr]
    }


# + multi_term add_term_rr                  +
# - multi_term add_term_rr                  -
# ε                                         <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@nonterminal_decorator
def add_term_rr():
    return {
        # frozenset({(BinaryOperatorToken, '+')}): [(BinaryOperatorToken, '+'), multi_term, add_term_rr],
        frozenset({(BinaryOperatorToken, '+')}): [(BinaryOperatorToken, '+'), expr],
        frozenset({(BinaryOperatorToken, '-')}): [(BinaryOperatorToken, '-'), expr],
        # frozenset({(BinaryOperatorToken, '-')}): [(BinaryOperatorToken, '-'), multi_term, add_term_rr],
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
        }): None,
        frozenset({None}): None
    }


# isvoid_term multi_term_rr                 isvoid, ~, ID, integer, string, true, false
@nonterminal_decorator
def multi_term():
    return {
        frozenset({
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): [isvoid_term, multi_term_rr]
    }


# * isvoid_term multi_term_rr	            *
# / isvoid_term multi_term_rr	            /
# ε	                                        +, -, <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@nonterminal_decorator
def multi_term_rr():
    return {
        # frozenset({(BinaryOperatorToken, '*')}): [(BinaryOperatorToken, '*'), isvoid_term, multi_term_rr],
        frozenset({(BinaryOperatorToken, '*')}): [(BinaryOperatorToken, '*'), expr],
        frozenset({(BinaryOperatorToken, '/')}): [(BinaryOperatorToken, '/'), expr],
        # frozenset({(BinaryOperatorToken, '/')}): [(BinaryOperatorToken, '/'), isvoid_term, multi_term_rr],
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
        }): None,
        frozenset({None}): None
    }


# isvoid tilde_term                         isvoid
# tilde_term                                ~, ID, integer, string, true, false
@nonterminal_decorator
def isvoid_term():
    return {
        frozenset({(UnaryOperatorToken, 'isvoid')}): [(UnaryOperatorToken, 'isvoid'), expr],
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
@nonterminal_decorator
def tilde_term():
    return {
        frozenset({(UnaryOperatorToken, '~')}): [(UnaryOperatorToken, '~'), expr],
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
@nonterminal_decorator
def factor():
    return {
        frozenset({(ObjectIdToken, None)}): [(ObjectIdToken, None), factor_id],
        frozenset({(IntegerToken, None)}): [(IntegerToken, None)],
        frozenset({(StringToken, None)}): [(StringToken, None)],
        frozenset({(BooleanToken, None)}): [(BooleanToken, None)],
        # frozenset({None}): [expr]
    }


# ( optional_comma_expr )                   (
# ε                                         *, /, +, -, <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
# TODO: temp
# -> expr                                   ->
@nonterminal_decorator
def factor_id():
    return {
        frozenset({(BracketToken, '(')}): [(BracketToken, '('), optional_comma_expr, (BracketToken, ')')],
        frozenset({(AssignmentToken, None)}): [(AssignmentToken, None), expr],
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
        }): None,
        frozenset({None}): None
    }
