from tokens import *
from ast import *
import logger


# Error class for general parse errors
class ParseError(Exception):
    def __init__(self, token, expected_token, expected_value, message):
        self.message = '[{}:{}] {}'.format(token.line, token.column, message)
        self.token = token
        self.expected_token = expected_token
        self.expected_value = expected_value


# Iterator wrapper that allows looking ahead at the next item
class LookaheadIterator:
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


# ~ May the lord forgive my use of globals ~
errors = []     # Errors encountered


# Entry point for syntax parsing
def parse(token_lst):
    # Setup iterator with lookahead functionality
    iterator = LookaheadIterator(token_lst)

    ast = program(iterator)

    return ast, errors


# Parse and check productions, by choosing the correct production using the first+/predict set in `predict_dict`
def parse_next(predict_dict, token_iter, name):
    # global _panic

    next_token = token_iter.lookahead()

    # logger.info('Looking for {}'.format(next_token))
    # Grab the production that corresponds to whenever the token is in a certain predict set
    production_tuple = production_from_predict_dict(predict_dict, next_token)

    # If the token is in none of the first+/predict sets for any production in the dict, error
    if production_tuple is False:
        raise ParseError(next_token, None, None,
                         'No matches for "{}" in non-terminal "{}"'.format(next_token.val(), name))

    # Empty productions result in moving on, and exiting the production
    elif production_tuple is None:
        # logger.header('Backing out from non-terminal (e)', name)
        return []

    # Pull out next production as a list of terminals and non-terminals
    production, func = production_tuple

    # logger.header('Entering non-terminal', name)

    tree = []
    for terminal in production:
        try:
            tree.append(parse_terminal(terminal, token_iter))
        except ParseError as err:
            handle_error(err, token_iter, predict_dict, production, terminal)

    # If there are errors, stop building the AST, it's probably gonna be broke :(
    return func(tree) if not errors else []


# Given a predict dictionary, and a token, determine which production to use, or None if there is no match
def production_from_predict_dict(predict_dict, token):
    # Try all the predict sets for the production/non-terminal
    for predict_set in predict_dict:
        if token_in_set(token, predict_set):
            return predict_dict[predict_set]

    # No match :(
    return False


# Returns boolean for whether the given token exists in the predict set
def token_in_set(token, predict_set):
    try:
        next(required_token for required_token in predict_set if compare_token(token, required_token))
        return True

    except StopIteration:
        return False


# Go a possible terminals/non-terminal, from a production
def parse_terminal(terminal, token_iter):
    # A tuple means it's a terminal, so consume and compare
    if isinstance(terminal, tuple):
        token = token_iter.lookahead()

        # If the token is as expected, move forward
        if compare_token(token, terminal):
            token_iter.next()

        # Otherwise, raise your hands in the air ¯\_(-_-)_/¯
        else:
            terminal_type, terminal_value = terminal

            raise ParseError(token, terminal_type.name, terminal_value, 'Unexpected symbol "{}", expected "{}"'.format(
                token.val(),
                terminal_value or terminal_type.name))

        return token.val()

    # Otherwise it's a non-terminal, so call it
    else:
        return terminal(token_iter)


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
            #TODO remove __name__
            return parse_next(production(), token_iter, production.__name__)
        except ParseError as err:
            handle_error(err, token_iter, production())

    return wrapper


# 3 cases when you know the production, or no production with a predict_dict size of 1
# missing token - try next terminal in production with current token
# extra token - try current terminal production with next token
# malformed current token - try next production with next token

# When you're not in production/terminal = None
# Panic, keep discarding tokens until the production works

# Try each of these strategies?

#

def handle_error(err, token_iter, predict_dict, production=None, terminal=None):
    print('entering', err)
    errors.append(err.message)
    next_token = token_iter.lookahead()

    # If the next token is the end of file,
    if isinstance(next_token, EOFToken):
        return

    panic_synchronise(token_iter, predict_dict)

    print('matched again ' + str(token_iter.lookahead()))
    parse_next(predict_dict, token_iter, '')

    # # Not in any particular production
    # while panic:
    #     next_token = token_iter.lookahead()
    #     print('trying', next_token)
    #
    #     production_tuple = production_from_predict_dict(predict_dict, next_token)
    #     if production_tuple is False:
    #         token_iter.next()
    #     else:
    #         print('matched again ' + str(token_iter.lookahead()))
    #         panic = False
    #         parse_next(predict_dict, token_iter, '')


# Throws away tokens until back in sync with production
def panic_synchronise(token_iter, predict_dict):
    while production_from_predict_dict(predict_dict, token_iter.lookahead()) is False:
        next_token = token_iter.next()

        # If the next token is the end of file,
        if isinstance(next_token, EOFToken):
            return

    logger.success('matched again ' + str(token_iter.lookahead()))

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
        frozenset({(KeywordToken, 'class')}): ([class_def, (SemiColonToken, None), programs],
                                               lambda x: Program([x[0], *x[2]]))
    }


# program                                   class
# ε                                         $
@nonterminal_decorator
def programs():
    return {
        frozenset({(KeywordToken, 'class')}): ([program], lambda x: x[0]),
        frozenset({(EOFToken, None)}): None
    }


# class TYPE class_detail                   class
@nonterminal_decorator
def class_def():
    return {
        frozenset({(KeywordToken, 'class')}): (
            [(KeywordToken, 'class'), (TypeIdToken, None), class_detail], lambda x: Class(x[1], *x[2]))
    }


# bracket_feature                           {
# inherits TYPE bracket_feature             inherits
@nonterminal_decorator
def class_detail():
    return {
        frozenset({(BracketToken, '{')}): ([bracket_feature], lambda x: [None, x[0]]),
        frozenset({(KeywordToken, 'inherits')}): ([(KeywordToken, 'inherits'), (TypeIdToken, None), bracket_feature],
                                                  lambda x: [x[1], x[2]])
    }


# { optional_bracket_feature }              {
@nonterminal_decorator
def bracket_feature():
    return {
        frozenset({(BracketToken, '{')}): ([(BracketToken, '{'), optional_bracket_feature, (BracketToken, '}')],
                                           lambda x: x[1])
    }


# class_feature	                            ID
# ε	                                        }
@nonterminal_decorator
def optional_bracket_feature():
    return {
        frozenset({(ObjectIdToken, None)}): ([class_feature], lambda x: x[0]),
        frozenset({(BracketToken, '}')}): None
    }


# feature ; class_features                  ID
@nonterminal_decorator
def class_feature():
    return {
        frozenset({(ObjectIdToken, None)}): ([feature, (SemiColonToken, None), class_features], lambda x: [x[0], *x[2]])
    }


# class_feature	                            ID
# ε	                                        }
@nonterminal_decorator
def class_features():
    return {
        frozenset({(ObjectIdToken, None)}): ([class_feature], lambda x: x[0]),
        frozenset({(BracketToken, '}')}): None
    }


# ID feature_details                        ID
@nonterminal_decorator
def feature():
    return {
        frozenset({(ObjectIdToken, None)}): ([(ObjectIdToken, None), feature_details], lambda x: Feature(x[0], *x[1]))
    }


# ( optional_features ) : TYPE { expr }     (
# : TYPE optional_expr                      :
@nonterminal_decorator
def feature_details():
    return {
        frozenset({(BracketToken, '(')}): ([
                                               (BracketToken, '('),
                                               optional_features,
                                               (BracketToken, ')'),
                                               (ColonToken, None),
                                               (TypeIdToken, None),
                                               (BracketToken, '{'),
                                               expr,
                                               (BracketToken, '}')
                                           ], lambda x: [x[4], x[1], x[6]]),
        frozenset({(ColonToken, None)}): ([(ColonToken, None), (TypeIdToken, None), optional_expr],
                                          lambda x: [x[1], None, x[2]])
    }


# feature_formal                            ID
# ε	                                        )
@nonterminal_decorator
def optional_features():
    return {
        frozenset({(ObjectIdToken, None)}): ([feature_formal], lambda x: x[0]),
        frozenset({(BracketToken, ')')}): None
    }


# <- expr                                   <-
# ε                                         ,, in, ;
@nonterminal_decorator
def optional_expr():
    return {
        frozenset({(AssignmentToken, None)}): ([(AssignmentToken, None), expr], lambda x: x[1]),
        frozenset({(CommaToken, None), (KeywordToken, 'in'), (SemiColonToken, None)}): None
    }


# formal feature_formals                    ID
@nonterminal_decorator
def feature_formal():
    return {
        frozenset({(ObjectIdToken, None)}): ([formal, feature_formals], lambda x: [x[0], *x[1]])
    }


# , feature_formal	                        ,
# ε                                         )
@nonterminal_decorator
def feature_formals():
    return {
        frozenset({(CommaToken, None)}): ([(CommaToken, None), feature_formal], lambda x: x[1]),
        frozenset({(BracketToken, ')')}): None
    }


# ID : TYPE                                 ID
@nonterminal_decorator
def formal():
    return {
        frozenset({(ObjectIdToken, None)}): (
            [(ObjectIdToken, None), (ColonToken, None), (TypeIdToken, None)], lambda x: Formal(x[0], x[2]))
    }


# not_term expr_rr                          ID, not, isvoid, ~, integer, string, true, false
@nonterminal_decorator
def expr():
    return {
        frozenset({
            (ObjectIdToken, None),
            (UnaryOperatorToken, None),
            (StringToken, None),
            (IntegerToken, None),
            (KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '{'),
            (BracketToken, '('),
            (BooleanToken, None)}): ([not_term, expr_rr], lambda x: None )
    }


# @TYPE.ID( optional_comma_expr ) expr_rr   @
# .ID( optional_comma_expr ) expr_rr        .
# ε                                         ;, ,, then, else, fi, loop, pool, @, ., of, ), }, in, *, /, +, -, <=, <, =
@nonterminal_decorator
def expr_rr():
    return {
        frozenset({(DispatchToken, '@')}): ([
                                                (DispatchToken, '@'),
                                                (TypeIdToken, None),
                                                (DispatchToken, '.'),
                                                (ObjectIdToken, None),
                                                (BracketToken, '('),
                                                optional_comma_expr,
                                                (BracketToken, ')'),
                                                expr_rr
                                            ], lambda x: None),
        frozenset({(DispatchToken, '.')}): ([
                                                (DispatchToken, '.'),
                                                (ObjectIdToken, None),
                                                (BracketToken, '('),
                                                optional_comma_expr,
                                                (BracketToken, ')'),
                                                expr_rr
                                            ], lambda x: None),
        frozenset({
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
            (BinaryOperatorToken, None),
            (ComparatorToken, None)
        }): None
    }


# formal => expr ; id_type_arrows           ID
@nonterminal_decorator
def id_type_arrow():
    return {
        frozenset({(ObjectIdToken, None)}): ([formal, (ArrowToken, None), expr, (SemiColonToken, None), id_type_arrows],
                                             lambda x: None)
    }


# id_type_arrow                             ID
# ε                                         esac
@nonterminal_decorator
def id_type_arrows():
    return {
        frozenset({(ObjectIdToken, None)}): ([id_type_arrow], lambda x: None),
        frozenset({(KeywordToken, 'esac')}): None
    }


# formal optional_expr id_type_exprs        ID
@nonterminal_decorator
def id_type_expr():
    return {
        frozenset({(ObjectIdToken, None)}): ([formal, optional_expr, id_type_exprs], lambda x: None)
    }


# , id_type_expr                            ,
# ε                                         in
@nonterminal_decorator
def id_type_exprs():
    return {
        frozenset({(CommaToken, None)}): ([(CommaToken, None), id_type_expr], lambda x: None),
        frozenset({(KeywordToken, 'in')}): None
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
        }): ([comma_expr], lambda x: None),
        frozenset({(BracketToken, ')')}): None
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
        }): ([expr, comma_exprs], lambda x: None)
    }


# , comma_expr                              ,
# ε                                         )
@nonterminal_decorator
def comma_exprs():
    return {
        frozenset({(CommaToken, None)}): ([(CommaToken, None), comma_expr], lambda x: None),
        frozenset({(BracketToken, ')')}): None
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
        }): ([expr, (SemiColonToken, None), semicolon_exprs], lambda x: None)
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
        }): ([semicolon_expr], lambda x: None),
        frozenset({(BracketToken, '}')}): None
    }


# not compare_term                          not
# compare_term                              isvoid, ~, ID, integer, string, true, false, if, while, let, case, new, {, (
@nonterminal_decorator
def not_term():
    return {
        frozenset({(UnaryOperatorToken, 'not')}): ([(UnaryOperatorToken, 'not'), expr], lambda x: None),
        frozenset({
            (KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '{'),
            (BracketToken, '('),
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): ([compare_term], lambda x: None)
    }


# add_term compare_term_rr                  isvoid, ~, ID, integer, string, true, false
@nonterminal_decorator
def compare_term():
    return {
        frozenset({
            (KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '{'),
            (BracketToken, '('),
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): ([add_term, compare_term_rr, expr_rr], lambda x: None)
    }


# <= add_term compare_term_rr               <=
# < add_term compare_term_rr                <
# = add_term compare_term_rr                =
# ε                                         @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@nonterminal_decorator
def compare_term_rr():
    return {
        frozenset({(ComparatorToken, None)}): ([(ComparatorToken, None), expr], lambda x: None),
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


# multi_term add_term_rr                    isvoid, ~, ID, integer, string, true, false, if, while, let, case, new, {, (
@nonterminal_decorator
def add_term():
    return {
        frozenset({(
            KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '{'),
            (BracketToken, '('),
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): ([multi_term, add_term_rr], lambda x: None)
    }


# + multi_term add_term_rr                  +
# - multi_term add_term_rr                  -
# ε                                         <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@nonterminal_decorator
def add_term_rr():
    return {
        frozenset({(BinaryOperatorToken, '+')}): ([(BinaryOperatorToken, '+'), expr], lambda x: None),
        frozenset({(BinaryOperatorToken, '-')}): ([(BinaryOperatorToken, '-'), expr], lambda x: None),
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


# isvoid_term multi_term_rr                 isvoid, ~, ID, integer, string, true, false, if, while, let, case, new, {, (
@nonterminal_decorator
def multi_term():
    return {
        frozenset({
            (KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '{'),
            (BracketToken, '('),
            (UnaryOperatorToken, 'isvoid'),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): ([isvoid_term, multi_term_rr], lambda x: None)
    }


# * isvoid_term multi_term_rr	            *
# / isvoid_term multi_term_rr	            /
# ε	                                        +, -, <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
@nonterminal_decorator
def multi_term_rr():
    return {
        frozenset({(BinaryOperatorToken, '*')}): ([(BinaryOperatorToken, '*'), expr], lambda x: None),
        frozenset({(BinaryOperatorToken, '/')}): ([(BinaryOperatorToken, '/'), expr], lambda x: None),
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
# tilde_term                                ~, ID, integer, string, true, false, if, while, let, case, new, {, (
@nonterminal_decorator
def isvoid_term():
    return {
        frozenset({(UnaryOperatorToken, 'isvoid')}): ([(UnaryOperatorToken, 'isvoid'), expr], lambda x: None),
        frozenset({
            (KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '{'),
            (BracketToken, '('),
            (UnaryOperatorToken, '~'),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): ([tilde_term], lambda x: None)
    }


# ~ factor                                  ~
# factor                                    ID, integer, string, true, false, if, while, let, case, new, {, (
@nonterminal_decorator
def tilde_term():
    return {
        frozenset({(UnaryOperatorToken, '~')}): ([(UnaryOperatorToken, '~'), expr], lambda x: None),
        frozenset({
            (KeywordToken, 'if'),
            (KeywordToken, 'while'),
            (KeywordToken, 'let'),
            (KeywordToken, 'case'),
            (KeywordToken, 'new'),
            (BracketToken, '{'),
            (BracketToken, '('),
            (ObjectIdToken, None),
            (IntegerToken, None),
            (StringToken, None),
            (BooleanToken, None)
        }): ([factor], lambda x: None)
    }


# if expr then expr else expr fi expr_rr    if
# while expr loop expr pool expr_rr         while
# { semicolon_expr } expr_rr                {
# let id_type_expr in expr expr_rr          let
# case expr of id_type_arrow esac expr_rr   case
# new TYPE expr_rr                          new
# ( expr ) expr_rr                          (
# ID factor_id                              ID
# integer	                                integer
# string	                                string
# true	                                    TRUE
# false                                     FALSE
@nonterminal_decorator
def factor():
    return {
        frozenset({(KeywordToken, 'if')}): (
            [(KeywordToken, 'if'),
             expr,
             (KeywordToken, 'then'),
             expr,
             (KeywordToken, 'else'),
             expr,
             (KeywordToken, 'fi'),
             expr_rr], lambda x: None),
        frozenset({(KeywordToken, 'while')}): (
            [(KeywordToken, 'while'),
             expr,
             (KeywordToken, 'loop'),
             expr,
             (KeywordToken, 'pool'),
             expr_rr], lambda x: None),
        frozenset({(BracketToken, '{')}): (
            [(BracketToken, '{'), semicolon_expr, (BracketToken, '}'), expr_rr], lambda x: None),
        frozenset({(KeywordToken, 'let')}): (
            [(KeywordToken, 'let'), id_type_expr, (KeywordToken, 'in'), expr, expr_rr], lambda x: None),
        frozenset({(KeywordToken, 'case')}): (
            [(KeywordToken, 'case'),
             expr,
             (KeywordToken, 'of'),
             id_type_arrow,
             (KeywordToken, 'esac'),
             expr_rr], lambda x: None),
        frozenset({(KeywordToken, 'new')}): ([(KeywordToken, 'new'), (TypeIdToken, None), expr_rr], lambda x: None),
        frozenset({(BracketToken, '(')}): ([(BracketToken, '('), expr, (BracketToken, ')'), expr_rr], lambda x: None),
        frozenset({(ObjectIdToken, None)}): ([(ObjectIdToken, None), factor_id, expr_rr], lambda x: None),
        frozenset({(IntegerToken, None)}): ([(IntegerToken, None), expr_rr], lambda x: None),
        frozenset({(StringToken, None)}): ([(StringToken, None), expr_rr], lambda x: None),
        frozenset({(BooleanToken, None)}): ([(BooleanToken, None), expr_rr], lambda x: None)
    }


# ( optional_comma_expr )                   (
# ε                                         *, /, +, -, <=, <, =, @, ., ;, ,, then, else, fi, loop, pool, of, ), }, in
# <- expr                                   <-
@nonterminal_decorator
def factor_id():
    return {
        frozenset({(BracketToken, '(')}): ([(BracketToken, '('), optional_comma_expr, (BracketToken, ')')],
                                           lambda x: BracketedExpr(x[1])),
        frozenset({(AssignmentToken, None)}): ([(AssignmentToken, None), expr], lambda x: None),
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
