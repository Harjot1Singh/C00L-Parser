# General AST class to provide a common base
class AST:
    pass


# program ::= [[class; ]]+
# Holds a list of classes
class Program(AST):
    def __init__(self, classes):
        self.classes = classes

    def __iter__(self):
        return iter(self.classes)

    def __str__(self):
        return '[{}]'.format(', '.join(str(class_def) for class_def in self.classes))


# class ::= class TYPE [inherits TYPE] { [[feature; ]]∗
# Holds the type, optional inherited type, and an optional list of features
class Class(AST):
    def __init__(self, class_type, inherits_type=None, features=None):
        self.class_type = class_type
        self.inherits_type = inherits_type
        self.features = features

    def __iter__(self):
        return iter(self.features)

    def __str__(self):
        return 'Class({})'.format(self.class_type)


# Wrapper for ID : TYPE [ <- expr ]
# Holds the id, the type, and an optional expression
class IdType(AST):
    def __init__(self, identifier, identifier_type, expr=None):
        self.identifier = identifier
        self.identifier_type = identifier_type
        self.expr = expr


# feature ::= ID( [ formal [[, formal]]∗ ] ) : TYPE { expr }
# feature ::= ID : TYPE [ <- expr ]
# Holds the id, the type, an optional list of formal parameters, and an optional expression
class Feature(AST):
    def __init__(self, identifier, identifier_type, formals=None, expr=None):
        self.identifier = identifier
        self.identifier_type = identifier_type
        self.formals = formals
        self.expr = expr


# formal ::= ID : TYPE
# Holds an id and its type
class Formal(AST):
    def __init__(self, identifier, identifier_type):
        self.identifier = identifier
        self.identifier_type = identifier_type


# General expression class to provide a common base
class Expr(AST):
    pass


# expr ::= ID <- expr
# Holds the id and assigned expression
class Assignment(Expr):
    def __init__(self, identifier, expr):
        self.identifier = identifier
        self.expr = expr


# expr ::= expr[@TYPE].ID( [ expr [[, expr]]∗ ] )
# expr ::= ID( [ expr [[, expr]]∗ ] )
# Holds the id, called expressions, optional prefixed expression, and optional type (to cast to)
class Dispatch(Expr):
    def __init__(self, identifier, exprs, pre_expr=None, cast_type=None):
        self.identifier = identifier
        self.exprs = exprs
        self.pre_expr = pre_expr
        self.cast_type = cast_type


# expr ::= if expr then expr else expr fi
# Holds the if expression, then expression, and else expression
class If(Expr):
    def __init__(self, if_expr, then_expr, else_expr):
        self.if_expr = if_expr
        self.then_expr = then_expr
        self.else_expr = else_expr


# expr ::= while expr loop expr pool
# Holds the while expr, and loop expr
class While(Expr):
    def __init__(self, while_expr, loop_expr):
        self.while_expr = while_expr
        self.loop_expr = loop_expr


# expr ::= { [[expr; ]]+ }
# Holds a list of expressions
class BracedExpr(Expr):
    def __init__(self, exprs):
        self.exprs = exprs


# expr ::= (expr)
# Holds a (bracketed) expression
class BracketedExpr(Expr):
    def __init__(self, expr):
        self.expr = expr


# expr ::= let ID : TYPE [ <- expr ] [[,ID : TYPE [ <- expr ]]]∗ in expr
# Holds a list of IdTypes, and the in expression
class Let(Expr):
    def __init__(self, identifier_types, in_expr):
        self.identifier_types = identifier_types
        self.in_expr = in_expr


# expr ::= case expr of [[ID : TYPE => expr; ]]+ esac
# Holds the case expr and a list of IdTypes
class Case(Expr):
    def __init__(self, expr, identifier_types):
        self.expr = expr
        self.identifier_types = identifier_types


# expr ::= new TYPE
# Holds a type name for instantiation
class New(Expr):
    def __init__(self, class_type):
        self.class_type = class_type


# expr ::= ~expr
# expr ::= not expr
# expr ::= isvoid expr
# Holds the operator and expression for unary operations
class Unary(Expr):
    def __init__(self, operator, expr):
        self.operator = operator
        self.expr = expr


# expr ::= expr + expr
# expr ::= expr - expr
# expr ::= expr * expr
# expr ::= expr / expr
# expr ::= expr < expr
# expr ::= expr <= expr
# expr ::= expr = expr
# Holds a left expression, operator, right expression for binary operations
class Binary(Expr):
    def __init__(self, left_expr, operator, right_expr):
        self.left_expr = left_expr
        self.operator = operator
        self.right_expr = right_expr
