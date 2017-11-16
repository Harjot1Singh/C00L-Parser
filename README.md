# C00L-Parser
A lexer and syntactic predictive parser for the C00L programming language, written in Python.

The transformed LL(1) (I hope) grammar can be found in `grammar.txt`.


# Requirements

- Python 3


# Running

`python3 src/coolparser.py [filename]` to run the parser on the given file. Any detected syntactic errors will be reported.

Running `./test-cool.sh` will run the parser against all the files in the `tests/` directory.


# Features

- Partial AST
- (Dodgy) error recovery 
  - Predictive
  - Panic-based
- Does NOT handle comments (obviously, this is a feature)
