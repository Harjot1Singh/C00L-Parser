# Parser for the C00L language, 1503883

import logger

from tokens import tokenise
import syntax_parser


# Parser entry point
def parse(path):
    # Read contents of file
    with open(path) as file:
        program_string = file.read()

    # Lex character-by-character
    tokens, lex_errors = tokenise(program_string)

    # Syntax parse
    program, parse_errors = syntax_parser.parse(tokens)

    # Print out classes and method names if there were no errors
    all_errors = lex_errors + parse_errors

    if not all_errors:
        logger.success('No errors found')

        # Iterate through all the classes in the program AST, and collect them
        for class_def in program:
            logger.header(class_def.class_type, end=' - ')

            methods = ', '.join([feature.identifier for feature in class_def])
            logger.info(methods)

    # Otherwise, print out the errors
    else:
        logger.error('Errors found')
        logger.info('\n'.join(all_errors))


# Application entry point
if __name__ == "__main__":
    import sys
    parse(sys.argv[1])