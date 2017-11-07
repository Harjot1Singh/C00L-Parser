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

    # Parse
    classes, parse_errors = syntax_parser.parse(tokens)

    # Print out classes and method names if there were no errors
    all_errors = lex_errors + parse_errors
    if not all_errors:
        logger.success('No errors found')
        for class_name in classes:
            logger.info(class_name, '-', ', '.join(classes[class_name]))

    # Otherwise, print out the errors
    else:
        logger.error('Errors found')
        logger.info('\n'.join(all_errors))


# Application entry point
if __name__ == "__main__":
    import sys
    parse(sys.argv[1])