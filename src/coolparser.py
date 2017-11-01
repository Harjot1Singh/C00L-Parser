# Parser for the C00L language, 1503883

import logger

from tokens import tokenise


# Parser entry point
def parse(path):
    try:
        # Read contents of file
        with open(path) as file:
            program_string = file.read()
        # Lex character-by-character
        tokens = tokenise(program_string)
        logger.info(tokens)
        # Parse
        logger.success('No errors found')
    except RuntimeWarning: # defo change to own error
        logger.error('Errors found')
    pass


# Application entry point
if __name__ == "__main__":
    import sys
    parse(sys.argv[1])