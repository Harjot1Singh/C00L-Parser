import logger


# Parser entry point
def parse(path):
    logger.header('Parsing', path)
    try:
        # Lex
        # Parse
        logger.success('No errors found in', path)
    except Exception:
        logger.error('Errors found in', path)
    pass


# Application entry point
if __name__ == "__main__":
    import sys
    parse(sys.argv[1])