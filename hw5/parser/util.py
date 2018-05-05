from lexer.lexeme import Delim


def is_close_block(token):
    return type(token) is Delim and token.value is Delim.lexeme_value['BLOCK_CLOSE']


def is_semicolon(token):
    return type(token) is Delim and token.value is Delim.lexeme_value['COLON']
