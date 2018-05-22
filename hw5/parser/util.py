from lexer.lexeme import Delim, Keyword


def is_open_block(token):
    return type(token) is Delim and token.value is Delim.lexeme_value['BLOCK_OPEN']


def is_close_block(token):
    return type(token) is Delim and token.value is Delim.lexeme_value['BLOCK_CLOSE']


def is_semicolon(token):
    return type(token) is Delim and token.value is Delim.lexeme_value['COLON']


def is_open_brace(token):
    return type(token) is Delim and token.value is Delim.lexeme_value['OPEN']


def is_close_brace(token):
    return type(token) is Delim and token.value is Delim.lexeme_value['CLOSE']


def is_comma(token):
    return type(token) is Delim and token.value is Delim.lexeme_value['COMMA']


def is_read(token):
    return type(token) is Keyword and token.value is Keyword.lexeme_value['READ']


def is_write(token):
    return type(token) is Keyword and token.value is Keyword.lexeme_value['WRITE']


def is_if(token):
    return type(token) is Keyword and token.value is Keyword.lexeme_value['IF']


def is_else(token):
    return type(token) is Keyword and token.value is Keyword.lexeme_value['ELSE']


def is_while(token):
    return type(token) is Keyword and token.value is Keyword.lexeme_value['WHILE']


def is_assignment_operator(token):
    return type(token) is Delim and token.value is Delim.lexeme_value['ASSIGN']


def is_arithm_assignment_operator(token):
    return type(token) is Delim and token.value in [Delim.lexeme_value['PLUS_ASSIGN'],
                                                    Delim.lexeme_value['MINUS_ASSIGN'],
                                                    Delim.lexeme_value['MUL_ASSIGN'],
                                                    Delim.lexeme_value['DIV_ASSIGN']]
