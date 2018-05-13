from lexer import Lexer, LexerError
from lexer.lexeme import *

lexer = Lexer()


def get_lexemes(s):
    return lexer.run(s)


def test_valid1():
    answer = [
        Keyword('read', 0, (2, 6)),
        Ident('x', 0, (7, 8)),
        Delim(';', 0, (8, 9)),
        Keyword('if', 0, (10, 12)),
        Ident('y', 0, (13, 14)),
        Op('+', 0, (15, 16)),
        Num(1.0, 0, (17, 18)),
        Op('==', 0, (19, 21)),
        Ident('x', 0, (22, 23)),
        Keyword('then', 0, (24, 28)),
        Keyword('write', 0, (29, 34)),
        Ident('y', 0, (35, 36)),
        Keyword('else', 0, (37, 41)),
        Keyword('write', 0, (42, 47)),
        Ident('x', 0, (48, 49))
    ]
    code = '  read x; if y + 1 == x then write y else write x\n'
    lexemes = get_lexemes(code)
    for lexeme, ans in zip(lexemes, answer):
        assert lexeme == ans


def test_valid2():
    answer = [
        Keyword('read', 0, (0, 4)),
        Ident('x', 0, (5, 6)),
        Delim(';', 0, (6, 7)),
        Keyword('if', 0, (8, 10)),
        Ident('y', 0, (11, 12)),
        Op('+', 0, (13, 14)),
        Num(1.0, 0, (15, 16)),
        Op('==', 0, (17, 19)),
        Ident('x', 0, (20, 21)),
        Keyword('then', 0, (22, 26)),
        Keyword('write', 0, (27, 32)),
        Ident('y', 0, (33, 34)),
        Keyword('else', 0, (35, 39)),
        Keyword('write', 0, (40, 45)),
        Ident('x', 0, (46, 47)),
        Keyword('while', 0, (48, 53)),
        Bool('true', 0, (54, 58)),
        Op('&&', 0, (59, 61)),
        Num(1.0, 0, (62, 63)),
        Op('-', 0, (64, 65)),
        Delim('(', 0, (66, 67)),
        Num(23.0, 0, (67, 69)),
        Op('+', 0, (70, 71)),
        Num(4.2e-11, 0, (72, 78)),
        Op('*', 0, (79, 80)),
        Ident('alpha', 0, (81, 86)),
        Delim(')', 0, (86, 87)),
        Op('!=', 0, (92, 94)),
        Ident('_beta', 0, (95, 100))
    ]
    code = 'read x; if y + 1 == x then write y else write x' \
           ' while true && 1 - (23 + 42e-12 * alpha)     != _beta \n \n'
    lexemes = get_lexemes(code)
    for lexeme, ans in zip(lexemes, answer):
        assert lexeme == ans


def test_valid3():
    answer = [
        Num(2.0, 0, (0, 1)),
        Op('+', 0, (1, 2)),
        Num(3.0, 0, (2, 3)),
        Op('-', 0, (3, 4)),
        Num(1.3e-09, 0, (4, 10)),
        Op('-', 0, (10, 11)),
        Num(3.0, 0, (11, 12)),
        Op('%', 0, (12, 13)),
        Delim('(', 0, (13, 14)),
        Ident('x', 0, (14, 15)),
        Op('||', 0, (15, 17)),
        Ident('y', 0, (17, 18)),
        Op('*', 0, (18, 19)),
        Ident('_2z', 0, (19, 22)),
        Delim(')', 0, (22, 23)),
    ]
    code = '2+3-1.3e-9-3%(x||y*_2z)'
    lexemes = get_lexemes(code)
    for lexeme, ans in zip(lexemes, answer):
        assert lexeme == ans


def test_valid4():
    answer = [
        Keyword('while', 0, (0, 5)),
        Delim('(', 0, (6, 7)),
        Num(2.0, 0, (7, 8)),
        Delim(',', 0, (8, 9)),
        Num(3.0, 0, (11, 12)),
        Delim(',', 0, (12, 13)),
        Num(4.0, 0, (15, 16)),
        Delim(',', 0, (16, 17)),
        Num(5.0, 0, (20, 21)),
        Delim(')', 0, (21, 22)),
        Op('>=', 0, (24, 26)),
        Delim('(', 0, (27, 28)),
        Ident('a', 0, (29, 30)),
        Delim(',', 0, (31, 32)),
        Ident('b', 0, (33, 34)),
        Delim(',', 0, (34, 35)),
        Ident('c', 0, (36, 37)),
        Delim(',', 0, (37, 38)),
        Ident('d', 0, (39, 40)),
        Delim(')', 0, (40, 41)),
        Delim(';', 0, (41, 42))
    ]
    code = 'while (2,  3,  4,   5)  >= ( a , b, c, d);'
    lexemes = get_lexemes(code)
    for lexeme, ans in zip(lexemes, answer):
        assert lexeme == ans


def test_valid5():
    code = ''
    lexemes = get_lexemes(code)
    assert lexemes == []


def test_valid6():
    code = ' \t \t     \n \r \r\n \n   \t\t\t \n \t '
    lexemes = get_lexemes(code)
    assert lexemes == []


def test_err1():
    code = 'while (2,  3a,  4,   5)  >= ( a , b, c, d);'
    try:
        get_lexemes(code)
    except LexerError as e:
        assert e.position == 11
    else:
        assert False


def test_err2():
    code = 'if a &-& b'
    try:
        get_lexemes(code)
    except LexerError as e:
        assert e.position == 5
    else:
        assert False


def test_err3():
    code = 'if a ~= 42; (3)'

    try:
        get_lexemes(code)
    except LexerError as e:
        assert e.position == 5
    else:
        assert False
