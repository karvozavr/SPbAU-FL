from lexer.lexeme import *


def test_keyword_parse():
    s = ['while', 'if ', 'if 0 > 2', 'read x']
    ans = ['while', 'if', 'if', 'read']
    for test, answer in zip(s, ans):
        kw = Keyword.parse(test, 0, 0)
        assert kw.value is answer


def test_keyword_parse_from_position():
    s = '23 write a'
    kw = Keyword.parse(s, position=3, line=0)
    assert kw.value is 'write'


def test_keyword_parse_error():
    s_err = ['2while', 'whilevar', ' read']
    for test in s_err:
        assert Keyword.parse(test, 0, 0) is None


def test_number_parse():
    s = ['22', '23e-23-1', '42.5']
    ans = [22, 23e-23, 42.5]
    for test, answer in zip(s, ans):
        kw = Num.parse(test, 0, 0)
        assert kw.value == answer


def test_number_parse_error():
    s = ['a22', '23e', '42.e', '232qew']
    for test in s:
        kw = Num.parse(test, 0, 0)
        assert kw is None


def test_parse_ident():
    s = ['var', 'var+b', 'a - 2', '_aa2+3']
    ans = ['var', 'var', 'a', '_aa2']
    for test, answer in zip(s, ans):
        kw = Ident.parse(test, 0, 0)
        assert kw.value == answer


def test_parse_ident_error():
    s = ['2var', '+aa', '(ident', '2 ident']
    for test in s:
        kw = Ident.parse(test, 0, 0)
        assert kw is None


if __name__ == '__main__':
    test_number_parse_error()
