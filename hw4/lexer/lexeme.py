import re

WHITESPACE = [' ', '\f', '\t', '\n', '\r', '\n\r']


class LexerError(Exception):
    __slots__ = ('position',)

    def __init__(self, message, position):
        super().__init__(message)
        self.position = position


class Lexeme:
    class LexemeInfo:
        __slots__ = ('line', 'interval')

        def __init__(self, line, interval):
            self.line = line
            self.interval = interval

    __slots__ = ('value', 'info')

    def __init__(self, value, line, interval):
        self.info = Lexeme.LexemeInfo(line=line, interval=interval)
        self.value = value

    @staticmethod
    def parse(s, pos, line):
        """
        Parse this lexeme.

        :param s: string to parse from
        :param pos: position in the string
        :param line: current line
        :return: parsed lexeme
        """
        raise NotImplementedError

    def __str__(self):
        fmt = '{type}("{value}", {line}, {start}, {end})'
        start, end = self.info.interval
        return fmt.format(
            type=self.__class__.__name__,
            value=self.value,
            line=self.info.line,
            start=start,
            end=end
        )


class Keyword(Lexeme):
    lexeme_value = dict(
        IF='if',
        THEN='then',
        ELSE='else',
        WHILE='while',
        DO='do',
        READ='read',
        WRITE='write'
    )

    def __init__(self, value, line, interval):
        super().__init__(value, line, interval)

    @staticmethod
    def parse(s: str, pos: int, line):
        s = s[pos:]
        for value in Keyword.lexeme_value.values():
            end = len(value) + pos
            if s.startswith(value) and is_terminal(s, end):
                return Keyword(value=value, line=line, interval=(pos, end))
        return None


class Number(Lexeme):
    def __init__(self, value, line, interval):
        super().__init__(value, line, interval)

    @staticmethod
    def parse(s: str, pos: int, line):
        s = s[pos:]
        number, length = Number._get_number(s)
        end = pos + length
        if length > 0 and is_terminal(s, end):
            return Number(value=number, line=line, interval=(pos, end))
        return None

    @staticmethod
    def _get_number(s) -> (float, int):
        result = re.search(r'^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?', s)
        return (float(result.group()), len(result.group())) if result is not None else (0, 0)


class Bool(Lexeme):
    lexeme_value = dict(
        TRUE='true',
        FALSE='false'
    )

    def __init__(self, value, line, interval):
        super().__init__(value, line, interval)

    @staticmethod
    def parse(s: str, pos: int, line):
        s = s[pos:]
        for value in Bool.lexeme_value.values():
            end = len(value) + pos
            if s.startswith(value) and is_terminal(s, end):
                return Bool(value=value, line=line, interval=(pos, end))
        return None


class Op(Lexeme):
    lexeme_value = dict(
        PLUS='+',
        MINUS='-',
        MUL='*',
        DIV='/',
        MOD='%',
        EQ='==',
        NEQ='!=',
        GT='>',
        GE='>=',
        LT='<',
        LE='<=',
        AND='&&',
        OR='||'
    )

    @staticmethod
    def parse(s: str, pos: int, line):
        s = s[pos:]
        for value in Op.lexeme_value.values():
            end = len(value) + pos
            if s.startswith(value):
                return Op(value=value, line=line, interval=(pos, end))
        return None

    def __init__(self, value, line, interval):
        super().__init__(value, line, interval)


class Ident(Lexeme):
    def __init__(self, value, line, interval):
        super().__init__(value, line, interval)

    @staticmethod
    def parse(s, pos, line):
        s = s[pos:]
        ident, length = Ident._get_ident(s)
        end = pos + length
        if length > 0 and is_terminal(s, end):
            return Ident(value=ident, line=line, interval=(pos, end))
        return None

    @staticmethod
    def _get_ident(s) -> (float, int):
        result = re.search(r'^[a-z_]+[a-z0-9_]*', s)
        if result is not None:
            return result.group(), len(result.group())
        return None, 0


class Delim(Lexeme):
    lexeme_value = dict(
        COLON=';',
        COMMA=',',
        OPEN='(',
        CLOSE=')'
    )

    @staticmethod
    def parse(s: str, pos: int, line):
        s = s[pos:]
        for value in Delim.lexeme_value.values():
            end = len(value) + pos
            if s.startswith(value):
                return Delim(value=value, line=line, interval=(pos, end))
        return None

    def __init__(self, value, line, interval):
        super().__init__(value, line, interval)


def is_whitespace(s, idx):
    if len(s) <= idx:
        return True
    return s[idx] in WHITESPACE


def is_terminal(s, idx):
    if len(s) <= idx:
        return True
    return is_whitespace(s, idx) or not re.match(r'[a-z0-9_]', s[idx])
