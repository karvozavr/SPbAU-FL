import re

WHITESPACE = [' ', '\f', '\t', '\n', '\r', '\n\r']


class Lexeme:
    format = '{type}({value}, {line}, {start}, {end})'

    class LexemeInfo:
        __slots__ = ('line', 'interval')

        def __init__(self, line, interval):
            self.line = line
            self.interval = interval

        def __eq__(self, other):
            return self.line == other.line and \
                   self.interval == other.interval

    __slots__ = ('value', 'info')

    def __init__(self, value, line, interval):
        self.info = Lexeme.LexemeInfo(line=line, interval=interval)
        self.value = value

    def __eq__(self, other):
        return self.__class__ == other.__class__ and \
               self.value == other.value and \
               self.info == other.info

    @staticmethod
    def parse(s: str, position: int, line: int):
        """
        Parse this lexeme.

        :param s: string to parse from
        :param position: position in the string
        :param line: current line
        :return: parsed lexeme
        """
        raise NotImplementedError

    @staticmethod
    def set_lexeme_format(fmt='{type}({value}, {line}, ({start}, {end}))'):
        """
        Lexeme string format

        :param fmt: format string, you may use: {type}, {value}, {line}, {start}, {end}
        """
        Lexeme.format = fmt

    def __str__(self):
        fmt = Lexeme.format
        start, end = self.info.interval
        return fmt.format(
            type=self.__class__.__name__,
            value=self.value_str(),
            line=self.info.line,
            start=start,
            end=end
        )

    def value_str(self):
        return '\'{value}\''.format(value=self.value)


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
    def parse(s: str, position: int, line: int):
        for value in Keyword.lexeme_value.values():
            end = len(value) + position
            if s.startswith(value, position) and is_terminal(s, end):
                return Keyword(value=value, line=line, interval=(position, end))
        return None


class Number(Lexeme):
    def __init__(self, value, line, interval):
        super().__init__(value, line, interval)

    @staticmethod
    def parse(s: str, position: int, line: int):
        number, length = Number._get_number(s[position:])
        end = position + length
        if length > 0 and is_terminal(s, end) and Number._good_number_end(s, end):
            return Number(value=number, line=line, interval=(position, end))
        return None

    @staticmethod
    def _get_number(s) -> (float, int):
        result = re.search(r'^[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?', s)
        return (float(result.group()), len(result.group())) if result is not None else (0, 0)

    def value_str(self):
        return '{value}'.format(value=self.value)

    @staticmethod
    def _good_number_end(s, idx):
        if idx < len(s):
            return s[idx] != '.'
        return True


class Bool(Lexeme):
    lexeme_value = dict(
        TRUE='true',
        FALSE='false'
    )

    def __init__(self, value, line, interval):
        super().__init__(value, line, interval)

    @staticmethod
    def parse(s: str, position: int, line: int):
        for value in Bool.lexeme_value.values():
            end = len(value) + position
            if s.startswith(value, position) and is_terminal(s, end):
                return Bool(value=value, line=line, interval=(position, end))
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
        GE='>=',
        LE='<=',
        GT='>',
        LT='<',
        AND='&&',
        OR='||'
    )

    @staticmethod
    def parse(s: str, position: int, line: int):
        for value in Op.lexeme_value.values():
            end = len(value) + position
            if s.startswith(value, position):
                return Op(value=value, line=line, interval=(position, end))
        return None

    def __init__(self, value, line, interval):
        super().__init__(value, line, interval)


class Ident(Lexeme):
    def __init__(self, value, line, interval):
        super().__init__(value, line, interval)

    @staticmethod
    def parse(s, position, line):
        ident, length = Ident._get_ident(s[position:])
        end = position + length
        if length > 0 and is_terminal(s, end):
            return Ident(value=ident, line=line, interval=(position, end))
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
    def parse(s: str, position: int, line):
        for value in Delim.lexeme_value.values():
            end = len(value) + position
            if s.startswith(value, position):
                return Delim(value=value, line=line, interval=(position, end))
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
