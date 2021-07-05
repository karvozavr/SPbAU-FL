from lexer.lexeme import *

WHITESPACE = [' ', '\f', '\t']
NEWLINE = ['\r\n', '\n', '\r']


class LexerError(Exception):
    __slots__ = ('position', 'line')

    def __init__(self, message, position, line=0):
        super().__init__(message)
        self.position = position
        self.line = line


def parse_whitespace(s, pos):
    new_pos = pos
    while new_pos < len(s) and s[new_pos] in WHITESPACE:
        new_pos += 1
    return new_pos


def parse_newline(s, pos):
    new_pos = pos
    for newline_symb in NEWLINE:
        if s.startswith(newline_symb, pos):
            new_pos += len(newline_symb)
            break
    return new_pos


def next_lexeme(s, pos, line):
    lexeme_types = [Keyword, Bool, Ident, Number, Delim, Op]

    for lexeme in lexeme_types:
        result = lexeme.parse(s=s, position=pos, line=line)
        if result is not None:
            return result

    return None


class Lexer:
    __slots__ = ('noexcept', '_error_pos')

    def __init__(self, fmt='{type}({value}, {line}, {start}, {end})', noexcept=False):
        Lexeme.set_lexeme_format(fmt=fmt)
        self.noexcept = noexcept
        self._error_pos = -1

    @property
    def error_pos(self):
        return self._error_pos

    def run(self, s):
        pos = 0
        line = 0
        lexeme_list = []
        while pos < len(s):
            new_pos = parse_whitespace(s, pos)
            if new_pos > pos:
                pos = new_pos
                continue

            new_pos = parse_newline(s, pos)
            if new_pos > pos:
                pos = new_pos
                line += 1
                continue

            lexeme = next_lexeme(s, pos, line)
            if lexeme is not None:
                lexeme_list.append(lexeme)
                pos = lexeme.info.interval[1]
            else:
                if self.noexcept:
                    self._error_pos = pos
                    return None
                else:
                    self._error_pos = pos
                    raise LexerError(message="Failed to parse lexeme.", position=pos, line=line)
        return lexeme_list
