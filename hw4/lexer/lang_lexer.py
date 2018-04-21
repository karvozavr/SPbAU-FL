# class Tokens:
#     __slots__ = ('position', 'string')
#
#     def __init__(self, pos, string):
#         self.position = pos
#         self.string = string
#
#
# class ParseResult:
#     __slots__ = ('error', 'result')
#
#     def __init__(self, result=None, error=None):
#         self.result = result
#         self.error = error
#
#     def success(self) -> bool:
#         return self.error is None
#
#
# class Parser:
#     __slots__ = ('parse',)
#
#     def __init__(self, parse):
#         self.parse = parse
#
#     def __call__(self, tokens):
#         return self.parse(tokens)
#
#     def __add__(self, other):
#         def _parse(tokens):
#             result, rest = self(tokens)
#             if result.success():
#                 return result, rest
#             else:
#                 return other(tokens), rest
#
#         return Parser(_parse)
#
#     def __mul__(self, other):
#         def _parse(tokens):
#             result, rest = self(tokens)
#             if result.success():
#                 return result, rest
#             else:
#                 return other(tokens), rest


def next_lexeme(s, pos):
    pass


def get_lexemes(s):
    pos = 0
    while pos < len(s):
        next_lexeme(s, pos)

