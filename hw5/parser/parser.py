from lexer.lexeme import *
from parser.util import *

Info = Lexeme.LexemeInfo


class ASTNode:
    __slots__ = ['children', 'info', 'name']

    @staticmethod
    def parse(lexemes, pos):
        """
        Parse this node from tokens.
        :param lexemes: tokens list
        :param pos: first token position
        :return: (token or None, new_pos)
        """
        raise NotImplementedError

    def __init__(self, info, children):
        self.name = str(type(self))
        self.info = info
        self.children = children


class Operator(ASTNode):
    def __init__(self, info, op_type):
        super().__init__(info=info, children=[])
        self.op_type = op_type

    @staticmethod
    def parse(lexemes, pos):
        token = lexemes[pos]
        if type(token) is Op:
            return Operator(info=token.info, op_type=token.value), pos + 1
        return None, pos


class Identifier(ASTNode):
    def __init__(self, info, id):
        super().__init__(info=info, children=[])
        self.id = id

    @staticmethod
    def parse(lexemes, pos):
        token = lexemes[pos]
        if type(token) is Ident:
            return Identifier(info=token.info, id=token.value), pos + 1
        return None, pos


class Statement(ASTNode):

    @staticmethod
    def parse(lexemes, pos):
        raise NotImplementedError


class Block(ASTNode):

    @staticmethod
    def parse(lexemes, pos):
        token = lexemes[pos]
        children = []
        while not is_close_block(token=token):
            statement, new_pos = Statement.parse(lexemes=lexemes, pos=pos)
            if statement is not None and is_semicolon(lexemes[new_pos]):
                children.append(statement)
                pos = new_pos + 1
            else:
                return None, pos
            token = lexemes[pos]

        return Block(children=children, info=None)
