from lexer import Lexer
from lexer.lexeme import *
from parser.util import *
import copy

Info = Lexeme.LexemeInfo


class ASTNode:
    __slots__ = ['children', 'info', 'node_name']

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
        self.node_name = str(type(self))
        self.info = info
        self.children = children

    def __str__(self):
        return '{me}: {info} ( {children} )'.format(me=self.node_name, info=self.info, children=self.children)


class Operator(ASTNode):
    def __init__(self, info, op_type):
        super().__init__(info=info, children=[])
        self.op_type = op_type
        self.node_name = 'Operator'

    def __str__(self):
        return '{name}\n{op}'.format(name=self.node_name, op=self.op_type)

    @staticmethod
    def parse(lexemes, pos):
        token = lexemes[pos]
        if type(token) is Op:
            return Operator(info=token.info, op_type=token.value), pos + 1
        return None, pos


class Identifier(ASTNode):
    def __init__(self, info, name):
        super().__init__(info=info, children=[])
        self.name = name
        self.node_name = 'Identifier'

    def __str__(self):
        return '{name}\n{value}'.format(name=self.node_name, value=self.name)

    @staticmethod
    def parse(lexemes, pos):
        token = lexemes[pos]
        if type(token) is Ident:
            return Identifier(info=token.info, name=token.value), pos + 1
        return None, pos


class Block(ASTNode):
    node_name = 'Block'

    def __str__(self):
        return '{name}'.format(name=self.node_name)

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

        return Block(children=children, info=None), pos


class Number(ASTNode):

    def __init__(self, info, value):
        super().__init__(info, children=[])
        self.value = value
        self.node_name = 'Number'

    def __str__(self):
        return '{name}: {value}'.format(name=self.node_name, value=self.value)

    @staticmethod
    def parse(lexemes, pos):
        token = lexemes[pos]
        if type(token) is Num:
            return Number(info=token.info, value=token.value), pos + 1
        return None, pos


class FunctionCall(ASTNode):

    def __init__(self, info, children, name):
        super().__init__(info, children=children)
        self.name = name
        self.node_name = 'FunctionCall'

    def __str__(self):
        return '{name}\n{func_name}'.format(name=self.node_name, func_name=self.name)

    @staticmethod
    def parse(lexemes, pos):
        function_name, new_pos = Identifier.parse(lexemes=lexemes, pos=pos)
        if function_name is not None and is_open_brace(lexemes[new_pos]):
            args, end_pos = Arguments.parse(lexemes=lexemes, pos=new_pos)
            return FunctionCall(
                info=Info(line=function_name.info.line, interval=function_name.info.interval),
                name=function_name.name,
                children=[args]), end_pos
        else:
            return None, pos


class Expression(ASTNode):
    terminals = [Number, FunctionCall, Identifier]

    @staticmethod
    def parse(lexemes, pos):
        lhs, new_pos = Expression.parse_terminal(lexemes=lexemes, pos=pos)
        if lhs is not None:
            if len(lexemes) <= new_pos:
                return lhs, new_pos
            op, rhs_pos = Operator.parse(lexemes=lexemes, pos=new_pos)
            if op is None:
                return lhs, new_pos
            else:
                rhs, new_pos = Expression.parse(lexemes=lexemes, pos=rhs_pos)
                if rhs is not None:
                    op.children = [lhs, rhs]
                    return op, new_pos
                else:
                    return None, pos

        return None, pos

    @staticmethod
    def parse_terminal(lexemes, pos):
        for node_type in Expression.terminals:
            result, new_pos = node_type.parse(lexemes=lexemes, pos=pos)
            if result is not None:
                return result, new_pos
        token = lexemes[pos]
        if is_open_brace(token):
            result, new_pos = Expression.parse(lexemes=lexemes, pos=pos + 1)
            if result is not None and is_close_brace(lexemes[new_pos]):
                return result, new_pos + 1
        return None, pos


class ReadFunction(ASTNode):

    def __init__(self, info, children):
        super().__init__(info, children=children)
        self.node_name = 'InputFunction'

    def __str__(self):
        return 'Read function'

    @staticmethod
    def parse(lexemes, pos):
        if is_read(lexemes[pos]) and is_open_brace(lexemes[pos + 1]):
            arg, new_pos = Identifier.parse(lexemes=lexemes, pos=pos + 2)
            if arg is not None:
                if is_close_brace(lexemes[new_pos]):
                    return ReadFunction(info=Info(
                        line=lexemes[pos].info.line,
                        interval=(
                            lexemes[pos].info.interval[0],
                            lexemes[new_pos].info.interval[1])),
                        children=[arg]), new_pos + 1
        return None, pos


class PrintFunction(ASTNode):

    def __init__(self, info, children):
        super().__init__(info, children=children)
        self.node_name = 'OutputFunction'

    def __str__(self):
        return 'Write function'

    @staticmethod
    def parse(lexemes, pos):
        if is_write(lexemes[pos]) and is_open_brace(lexemes[pos + 1]):
            arg, new_pos = Expression.parse(lexemes=lexemes, pos=pos + 2)
            if arg is not None:
                if is_close_brace(lexemes[new_pos]):
                    return PrintFunction(info=Info(
                        line=lexemes[pos].info.line,
                        interval=(
                            lexemes[pos].info.interval[0],
                            lexemes[new_pos].info.interval[1])),
                        children=[arg]), new_pos + 1

        return None, pos


class Condition(ASTNode):

    def __init__(self, condition, true_block, false_block):
        condition.node_name = 'Condition'
        true_block.node_name = 'If true'
        false_block.node_name = 'If false'
        super().__init__(info=None, children=[condition, true_block, false_block])
        self.node_name = 'Condition'

    def __str__(self):
        return 'Conditional'

    @staticmethod
    def parse(lexemes, pos):
        if is_if(lexemes[pos]):
            condition, new_pos = Expression.parse(lexemes=lexemes, pos=pos + 1)
            if condition is not None and is_open_block(lexemes[new_pos]):
                true_block, new_pos = Block.parse(lexemes=lexemes, pos=new_pos + 1)
                if true_block is not None and is_close_block(lexemes[new_pos]) and is_else(
                        lexemes[new_pos + 1]) and is_open_block(lexemes[new_pos + 2]):
                    false_block, new_pos = Block.parse(lexemes=lexemes, pos=new_pos + 3)
                    if false_block is not None and is_close_block(lexemes[new_pos]):
                        return Condition(condition=condition, true_block=true_block,
                                         false_block=false_block), new_pos + 1
        return None, pos


class WhileLoop(ASTNode):

    def __init__(self, condition, block):
        condition.node_name = 'Condition'
        block.node_name = 'Loop body'
        super().__init__(info=None, children=[condition, block])
        self.node_name = 'WhileLoop'

    def __str__(self):
        return 'While loop'

    @staticmethod
    def parse(lexemes, pos):
        if is_while(lexemes[pos]):
            condition, new_pos = Expression.parse(lexemes=lexemes, pos=pos + 1)
            if condition is not None and is_open_block(lexemes[new_pos]):
                block, new_pos = Block.parse(lexemes=lexemes, pos=new_pos + 1)
                if block is not None and is_close_block(lexemes[new_pos]):
                    return WhileLoop(condition=condition, block=block), new_pos + 1
        return None, pos


class Assignment(ASTNode):

    def __init__(self, var, value):
        super().__init__(info=None, children=[var, value])
        self.node_name = 'Assignment'

    def __str__(self):
        return 'Assignment'

    @staticmethod
    def parse(lexemes, pos):
        var, new_pos = Identifier.parse(lexemes=lexemes, pos=pos)
        if var is not None:
            op = lexemes[new_pos]
            if is_assignment_operator(op):
                expr, new_pos = Expression.parse(lexemes=lexemes, pos=new_pos + 1)
                if expr is not None:
                    return Assignment(var=var, value=expr), new_pos
            elif is_arithm_assignment_operator(op):
                expr, new_pos = Expression.parse(lexemes=lexemes, pos=new_pos + 1)
                if expr is not None:
                    result = Operator(info=expr.info, op_type=op.value[0:1])
                    result.children = [copy.copy(var), expr]
                    return Assignment(var=var, value=result), new_pos
        return None, pos


class Statement(ASTNode):
    terminals = [ReadFunction, PrintFunction, WhileLoop, Condition, FunctionCall, Assignment]

    @staticmethod
    def parse(lexemes, pos):
        for node_type in Statement.terminals:
            result, new_pos = node_type.parse(lexemes=lexemes, pos=pos)
            if result is not None:
                return result, new_pos
        return None, pos


class Arguments(ASTNode):

    def __init__(self, children):
        super().__init__(info=None, children=children)
        self.node_name = 'Arguments'

    def __str__(self):
        return 'Arguments'

    @staticmethod
    def parse(lexemes, pos):
        args = []
        if is_open_brace(lexemes[pos]):
            current_pos = pos + 1
            while not is_close_brace(lexemes[current_pos]):
                expr, new_pos = Expression.parse(lexemes=lexemes, pos=current_pos)
                if expr is not None:
                    args.append(expr)
                    if is_comma(lexemes[new_pos]):
                        current_pos = new_pos + 1
                        continue
                    elif is_close_brace(lexemes[new_pos]):
                        break
                    else:
                        return None, pos
                else:
                    return None, pos
            end_pos = pos + 2 + (0 if len(args) == 0 else 2 * len(args) - 1)
            return Arguments(children=args), end_pos
        return None, pos


class FunctionDefinition(ASTNode):

    def __init__(self, name, body, args):
        body.node_name = 'Function body'
        super().__init__(info=None, children=[args, body])
        self.node_name = 'FunctionDefinition'
        self.name = name

    def __str__(self):
        return 'Function Definition\n' \
               '{name}'.format(name=self.name)

    @staticmethod
    def parse(lexemes, pos):
        function_name, new_pos = Identifier.parse(lexemes=lexemes, pos=pos)
        if function_name is not None and is_open_brace(lexemes[new_pos]):
            args, end_pos = Arguments.parse(lexemes=lexemes, pos=new_pos)
            if is_open_block(lexemes[end_pos]):
                body, end_pos = Block.parse(lexemes=lexemes, pos=end_pos + 1)
                if body is not None and is_close_block(lexemes[end_pos]):
                    return FunctionDefinition(name=function_name.name, body=body, args=args), end_pos + 1
        return None, pos


class Program(ASTNode):
    def __init__(self, children):
        super().__init__(info=None, children=children)
        self.node_name = 'Program'

    def __str__(self):
        return 'Program'

    @staticmethod
    def parse(lexemes, pos):
        current_pos = pos
        children = []
        while current_pos < len(lexemes):
            definition, new_pos = FunctionDefinition.parse(lexemes=lexemes, pos=current_pos)
            if definition is not None:
                children.append(definition)
                current_pos = new_pos
            else:
                return None
        return Program(children=children)


def error(position, text):
    print('Error at {pos}: {text}'.format(pos=position, text=text))


def parse_program(code):
    lexer = Lexer()
    lexemes = lexer.run(code)
    return Program.parse(lexemes=lexemes, pos=0)
