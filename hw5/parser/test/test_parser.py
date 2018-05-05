import unittest
from lexer import Lexer
from lexer.lexeme import Lexeme
from parser.parser import Operator


class TestParser(unittest.TestCase):

    def testOperator(self):
        lexer = Lexer()
        code = '23 + 2'
        lexemes = lexer.run(code)
        expected = Operator(info=Lexeme.LexemeInfo(line=0, interval=(3, 4)), op_type='+')
        result, pos = Operator.parse(lexemes=lexemes, pos=1)
        self.assertEqual(pos, 2)
        self.assertEqual(expected.info, result.info)
        self.assertEqual(expected.op_type, result.op_type)
        self.assertEqual(expected.children, result.children)
