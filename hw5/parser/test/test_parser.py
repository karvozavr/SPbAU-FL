import unittest

from lexer import Lexer
from parser.parser import Operator, Identifier, Info, Expression, FunctionCall, Condition, WhileLoop, \
    FunctionDefinition, parse_program
from parser.visualizer import render_ast


class TestParser(unittest.TestCase):

    def testOperator(self):
        lexer = Lexer()
        code = '23 + 2'
        lexemes = lexer.run(code)
        expected = Operator(info=Info(line=0, interval=(3, 4)), op_type='+')
        result, pos = Operator.parse(lexemes=lexemes, pos=1)
        self.assertEqual(pos, 2)
        self.assertEqual(expected.info, result.info)
        self.assertEqual(expected.op_type, result.op_type)
        self.assertEqual(expected.children, result.children)

    def testIdentifier(self):
        lexer = Lexer()
        code = 'var_one := 1'
        lexemes = lexer.run(code)
        expected = Identifier(info=Info(line=0, interval=(0, 7)), name='var_one')
        result, pos = Identifier.parse(lexemes=lexemes, pos=0)
        self.assertEqual(pos, 1)
        self.assertEqual(expected.info, result.info)
        self.assertEqual(expected.name, result.name)
        self.assertEqual(expected.children, result.children)

    def testExpr(self):
        lexer = Lexer()
        code = '(var_one + 1) - (2 * (3 + 9))'
        lexemes = lexer.run(code)
        result, new_pos = Expression.parse(lexemes=lexemes, pos=0)
        self.assertIsNotNone(result)

    def testCall(self):
        lexer = Lexer()
        code = 'foo()'
        lexemes = lexer.run(code)
        result, new_pos = FunctionCall.parse(lexemes=lexemes, pos=0)
        self.assertIsNotNone(result)

    def testCondition(self):
        lexer = Lexer()
        code = 'if ((1 + 2) == 3) {} else {}'
        lexemes = lexer.run(code)
        result, new_pos = Condition.parse(lexemes=lexemes, pos=0)
        self.assertIsNotNone(result)

    def testWhile(self):
        lexer = Lexer()
        code = 'while (1 + 1 != 2) { write(3 + 3); read (x2); }'
        lexemes = lexer.run(code)
        result, new_pos = WhileLoop.parse(lexemes=lexemes, pos=0)
        self.assertIsNotNone(result)

    def testFunctionDef(self):
        lexer = Lexer()
        code = 'foo(arg1, arg2) {' \
               '    write(2 + 2);' \
               '}'
        lexemes = lexer.run(code)
        result, new_pos = FunctionDefinition.parse(lexemes=lexemes, pos=0)
        self.assertIsNotNone(result)

    def test_parse_program(self):
        code = 'foo(x, y) {' \
               '    write(x + 2);' \
               '}' \
               'bar(x, y) {' \
               '    write(x + 2);' \
               '}' \
               '' \
               'main() {' \
               '    if 2 < 3 {' \
               '        while 1 {' \
               '            read(x);' \
               '            write(foo(x, x));' \
               '        };' \
               '    } else {' \
               '        write(404);' \
               '    };' \
               '    write(42 * 239);' \
               '}'
        result = parse_program(code=code)
        self.assertIsNotNone(result)

    def test_render(self):
        code = '''foo(x, y) {
                   write(x + 2);
               }
               bar(x, y) {
                   write(x + 2);
               }
               if (1 + (2 + (3 + (4 + (5 + (2 < 3)))))) {
                   while 1 {
                       read(x);
                      write(foo(x, x));
                   };
               } else {
                   write(404);
               };
               write(42 * 239);
               '''
        result = parse_program(code=code)
        render_ast(result)

    def test_plus_assign(self):
        code = '''
        main() {
            while (1) {\n
                read(x);\n
                write(foo(x, x));\n
                a += 2;
                b /= 3;
                c *= 4;
                d -= 1;
            };\n
        }
        '''
        result = parse_program(code=code)
        self.assertIsNotNone(result)

    def test_fi(self):
        code = '''
        main() {
            if (a > b) {
                write(sugar_baby);
            } fi;
        }
        '''
        result = parse_program(code=code)
        self.assertIsNotNone(result)

    def test_loop(self):
        code = '''
                main() {
                    loop {
                        write(42);
                    };
                }
                '''
        result = parse_program(code=code)
        self.assertIsNotNone(result)

