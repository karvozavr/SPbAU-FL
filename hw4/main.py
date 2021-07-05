#!/usr/bin/env python3

import click
from lexer import Lexer, LexerError


def get_code_str(filename):
    with open(filename, 'r') as code_file:
        return code_file.read()


def print_lexemes(lexemes, delimiter='; '):
    for lexeme in lexemes:
        print(lexeme, end=delimiter)
    print()


@click.command()
@click.option('--file', help='File with code to lex.')
@click.option('--format', default='{type}({value}, {line}, {start}, {end})',
              help='Lexeme format string. You may use: {type}, {value}, {line}, {start}, {end}')
@click.option('--delimiter', default='; ', help='Delimiter for printing lexemes.')
def main(file, format, delimiter):
    lexer = Lexer(fmt=format, noexcept=False)
    code_str = get_code_str(filename=file)
    try:
        lexemes = lexer.run(code_str)
    except LexerError as e:
        print('Lexer error at: {pos}.'.format(pos=e.position))
        exit(1)
    else:
        print_lexemes(lexemes=lexemes, delimiter=delimiter)


if __name__ == '__main__':
    main()
