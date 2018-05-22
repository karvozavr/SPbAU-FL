#!/usr/bin/env python3

import click
from parser import parse_program, render_ast


def get_code_str(filename):
    with open(filename, 'r') as code_file:
        return code_file.read()


def print_lexemes(lexemes, delimiter='; '):
    for lexeme in lexemes:
        print(lexeme, end=delimiter)
    print()


@click.command()
@click.option('--file', help='File with code to parse.')
@click.option('--delimiter', default='; ', help='Delimiter for printing lexemes.')
def main(file):
    code_str = get_code_str(filename=file)
    ast = parse_program(code=code_str)
    if ast is not None:
        render_ast(ast, )
    else:
        print('Parser error.')


if __name__ == '__main__':
    main()
