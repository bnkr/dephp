import logging, contextlib
from unittest import TestCase
from dephp.pyparser import php_program
from dephp.scanner import lexer, scan_string
from dephp.plyparser import parser, parse_string
from dephp import phpast as ast

@contextlib.contextmanager
def log_level(level):
    old = logging.getLogger().getEffectiveLevel()
    try:
        logging.getLogger().setLevel(level)
        yield
    finally:
        logging.getLogger().setLevel(old)

class PyparsingParserTestCase(TestCase):
    def test_emty_string_is_empty(self):
        parsed = php_program.parseString("")
        self.assertEquals([], list(parsed))

    def test_whitespace_string_is_empty(self):
        parsed = php_program.parseString("  ")
        self.assertEquals([], list(parsed))

    def test_non_php_parsed_is_junk(self):
        string = """
        not php
        """
        parsed = php_program.parseString(string)
        self.assertEquals([string], list(parsed))

class ExpressionTestCase(TestCase):
    def test_assign_with_double_quotes(self):
        # PHP Scanner doesn't work like this.
        tokens = scan_string('<?php $data = "a";')
        expected = ['VARIABLE', 'EQUALS', 'QUOTE', 'ENCAPSED_AND_WHITESPACE',
                    'QUOTE', 'SEMI']
        self.assertEquals(expected, [token.type for token in tokens])

        with log_level(logging.INFO):
            parsed = parse_string('<?php $data = "a";')

        expected = [ast.AssignOp(ast.Variable('$data'), '=', 'a')]
        self.assertEquals(expected, parsed)

    def test_assign_with_single_quotes(self):
        ast = parse_string('<?php $data = \'a\';')
        self.assertEquals([], ast)
