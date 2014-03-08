import logging, contextlib
from unittest import TestCase
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
        parsed = parse_string('<?php $data = \'a\';')
        expected = [ast.AssignOp(ast.Variable('$data'), '=', 'a')]
        self.assertEquals(expected, parsed)
