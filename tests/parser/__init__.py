import logging
from unittest import TestCase
from dephp.scanner import lexer, scan_string
from dephp.plyparser import parser, parse_string
from dephp import phpast as ast
from tests._util import log_level

class ExpressionTestCase(TestCase):
    def test_assign_with_double_quotes(self):
        # PHP Scanner doesn't work like this.
        tokens = scan_string('<?php $data = "a";')
        expected = ['VARIABLE', 'EQUALS', 'QUOTE', 'ENCAPSED_AND_WHITESPACE',
                    'QUOTE', 'SEMI']
        self.assertEquals(expected, [token.type for token in tokens])

        with log_level(logging.INFO):
            parsed = parse_string('<?php $data = "a";')

        expected = ast.Program([ast.AssignOp(ast.Variable('$data'), '=', ast.String('a'))])
        self.assertEquals(expected, parsed)

    def test_assign_with_single_quotes(self):
        parsed = parse_string('<?php $data = \'a\';')
        expected = ast.Program([ast.AssignOp(ast.Variable('$data'), '=', ast.String('a'))])
        self.assertEquals(expected, parsed)

    def test_parenthesis_new(self):
        """This tends to get shift/reduce-ey with other bracketted
        expressions."""
        self.markTestIncomplete("don't know the node names yet")

        parsed = parse_string('<?php (new stdclass)->prop;')
        expected = ast.Program([ast.AssignOp(ast.Variable('$data'), '=', ast.String('a'))])
        self.assertEquals(expected, parsed)

        parsed = parse_string('<?php (new stdclass(1, 2))->prop;')
        expected = ast.Program([ast.AssignOp(ast.Variable('$data'), '=', ast.String('a'))])
        self.assertEquals(expected, parsed)

class InternalFunctionsTestCase(TestCase):
    def test_isset(self):
        parsed = parse_string('<?php isset($a);')
        expected = ast.Program([ast.IsSet([ast.Variable('$a')])])
        self.assertEquals(expected, parsed)

        parsed = parse_string('<?php isset($a, $b);')
        expected = ast.Program([ast.IsSet([ast.Variable('$a'), ast.Variable('$b')])])
        self.assertEquals(expected, parsed)
