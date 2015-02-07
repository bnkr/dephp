from unittest import TestCase
from six.moves import StringIO
from dephp.scanner import lexer, scan_string
from dephp.plyparser import parser, parse_string
from dephp.output import PrettyOutputter
from dephp import phpast as ast

class PrettyPrinterTestCase(TestCase):
    def _run_from_string(self, string):
        parsed = parse_string(string)

        io = StringIO()

        output = PrettyOutputter(parsed)
        output.out = io
        output.run()

        return io.getvalue()

    def test_really_basic_thing_pretty(self):
        program = '<?php $data = "a";'
        expected = '$data = "a";'

        self.assertEquals(expected, self._run_from_string(program).strip())
