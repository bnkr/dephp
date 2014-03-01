from unittest import TestCase
from dephp.parser.pyparse import php_program

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
