from unittest import TestCase
from ply.lex import LexToken
from dephp.scanner import lexer, scan_string

def iter_tokens(lexer):
    while True:
        token = lexer.token()
        if token is None:
            break
        else:
            yield token

class LexerTestCase(TestCase):
    def _make_lexer(self, string):
        lexer.lexer.begin('INITIAL')
        lexer.input(string)
        return lexer

    def test_lex_something(self):
        # Probably broken because we messed with things to make double quotes work...
        token = scan_string("stuff")[0]

        self.assertEquals("stuff", token.value)
        self.assertEquals(1, token.lineno)
        self.assertEquals('INLINE_HTML', token.type)

        token = lexer.token()
        self.assertEquals(None, token)

    def test_unknown_is_token(self):
        lexer = self._make_lexer("<?php\xFF;")

        expected = [('UNKNOWN', '\xFF'),
                    ('SEMI', ';')]
        got = [(token.type, token.value) for token in iter_tokens(lexer)]
        self.assertEquals(expected, got)
