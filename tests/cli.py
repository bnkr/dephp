from unittest import TestCase
from dephp.cli import DephpRunner

class DephpRunnerTestCase(TestCase):
    def test_tokenise_cli_arguments_create_settings(self):
        runner = DephpRunner()
        parser = runner.get_parser()
        settings = runner.get_settings(parser, ['tokenise', '-v', __file__])
        self.assertEquals([__file__], settings.file)
        self.assertEquals('tokenise', settings.command)
        self.assertEquals(True, settings.verbose)
        self.assertEquals(False, settings.debug)

    def test_parse_cli_arguments_create_settings(self):
        runner = DephpRunner()
        parser = runner.get_parser()
        settings = runner.get_settings(parser, ['parse', '-d', __file__, __file__])
        self.assertEquals([__file__, __file__], settings.file)
        self.assertEquals('parse', settings.command)
        self.assertEquals(False, settings.verbose)
        self.assertEquals(True, settings.debug)
