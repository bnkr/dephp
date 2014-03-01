import sys, argparse, logging, os

class CliError(Exception):
    """Error suitable for printing on stderr."""

class DephpRunner(object):
    """Command line program."""
    def run(self):
        try:
            parser = self.get_parser()
            settings = self.get_settings(parser, argv=sys.argv[1:])
            self.setup_logging(settings)
            return getattr(self, settings.command)(settings)
        except CliError as ex:
            sys.stderr.write("{0}\n".format(ex))
            return 1

    def tokenise(self, settings):
        """Tokeniser which does something with each token."""
        from dephp.scanner import lexer, LexerSyntaxError

        def print_visitor(token):
            print token

        def noop_visitor(token):
            pass

        visitors = {
            'tokens': print_visitor,
            'lint': noop_visitor,
        }

        visitor = visitors[settings.to]

        def iter_tokens(io):
            # Input doesn't reset the state at the end of file.
            lexer.lexer.begin('INITIAL')
            lexer.input(io.read())
            while True:
                token = lexer.token()
                if token is None:
                    break
                yield token

        for name in settings.file:
            with open(name, 'r') as io:
                try:
                    map(visitor, iter_tokens(io))
                except LexerSyntaxError as ex:
                    sys.stderr.write("{0}:{1}: bad token: {2!r}\n".format(name, ex.line, ex.value))

        return 0

    def parse(self, settings):
        from dephp.plyparser import parser
        return 0

    def get_parser(self):
        parser = argparse.ArgumentParser()

        subparsers = parser.add_subparsers(dest="command")

        parse = subparsers.add_parser("parse")
        parse.add_argument("-t", "--to", help="Output type.")

        tokenise = subparsers.add_parser("tokenise")
        tokenise.add_argument("-t", "--to", help="Output type.",
                              choices=('tokens', 'lint'),
                              default="tokens")

        for subcommand in (parse, tokenise):
            self._add_shared_arguments(subcommand)

        return parser

    def _add_shared_arguments(self, parser):
        parser.add_argument("-v", "--verbose", action="store_true")
        parser.add_argument("-d", "--debug", action="store_true")
        parser.add_argument("file", nargs="+")

    def get_settings(self, parser, argv):
        settings = parser.parse_args(argv)

        unreadable = [name for name in settings.file
                      if not os.access(name, os.R_OK)]

        if unreadable:
            raise CliError("unreadable file: {0!r}".format(unreadable))

        return settings

    def setup_logging(self, settings):
        if settings.debug:
            level = logging.DEBUG
        elif settings.verbose:
            level = logging.INFO
        else:
            level = logging.WARNING

        logging.basicConfig(format="%(asctime)s %(levelname)s %(message)s",
                            level=level,)

def dephp_main():
    """Entry point for running dephp."""
    sys.exit(DephpRunner().run())
