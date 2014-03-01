import sys, argparse, logging

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
        return 0

    def parse(self, settings):
        return 0

    def get_parser(self):
        parser = argparse.ArgumentParser()

        subparsers = parser.add_subparsers(dest="command")

        parse = subparsers.add_parser("parse")
        parse.add_argument("-t", "--to", help="Output type.")

        tokenise = subparsers.add_parser("tokenise")
        tokenise.add_argument("-t", "--to", help="Output type.")

        for subcommand in (parse, tokenise):
            self._add_shared_arguments(subcommand)

        return parser

    def _add_shared_arguments(self, parser):
        parser.add_argument("-v", "--verbose", action="store_true")
        parser.add_argument("-d", "--debug", action="store_true")

    def get_settings(self, parser, argv):
        settings = parser.parse_args(argv)
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
