Dephp
=====

Dephp is a parser for the PHP grammar written in python using ply.  It is based
on work done in phply (https://github.com/ramen/phply) updated to use the same
grammar specification that PHP does.

It doesn't do very much useful at the moment but it is interesting to understand
the PHP grammar, and could be used in the future for static analysis and
conversions.

Usage
-----

Little to say at this point, but you can see parser debug output::

  $ bin/dephp parse tokenise.php -d -t noop
