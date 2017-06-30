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

Developing and Installing
-------------------------

Optionally set up a virtual environment.  This isolates dependencies and means
dephp won't conflict with anything else (unless your system python
changes)::

  $ cd dephp
  $ virtualenv venv

Required steps start here.  If you didn't make a virutalenv then use your system
python instead of the one in the virtualenv::

  $ ./venv/bin/pip install -r requirements.txt

Tests are using nose so the if you want to run through ``setup.py`` you need to
use the ``nosetests`` command, not the ``test`` command.
