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

Since not everyone uses buildout I'll explain it quickly.

Optionally set up a virtual environment.  This isolates dependencies and means
dephp won't conflict with anything else (unless your system python
changes)::

  $ cd dephp
  # --no-site-packages might be needed on older versions of virtualenv
  $ virtualenv venv
  # Sometimes not necessary but doesn't hurt.
  $ ./venv/bin/pip install -U setuptools

Required steps start here.  If you didn't make a virutalenv then use your system
python instead of the one in the virtualenv::

  # Download buildout
  $ ./venv/bin/python bootstrap.py
  # Install dependencies into ./eggs
  $ ./bin/buildout

The `./bin/python` script is now a python which will use your virtualenv and
also the local eggs downloaded by buildout.

You can now run dephp without messing with your system at all.  The eggs are
re-locateable so if you re-write the `sys.path` changes you can package the
entire tree as a .deb or .rpm package if you want.
