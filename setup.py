#!/usr/bin/python
import os, glob
from setuptools import setup, find_packages

setup(name="dephp", version="1.0.0",
      setup_requires=['nose>=1.0'],
      description="PHP Parser.",
      long_description=open('README.rst').read(), license="MIT",
      author="James Webber", author_email="bunkerprivate@gmail.com",
      packages=find_packages(exclude=["tests", "tests.*"]),
      entry_points={
          'console_scripts': [
              'dephp = dephp.cli:dephp_main',
           ],
      },
      url="http://github.com/bnkr/dephp",
      # Doesn't seem to work.
      test_suite='nose.collector')
