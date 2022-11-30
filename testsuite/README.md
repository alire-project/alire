Alire/ALR's testsuite
=====================

This directory intends to host a comprehensive testsuite for Alire/ALR as a
library/tool. The testsuite framework currently requires a Python 3 interpreter
with the e3-testsuite package (from PyPI) installed.

Assuming your environment already has a Python 3 interpreter and has
`virtualenv` installed, here is a quick cookbook to run the testsuite:

```sh
# Create a virtualenv (prefix to install packages)
# The exact command varies from one Linux distribution to another:
# virtualenv, virtualenv3, virtualenv-3.10, ...
$ virtualenv my-virtual-env

# Update your environment to use it
$ source my-virtual-env/bin/activate

# Install e3-testsuite and all its dependencies
$ pip install e3-testsuite

# You should now be able to run the testsuite (make sure you built alr and
# made it available with your PATH):
$ ./run.py
```
