Alire/ALR's testsuite
=====================

This directory intends to host a comprehensive testsuite for Alire/ALR as a
library/tool. The testsuite framework currently requires a Python 3
interpreter with the [e3-testsuite](https://e3-testsuite.readthedocs.io) and
[pexpect](https://pexpect.readthedocs.io) packages (from PyPI) installed.

You also must have [GNAT](https://www.gnu.org/software/gnat) and
[GPRBuild](https://github.com/AdaCore/gprbuild) in your `PATH`. You can install
these with, for example:
```sh
alr toolchain --install --install-dir=<dir> gnat_native=<version_x> gprbuild=<version_y>
```
and add `<dir>/gnat_native_<version_x>_(...)/bin` and
`<dir>/gprbuild_<version_y>_(...)/bin` to your `PATH`.

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
$ pip install e3-testsuite pexpect

# You should now be able to run the testsuite (make sure you built alr and
# made it available with your PATH):
$ ./run.py
```

# Creating tests
All tests are based on running a Python script. There are these test drivers:

- `python-script`: run in host in both sandboxed and shared build mode.
    - The build mode can be narrowed down with the `build_mode` attribute.
- `docker-wrapper`: run in a pristine docker Ubuntu image in shared build mode.

# Environment variables
The following variables can be used to modify testsuite behavior.

For `ALIRE_TESTSUITE_DISABLE_*` variables, their mere existence activates their
function, no matter their value, or lack of one.

- `ALIRE_TESTSUITE_DISABLE_DISTRO`: when defined, `alr` will be configured
 to not detect the system distribution and fall back to unknown distribution.

- `ALIRE_TESTSUITE_DISABLE_DOCKER`: when defined, `alr` will skip tests that
  require Docker (which are enabled by default if Docker is detected).

- `ALIRE_TESTSUITE_DISABLE_NETWORK_TESTS`: when defined, tests that
  require non-local network use will be skipped.

- `ALIRE_TESTSUITE_ENABLE_LOCAL_TESTS`: when defined, tests that are intended
  to be run locally only by the Alire developer team will not be skipped.

Example disabling Docker tests for a single run on Bash:
```Bash
$ ALIRE_TESTSUITE_DISABLE_DOCKER= ./run.sh
```
