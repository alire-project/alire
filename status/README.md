# BUILD TESTS

These files reflect the build results of releases during the continuous integration check.

The name of the file reflects that of the docker image used to test `alr`. Those without an explicit operating system are Debian testing.

At the top of each file are the details of the platform as detected by `alr`.

The possible status for each milestone is:

- ![green](https://placehold.it/8/00aa00/000000?text=+) PASS: the release was built normally.
- ![yellow](https://placehold.it/8/ffbb00/000000?text=+) UNAV/DEPS: the release is either disabled or some of its dependencies are missing in that platform.
- ![red](https://placehold.it/8/ff0000/000000?text=+) FAIL/ERR: either the release failed to build or `alr` itself suffered an unexpected error while attempting the test. This should not happen in stable `alr` builds and will be looked at ASAP.
