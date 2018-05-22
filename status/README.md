# BUILD TESTS

These text files reflect the build results of releases during the continuous integration check.

The name of the file reflects that of the docker image used to test `alr`. Those without an explicit operating system are Debian testing.

At the top of each file are the details of the platform as detected by `alr`.

The possible status for each milestone is:

- ![green](https://placehold.it/8/00aa00/000000?text=+) PASS: the release was built normally.
- ![yellow](https://placehold.it/8/ffbb00/000000?text=+) UNAV: the release is either disabled or some of its dependencies missing in that platform.
- ![red](https://placehold.it/8/ff0000/000000?text=+) FAIL: the release failed to build. This should not happen in stable `alr` builds and will be looked at ASAP.
- ![red](https://placehold.it/8/ff0000/000000?text=+) ERR: `alr` itself found erred unexpectedly while attempting to build the release. Same considerations as in FAIL apply.