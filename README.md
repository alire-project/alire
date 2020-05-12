[![Linux CI](https://github.com/alire-project/alire/workflows/CI%20linux/badge.svg)](https://github.com/alire-project/alire/actions)
[![Windows CI](https://github.com/alire-project/alire/workflows/CI%20Windows/badge.svg)](https://github.com/alire-project/alire/actions)
[![MacOS CI](https://github.com/alire-project/alire/workflows/CI%20macOS/badge.svg)](https://github.com/alire-project/alire/actions)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/ada-lang/Alire)

# ALR #

ALIRE: Ada LIbrary REpository.

A catalog of ready-to-use Ada libraries plus a command-line tool (`alr`) to
obtain, build, and incorporate them into your own projects. It aims to fulfill
a similar role to Rust's `cargo` or OCaml's `opam`.

### Caveat emptor ###

Documentation at this time is minimal. Expect further efforts in this direction
until this warning is removed.

## TL;DR ##

Available for Debian stable / Ubuntu >=17.10 / macOS / Windows

`alr` is undergoing frequent changes in preparation for a first publicly
announced beta. Hence, the current recommendation is to run the latest `master`
branch version, and double-check in case of problems that no new PRs have been
merged since your last compiled version.

To see available crates per platform/compiler, see the
[alire-crates-ci](https://github.com/alire-project/alire-crates-ci) companion
repository.

## Installation and First Steps ##

See the [Getting Started](doc/getting-started.md) guide.

## Design principles ##

alr is tailored to userspace, in a similar way to Python's virtualenv. A
project or workspace will contain all its dependencies.

At this time some projects require platform packages. In this case the user
will be asked to authorize a `sudo` installation through the platform package
manager.

Properties and dependencies of projects are managed through a TOML file. This
file exists locally for working copies of projects, and the Alire community
index stores the files corresponding to its projects.

The complete build environment is set up by setting the GPR_PROJECT_PATH
environment variable before running `gprbuild`, thus freeing the user from
concerns about installation paths. The user simply adds the used projects to
its own project GPR file with their simple name.

## Supported platforms ##

Alire requires a recent Ada 2012 compiler. In practice, this currently means
[GNAT Community](https://www.adacore.com/download) or GNAT FSF 7.2 onward. The
continuous integration checks are run against Debian stable, Ubuntu LTS,
Windows and MacOS.

Note that platform-provided Ada libraries (such as Debian's GtkAda) require the
use of the platform Ada compiler. Otherwise these libraries will be
unavailable, potentially making dependent crates unavailable too.
