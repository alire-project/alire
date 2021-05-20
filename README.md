[![Linux CI](https://github.com/alire-project/alire/workflows/CI%20linux/badge.svg)](https://github.com/alire-project/alire/actions)
[![Windows CI](https://github.com/alire-project/alire/workflows/CI%20Windows/badge.svg)](https://github.com/alire-project/alire/actions)
[![MacOS CI](https://github.com/alire-project/alire/workflows/CI%20macOS/badge.svg)](https://github.com/alire-project/alire/actions)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/ada-lang/Alire)
[![Gitpod ready](https://img.shields.io/badge/Gitpod-ready-908a85?logo=gitpod)](https://gitpod.io/#https://github.com/alire-project/alire)

# ALR #

ALIRE: Ada LIbrary REpository.

A catalog of ready-to-use Ada libraries plus a command-line tool (`alr`) to
obtain, build, and incorporate them into your own projects. It aims to fulfill
a similar role to Rust's `cargo` or OCaml's `opam`.

### Caveat emptor ###

Documentation at this time is a work in progress. Expect further efforts in
this direction until this warning is removed. Check the latest information at
https://alire.ada.dev/

## TL;DR ##

Available for Debian stable / Ubuntu >=17.10 / macOS / Windows

Download the latest stable version from the [Releases](https://github.com/alire-project/alire/releases) page. See the [Getting Started](doc/getting-started.md) guide for binary downloads.

If, instead, you want to test the latest development version, see [Building from sources](#building-from-sources).

## Installation and First Steps ##

See the [Getting Started](doc/getting-started.md) guide.

## Building from sources ##

The build process of `alr` is straighforward and depends only on a recent GNAT Ada 2012 compiler. All dependencies are included as submodules. A project file (`alr_env.gpr`) is provided to drive the build with all necessary configuration (see the macOS extra step below, though).

Follow these steps:

1. Clone the repository: `git clone --recurse-submodules https://github.com/alire-project/alire.git`
1. Enter the cloned repository folder.
1. Only on macOS: define the environment variable `OS=macOS`
1. Build the executable: `gprbuild -j0 -P alr_env`

The binary will be found at `bin/alr`. You can run `alr version` to see version and diagnostics information.

Sourcing the `scripts/alr-completion.bash` file will provide bash tab autocompletion.

## Design principles ##

alr is tailored to userspace, in a similar way to Python's virtualenv. A
project or workspace will contain all its dependencies.

Some crates benefit from using platform packages. In this case the user
will be asked to authorize a `sudo` installation through the platform package
manager.

Properties and dependencies of projects are managed through a TOML file
(`alire.toml`, found at the root of Alire workspaces). This file exists locally
for working copies of projects, and the Alire community index stores the files
corresponding to its projects.

The complete build environment is automatically set up by setting the
GPR_PROJECT_PATH environment variable before running `gprbuild`, thus freeing
the user from concerns about installation paths. The user simply adds the used
projects to its own project GPR file with their simple name. You can check the
environment `alr` is using with `alr printenv`.

## Supported platforms ##

Alire requires a recent Ada 2012 compiler. In practice, this currently means
[GNAT Community](https://www.adacore.com/download) or GNAT FSF 7.2 onward. The
continuous integration checks are run against Debian stable, Ubuntu LTS,
Windows and MacOS.

Note that platform-provided Ada libraries (such as Debian's GtkAda) require the
use of the platform Ada compiler. Otherwise these libraries will be
unavailable, potentially making dependent crates unavailable too.
