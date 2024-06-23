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

Available for Linux/macOS/Windows/FreeBSD/OpenBSD.

Download the latest stable version from the [Releases](https://github.com/alire-project/alire/releases) page. See the [Getting Started](doc/getting-started.md) guide for binary downloads.

If, instead, you want to test the latest development version, see [Building from sources](#building-from-sources) or, if you already have a recent `alr` in your system, [Building with `alr`](#building-with-alr).

## Installation and First Steps ##

See the [Getting Started](doc/getting-started.md) guide.

## Building from sources ##

The build process of `alr` is straighforward and depends only on a recent GNAT Ada 2012 compiler. All dependencies are included as submodules. A project file (`alr_env.gpr`) is provided to drive the build with all necessary configuration (which is also valid for editing with GNAT Studio).

The ALIRE_OS environment variable must be set to the OS for which `alr` is being build, taking one of the values in `freebsd`, `openbsd`, `linux`, `macos`, `windows`.

Follow these steps:

1. Clone the repository: `git clone --recurse-submodules https://github.com/alire-project/alire.git`
1. Enter the cloned repository folder.
1. Build the executable:
   * if you have Bash on your system: `dev/build.sh`
   * if you don't have Bash on your system: `ALIRE_OS=<one of: freebsd, openbsd, linux, macos, windows> gprbuild -j0 -p -P alr_env`

The binary will be found at `bin/alr`. You can run `alr version` to see version and diagnostics information.

Sourcing the `scripts/alr-completion.bash` file will provide Bash tab autocompletion.

## Building with `alr`

If you already have a recent enough `alr` binary, you can alternatively build
`alr` by simply running `alr build` at the root of the repository. This command
will retrieve all necessary dependencies prior to launching the build and 
configure the environment.

The master branch should normally be able to build itself in this fashion, as
this is one of our integration tests.

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

Alire can be built on Linux, macOS, Windows, FreeBSD, and OpenBSD.

Alire requires a recent Ada 2012 compiler. In practice, this currently means
the latest [GNAT Community](https://www.adacore.com/download) or a somewhat
recent GNAT FSF. Continuous integration is run against the Windows and macOS
Github Actions images, and a suite of Linux [docker
images](https://github.com/alire-project/alire/blob/538a3549a1dbbc6c09728cb987c71187578381b2/.github/workflows/ci-docker.yml#L20)
that includes at least Debian stable, Ubuntu LTS, CentOS, Arch and Fedora. The
packaged GNAT is used when available from the distribution.

Note that platform-provided Ada libraries (such as Debian's GtkAda) require the
use of the platform Ada compiler. Otherwise these libraries will be
unavailable, potentially making dependent crates unavailable too.
