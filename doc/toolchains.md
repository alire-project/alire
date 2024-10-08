# Toolchain management

Toolchains are comprised by a GNAT compiler and a `gprbuild` project file
processor. Alire strives to simplify the availability of GNAT releases, which
are packaged to be retrieved and used with ease.

The user can now select a preferred compiler via `alr toolchain --select`.
Releases may still override this selection (for example to use a
cross-compiler). Several compilers can be installed simultaneously, but only
the last one `--select`ed will be used by default. The rest will be used in
preference to uninstalled compilers, if the default one does not match some
crate's dependencies.

There are two kinds of dependencies on GNAT compilers: generic dependencies on
the `gnat` crate, which apply to every compiler, and dependencies on a precise
native or cross-compiler.

The native compiler for each platform is always `gnat_native`, whereas
cross-compilers follow the naming convention `gnat_<target>` (e.g.,
`gnat_riscv_elf`). Alire will only offer to install valid cross-compilers for
the host platform.

## Identifying available compilers

Compilers available for download can be listed with
`alr search --full --external-detect gnat_`.
They will also be shown by the selection assistant,
`alr toolchain --select`.

Running `alr toolchain` without arguments will show the installed compilers and
the preferred compiler, if any.

## Automatic compiler installation

If a crate requires a target-specific compiler via its dependencies, `alr` will
attempt to use first a matching installed compiler. Otherwise, a matching
compiler will be automatically downloaded.

## Automatic compiler selection by Alire

When a build is launched, or `alr printenv` is run to obtain a build environment,
`alr` will use the available compilers as follows:

1. A target-specific compiler needed due to dependencies will be
selected.
1. Otherwise, if the user has defined a preferred compiler, it will be
selected.
1. If no preferred compiler has been defined, Alire will rely on the existing
user environment.

Within these conditions, a compiler already downloaded will be preferred.
Downloading a new compiler will be attempted only if no matching compiler is
already available.

## Specifying dependencies on GNAT compilers for crate publishing

From the point of view of a user preparing a release for publication, these
are the considerations to bear in mind:

- Do not use any dependency on a compiler if you do not have a reason to do so.
- Use dependencies on `gnat` to specify known compiler version restrictions.
- Use dependencies on a precise gnat cross-compiler (e.g., `gnat_riscv_elf`)
  when your crate is purposely only available for such a target.
- It is normally not necessary to specify a dependency on the native compiler
  (`gnat_native`) as that would unnecessarily limit the availability of a
  library crate that might be useful to dependent cross-target crates.
- It may be useful to depend on `gnat_native` in cases where a crate builds a
  tool to be used always in the host platform, for example to be used in some
  action during the build process.

## Note about cross-compilation

Independently of the compiler made available by `alr` in the environment, the
crate project file still must define an appropriate `Target` attribute for the
desired compiler. At the moment, Alire does not examine project file contents
to identify necessary compilers, and relies only on regular `depends-on`
dependencies.
