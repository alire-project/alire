# Toolchain management

Toolchains are comprised by a GNAT compiler and a `gprbuild` project file
processor. Alire strives to simplify the availability of GNAT releases, which
are packaged to be retrieved and used with ease. Still, the compiler preferred
by the user might not be appropriate for some crates, which may cause subtle
interactions that this section explains.

Some crates may require particular GNAT compilers (for example for
cross-compilation). Note that, independently of the compiler finally made
available by `alr` in the environment, the crate project file still must define
an appropriate Target attribute for the desired compiler. At the moment, Alire
does not examine project file contents to identify necessary compilers, and
relies only on regular `depends-on` dependencies.

There are two sides to toolchain use by `alr`. On the one hand, a solution may
or may not have dependencies on GNAT compilers. On the other hand, the user may
or may have not selected a default toolchain for use via `alr toolchain
--select`. The interaction between these two features is explained next.

Before going into the details, there are two kind of dependencies on GNAT
compilers: generic dependencies on the `gnat` crate, which apply to every 
compiler, and dependencies on a precise native or cross-compiler, e.g.,
`gnat_native` or `gnat_riscv_elf`.

## Identifying available compilers

Available compilers can be listed with `alr search --full --external-detect
gnat_`. They will also be shown by the selection assistant, `alr toolchain
--select`.

## Specifying dependencies on GNAT compilers

From the point of view of a user preparing a release for publication, these
are the considerations to bear in mind:

- Do not use any dependency on a compiler if you do not have a reason to do so.
- Use dependencies on `gnat` to specify known compiler version restrictions.
- Use dependencies on a precise gnat cross-compiler (e.g., `gnat_riscv_elf`)
  when your crate is purposely only available for such a target.
- There is no reason to specify a dependency on the native compiler
  (`gnat_native`) as that would unnecessarily limit the availability of a
  library crate that might be useful to dependent cross-target crates.

## Interactions with a selected toolchain

From the point of view of a user wanting to compile some release, there will be
an interaction between the solution dependency on compilers and a selected
default compiler. 

The simplest and most usual scenario for native compilation is as follows:

- Solution without dependencies on `gnat`: 
   - If a default compiler has been configured: the compiler will be included in the
     PATH provided by `alr printenv` and used for the build.
   - No compiler has been configured: `alr` will attempt to use whatever toolchain is
     available in the user's environment prior to running `alr`. 
      - Note that compilers deployed with `alr toolchain --install` but not
        selected as the default will not be used in this case.

For crates with dependencies on GNAT compilers, the following two cases simultaneously apply:

- Solution with generic dependencies on `gnat`: The solver will provide a
  concrete GNAT to satisfy this dependency, applying the following
  prioritization:
   1. A precise compiler that is also a dependency.
   1. The default compiler, if it has been defined.
   1. A native compiler that is already deployed.
   1. A cross-compiler that is already deployed.
   1. A native compiler from the index, that will be deployed with the rest of
      dependencies.

- Solution with dependencies on a precise GNAT (`gnat_native`, `gnat_ricv_elf`,
  etc.): The `alr` solver will provide one compiler in the environment, applying the
  following prioritization:
   1. A matching compiler that is already deployed, explicitly via `alr
      toolchain --select`/`alr toolchain --install`, or implicitly during a previous
      solving. 
   1. A compiler that satisfies the dependency available in the catalog,
      which will be deployed as part of regular dependency retrieval.
