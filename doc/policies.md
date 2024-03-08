# Policies

## Crate ownership

Because Alire comes late in the history of the Ada and SPARK languages we will
not apply a strict "first come, first served" policy on crate names. At least for
the first months or years, we allow ourselves a degree of control on the
projects/crates published in the index, with the following goals:

- Long term support: Owner and maintainers of a project are most likely in the
  best position to maintain the corresponding Alire crate.
- Respect the ownership of projects: Owner and maintainers of a project deserve
  to be credited for their work.
- Avoid user confusion on the names of crates: Crate names should be clear with
  regard to the project they contain. For instance, do not try to impersonate
  existing crates or projects.

To that end we will potentially reject a crate or transfer the ownership of
a crate.

We count on the goodwill of the contributors to help us conduct this moderation
in a kind and courteous way. Do not submit a crate to the Alire index if you
are not willing to comply with this policy.

As the Alire project matures, we expect to do less moderating and potentially
remove this policy in favor of a "first come, first served" policy.

## Release immutability

A release (identified by a unique semantic version) is protected against
changes by its integrity hashes. If errors are identified post-publication, a
release could be withdrawn, or superseded by a new one (using the appropriate
major/minor/patch/build version changes), but not modified.

## Best practices

 - Avoid using `ada` as a prefix for your crate name, this will make the
   project harder to find in a list. `ada` suffix is ok when the project is a
   binding for an existing library (e.g. `sdlada`, `gtkada`).

 - Split big projects in multiple crates:

    - If your project is a collection of components (like GNATcoll for
      instance) and each component has different dependencies, you should
      consider splitting the collection into multiple Alire crates. The
      components can still be maintained in the same repository and use the
      same release archive/commit (e.g.
      [gnatcoll_sqlite](https://alire.ada.dev/crates/gnatcoll_sqlite),
      [gnatcoll_sql](https://alire.ada.dev/crates/gnatcoll_sql),
      [gnatcoll_postgres](https://alire.ada.dev/crates/gnatcoll_postgres)).

    - If your project is an application/executable/tool, some parts of the
      application may be interesting on their own and could benefit the
      ecosystem. For instance a parser for a standard file format would be
      useful across projects.

- Separate supporting projects into nested crates, in particular when
  developing libraries:

  - Tests, demos, examples, etc., can be provided in nested crates so they can
    be conveniently used locally when needed without causing extra build load
    on clients. See the documentation on
    [local pins](catalog-format-spec#using-pins-for-crate-testing) for
    details.

  - The manifests of these nested crates need not to be published to the
    community index if they do not provide an application of general interest.

 - GPR project file clashes: to prevent issues when combining the GPR project
   files of different crates, we recommend to follow the rules below:

    - Use a project file name that matches the name of the crate (e.g.
      `my_crate.gpr` for a crate named `my_crate`)

    - Avoid using multiple GPR project files for a single crate

    - Avoid using common names for GPR project files such as `shared.gpr`,
      `common.gpr`, `config.gpr`, etc.

    - Prefix GPR scenario variables with the name of your crate:
      ```
      Build_Mode := External ("MY_CRATE_BUILD_MODE", "release");
      ```

    - Avoid common names for GPR scenario variables such as `OS`, `TARGET`,
      `BUILD_MODE`, `MODE`, etc.

    - For library projects, do use the "standard" `LIBRARY_TYPE` external, but
      wrap it in a crate specific external:

      ```
      type Library_Type_Type is ("relocatable", "static", "static-pic");

      Library_Type : Library_Type_Type :=
        external ("MY_CRATE_LIBRARY_TYPE", external ("LIBRARY_TYPE", "static"));

      for Library_Kind use Library_Type;
      ```
      Having the `MY_CRATE_LIBRARY_TYPE` external will allow users to override
      the value `LIBRARY_TYPE` just for this crate, if need be.

## Source code

 - If you use non-ASCII characters in the source code, then use UTF-8 encoding
   for the sources and add `-gnatW8` to compiler options (as provided by
  `alr init` command). Other crates can use `-gnatW8` and this means the
   compiler will read your crate sources as UTF-8 and the run-time library
   will use UTF-8 for text I/O, so make sure your crate is OK with this.

 - Consider to follow or at least familiarize yourself with the
   [Ada Style Guide](https://ada-lang.io/docs/style-guide/Ada_Style_Guide).
