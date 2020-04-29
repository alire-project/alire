# Catalog format specification

## Big picture

Each project is described as a TOML file. For instance: `aaa.toml` for the AAA
project, `gnatcoll.toml` for the package corresponding to GNATCOLL.

Each TOML description contains an object that has a `general` table and then
one table per release.

```toml
[general]
...

['1.0']
...

['1.1']
...

['2.0']
...
```

`general` contains information common to all releases, then each release
provides sources (`origin`) and can refine common information.

## Information encoding

This section describes the various encodings used in this format to encode
information.

First, there are two kinds of data: atomic and composite data.

Atomic data designates values that cannot be decomposed. There are only two
atomic data types:

 - mere strings (`"Hello, world!"`);
 - booleans (`true`, `false`);

We can then split composite data in two kinds: lists (TOML's arrays) and
mappings (JSON's tables). Lists are just sequences of other values; for
instance a list of strings:

```toml
["A", "B"]
```

Mapping are the traditional sets of associations from keys (here, always
strings) to other values. For instance, the following represents a set of
dependencies, with version constraints:

```toml
libfoo = "^1.2"
libbar = "^2.0 & /=2.1.3" # Excluding a known bad version
```

In some contexts, information can be dynamic: special encodings can be used to
make data vary depending on the environment (OS, architecture, ...). The
environment is represented as a set of specific variables which can have a
specific set of values: see the [Parameters](#parameters) section below for a
comprehensive list.

To create atomic values in an environment-dependent way, use the following
construct (here to create a boolean):

```toml
{'case(distribution)' = {
    'debian|ubuntu': true,
    '...': false
}}

# Or in a more idiomatic TOML syntax
['case(distribution)']
'debian|ubuntu' = true
'...' = false
```

Depending on the value of the `distribution` environment variable, this will
return `true` (its value is `debian` or `ubuntu`) or `false` (for other
values).

A little variation allows to build environment-dependent composite data. For
instance, to make the dependency on `libbar` above dynamic:

```toml
{
    "libfoo": "^1.2",
    "case(os)": {
        "linux": {"libbar": "^2.0"},
        "windows": {"libwinbar": "^3.0"},
        "...": {}
    }
}

# Or in a more idiomatic TOML syntay
libfoo = "^1.2"

['case(os)'.linux]
libbar = "^2.0"

['case(os)'.windows]
libwinbar = "^3.0"

['case(os)'.'...']
```

The `case(os)` part selects dependencies depending on the value of the `os`
environment variable.

If the `os` environment variable contains `linux`, this will create the
following dependencies:

```toml
libfoo = "^1.2"
libbar = "^2.0"
```

If the `os` environment variable contains `windows`, this will create the
following dependencies:

```toml
libfoo = "^1.2"
libwinbar = "^3.0"
```

And finally for other `os` values:

```toml
libfoo = "^1.2"
```

## General Information

Unless specified, all the entries must be static, i.e. they cannot depend on
the context.

The `general` entry must contain an object. It contains itself the following
entries:

 - `description`: mandatory string. One-line description about the package. For
   instance:

   ```toml
   description = "Library to handle foobars"
   ```

 - `long-description`: optional free-form string to provide information about
   this package, in addition to `description`, without length restrictions.

- `authors`: optional array of strings. Flat list of human-readable names for
   the authors, i.e. the people that wrote the software that is packaged. For
   instance:

   ```toml
   authors = ["Alice Example",
              "Bob For Instance <bob@example.com>"]
   ```

 - `maintainers`: mandatory array of strings. Flat list of human-readable names
   (optional) for the maintainers, with a contact email (mandatory); i.e. the
   people that maintain the crate metadata in Alire. For instance:

   ```toml
   maintainers = ["alice@example.com",
                  "Bob For Instance <bob@athome.com>"]
   ```

 - `maintainers-logins`: mandatory array of strings. Flat list of github login
   usernames used by the maintainers of the crate. This information is used to
   authorize crate modifications. For instance:

   ```toml
   maintainers-logins = ["alicehacks", "bobcoder"]
   ```

 - `licenses`: mandatory array of strings. Flat list of licenses for the
   software that is packaged. The following licenses are allowed:

   > -   `AFL 3.0`, `AGPL 3.0`, `Apache 2.0`, `Artistic 2.0`,
   >     `BSD 2-Clauses`, `BSD 3-Clauses Clear`, `BSD 3-Clauses`,
   >     `BSL 1.0`, `CC0 1.0`, `CC BY 4.0`, `CC BY-SA 4.0`, `ECL 2.0`,
   >     `EPL 1.0`, `EPL 2.0`, `EUPL 1.1`, `EUPL 1.2`, `GPL 2.0`,
   >     `GPL 3.0`, `ISC`, `LGPL 2.1`, `LGPL 3.0`, `LPPL 1.3c`, `MIT`,
   >     `MPL 2.0`, `MS PL`, `MS RL`, `NCSA`, `OFL 1.1`, `OSL 3.0`,
   >     `PostgreSQL`, `Unlicense`, `WTFPL`, `zlib`, `GMGPL 2.0`,
   >     `GMGPL 3.0`, `Public Domain`.
   > -   `Custom`, can be used for project specific licenses.

   If the license is unknown or not in the list above, leave an empty array.

   ```toml
   licenses = []
   ```

   For a double license:

   ```toml
   licenses = ["GPL 3.0", "MIT"]
   ```

 - `website`: optional string. URL to the original project's website. For
   instance:

   ```toml
   website = "https://myproject.example.org/"
   ```

 - `tags`: optional array of strings. Flat list of topics covered by the crate.
   Tags will help users find crates related to their interests:

   ```toml
   tags = ["spark", "security"]
   ```

 - `available`: optional dynamic boolean expression. Determines whether the
   package is available for the current platform (true) or not (false). For
   instance:

   ```toml
   [available.'case(distribution)']
   'debian|ubuntu' = true
   '...' = false
   ```

 - `depends-on`: optional dynamic dependencies expression common to all
   releases. For instance:

   ```toml
   [depends-on]
   libfoo = "^1.2"

   [depends-on.'case(os)'.linux]
   libbar = "^2.0"

   [depends-on.'case(os)'.windows]
   libwinbar = "^3.0"
   ```

   Available constraint operators are the usual Ada ones (`=`, `/=`, `>`, `>=`,
   `<`, `<=`) plus caret (`^`, any upwards version within the same major point)
   and tilde (\~, any upwards version within the same minor point). Logical
   operators for and (&), or (|) are accepted; see the `Semantic_Versioning`
   project documentation on [extended version
   sets](https://github.com/alire-project/semantic_versioning#types).

 - `project-files`: optional list of strings. Each is a path, relative to the
   root of the source directory, to a project file to be made available.
   Expressions are accepted. For instance:

   ```toml
   project-files = ["my_project.gpr", "utils/utils_for_my_project.gpr"]

   [project-files.'case(word-size)']
   bits-64 = ["my_project.gpr"]
   bits-32 = ["my_project32.gpr"]
   ```

 - `gpr-externals`: optional table, giving a mapping from the name of external
   variables in the project files to sets of possible values (as array of
   strings), or an empty string if this set is infinite. For instance:

   ```toml
   [gpr-externals]
   BUILD_MODE = ["debug", "profile", "release"]
   TAG = ""
   ```

 - `gpr-set-externals`: optional table, giving a mapping from the name of
   external variables to the values to use by default when building the
   project. Expressions are accepted before the mapping. For instance:

   ```toml
   [gpr-set-externals]
   BUILD_MODE = "release"

   [gpr-set-externals.'case(os)']
   linux   = { OS = "gnu-linux" } # Compact table syntax is convenient in this case
   windows = { OS = "ms-linux" }  # to see all enumeration values, one per row.
   ```

 - `environment`: optional table used to modify environment variables that
   will apply at build time. Variables and values are specified with the form
   `VARIABLE.<action> = "value"`, where `<action>` is one of `append`,
   `prepend`, or `set`. For instance:

   ```toml
   [environment]
   C_INCLUDE_PATH.append = "/usr/include/something"
   PATH.prepend = "${CRATE_ROOT}/bin"
   MSYS.set = "${MSYS_ROOT}"
   ```

   Other environment variables may appear in the value, and will be replaced.
   Furthermore, a few predefined variables are provided by Alire:

   - `${CRATE_ROOT}` is the deployment directory of the crate.
   - `${MSYS_ROOT}` is available on Windows to obtain the detected location of
   an MSYS2 installation.

   Environment entries can use dynamic expressions:

   ```toml
   [environment.'case(os)']
   windows = { C_INCLUDE_PATH.append = "${MSYS_ROOT}/mingw64/include/SDL2" }
   ```

 - `executables`: optional list of strings. Each one is the simple name of an
   executable provided by the package. Executables are looked for by `alr` in
   the build tree and must not include a path. If only one executable is given,
   it is considered the default for `alr run`. For instance:

   ```toml
   executables = ["my_main"]
   ```

 - `actions`: optional list of actions to perform when installing this package.
   The general action syntax is:

   ```toml
   [[actions]]
   type = <kind>
   command = <command>
   ```

   `<command>` is a an array of strings for a shell command to run in the
   source directory. `<kind>` can be either:

   - `post-fetch`: the command is to be run right after getting the package
      sources

   - `post-compile`: the command is to be run right after GPRbuild has been
      run

   - `test`: the command is run on demand for crate testing within the Alire 
      ecosystem (using `alr test`).

   Actions accept dynamic expressions. For example:

   ```toml
   [[general.actions.'case(os)'.linux]]
   type = "post-fetch"
   command = ["make"]

   [[general.actions.'case(os)'.windows]]
   type = "post-fetch"
   command = ["cmd", "build"]

   [[general.actions.'case(os)'.'...']]
   # An explicit empty case alternative, which is not mandatory
   ```

## Release-specific information

Each release is materialized as an entry in the top-level object. The key is a
string for the version number for the release, while the value is an object to
contain the release-specific information. This object can contains the
following entries:

 - `origin`: mandatory dynamic string expression. URL used to fetch the sources
   to build. For instance:

   ```toml
   # Clone a git repository
   origin = "git+https://github.com/example-user/example-project"

   # Download and extract a source archive
   origin = "https://example.org/archive.tar.gz"
   ```

   If the package only maps a package from the system package manager,
   (for instance `make`), use:

   ```toml
   origin = "native:make"
   ```

   Make the expression evaluate to an empty string to mean that the package is
   not available, or just leave the alternative out. For instance, to state
   that `make` is available on Debian/Ubuntu and not on the other platforms:

   ```toml
   [origin.'case(distribution)']
   'debian|ubuntu' = "native:make"
   ```

 - `origin-hashes`: mandatory string array for git origins and source archives.
   An array of "kind:digest" fields that specify a hash kind and its value.
   Kinds accepted are: `sha512`.

 - `archive-name`: optional string. If `origin` points to a source archive,
   this can specifiy the name of the file to download, which is needed in order
   to properly extract the sources. For instance:

   ```toml
   origin = "https://example.org/0123456789"
   archive-name = "archive.tar.gz"
   archive-hash = "sha512:bf6082573dc537836ea8506a2c9a75dc7837440c35c5b02a52add52e38293640d99e90a9706690591f8899b8b4935824b195f230b3aa1c4da10911e3caf954c04ac"
   ```

 - `available`: optional dynamic boolean expression. It is used in the
   following way:

   1. If it evaluates to `false`, the package is not available for the current
      platform.

   1. Otherwise, the availability is determined by the `available` entry in the
       `general` section.

 - `notes`: optional string. Provides miscellanous information about this
   release. For instance:

   ```json
   notes = "Experimental version"
   ```

It can also contain the following entries: `depends-on`, `project-files`,
`gpr-externals`, `gpr-set-externals`, `executables`, `actions`. These are
optional. For atomic values, these override the ones from `general`, and for
lists/mappings, they are interpreted as additions. In the latter case,
conflicting entries are considered as errors.

## External releases

The above information applies to regular releases distributed from sources
(that is, the Ada projects whose distribution is the main Alire goal). Some
special supporting releases also exist that are described differently.

A release is considered "external" when it is not built from sources and,
furthermore, its semantic version cannot be known until run time. Hence, the
availability and version of these releases is detected by `alr`.

Several definitions for these external releases may exist, and so they are
defined in the index as a vector with key `external`:

```toml
[[external]]
# Common entries to all externals
kind = "hint" # One of several predefined external kinds
hint = "Please install SDL in your platform from source or system packages"
# Specific external kind parameters might follow
```

All external kinds can define these regular properties:

 - `available`: when defined, it restricts the external detection to the given
   environment conditions.

 - `hint`: explanation for the user on how to make the external available. This
   explanation is shown on request with `alr show --external`, or after
   `alr get`, for any external dependency that could not be detected. This
   property accepts dynamic expressions.

### External kinds: hints

A plain undetectable external intended to simply serve as a hint. For
crates that are known to be unavailable through Alire, it serves to
provide a generic or customized hint to the user. It has no specific
fields, other than the common ones just described. Its key is `"hint"`:

```toml
[[external]]
kind = "hint" # Identifies this external kind
# Bare minimum external. Optionally, the hint/available fields can be used.
```

### External kinds: command-line tools

This external kind is used to describe commands that can be run in the system,
and that are able to provide their own version via some particular invocation.
Their specific fields are (all mandatory):

```toml
kind = "version-output" # Identifies this external kind

version-command = ["gnat", "--version"]
# Invocation that will provide the version when the tool is available

version-regexp  = "^GNAT ([\\d\\.]+).*|^GNAT Community ([\\d]{4}).*"
# TOML-escaped GNAT.Regpat-compatible regular expression. Parenthesized
# matches will cause the matched expression to be parsed as the Semantic
# Version of the tool.
```

### External kinds: system packages

Systems that have their own package manager (e.g. Linux) can readily provide
many complex dependencies still unpackaged as source code in Alire. Alire can
use these on supported platforms (at this time, Debian & Ubuntu. Do not
hesitate to contact us if you would like to maintain other distributions)
during resolution.

A system external gives a list of platform package names that supply the
dependency natively. The platform package manager will be used to detect their
availability and version. To that effect, the `origin` field is used (which can
accept dynamic expressions in this context):

```toml
kind = "system" # Identifies this external kind
origin = ["libncursesada3", "libncursesada5"]
# As versions appear this list will grow. To speed up detection, dynamic
# expressions may become recommendable for certain system packages.
```

For Ada precompiled system libraries that require the platform compiler for
linking (e.g., in Debian/Ubuntu), and that cannot be used with other GNAT
compilers, this should be expressed with the `available` property, e.g.:

```toml
available.'case(toolchain)'.user = false
# `available` defaults to true, so it is enough to flag the user toolchains
```

## Parameters

 - `os`: name of the OS. Currently supported values are: `linux`, `macos` and
   `windows`.

 - `distribution`: name of the Linux distribution, or `none` if running on a
   different OS. Currently supported values are: `debian`, `ubuntu`.

 - `toolchain`: takes `system` value in distributions with the system Ada
   compiler first in PATH (GNAT FSF in Debian/Ubuntu), `user` otherwise (GNAT
   Community editions, other cross-target toolchains).

 - `word-size`: architecture word size. Currently supported values are:
   `bits-32`, `bits-64`, `bits-unknown`

## Further reading ##

You can inspect [index files](https://github.com/alire-project/alire-index) to
get an idea of how projects are included into the catalog.
