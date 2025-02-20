# Catalog format specification

## Big picture

Each release belonging to a crate is described as a TOML file. This file has
minor differences depending on its location: a local manifest is found at the
top-level directory of the sources of a project, in which case its named
`alire.toml`, whereas a manifest found in an index (e.g., the community index),
is named `<name>-<version>.toml`.

Other than that, contents follow the same conventions and there are only slight
differences (some fields are intended only for an index manifest, and cannot
appear, or are optional, in a local manifest). These differences are highlighted
in the following descriptions, where necessary.

Each TOML description file contains exactly one release, except for the special
external definitions that are described in their own section.

## Information encoding

This section describes the various encodings used in this format to encode
information.

First, there are two kinds of data: atomic and composite.

Atomic data designates values that cannot be decomposed. There are only two
atomic data types:

 - mere strings (`"Hello, world!"`);
 - booleans (`true`, `false`);

When a string denotes a relative path intended to be portable across operating
systems, it must use forward slashes as directory separator: `"path/to/my/resource"`.

We can then split composite data in two kinds: lists (TOML's arrays) and
mappings (JSON's tables). Lists are just sequences of other values, for
instance a list of strings:

```toml
["A", "B"]
```

Mappings are the traditional sets of associations from keys (here, always
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

All properties that support dynamic expressions follow the same structure, in
which the expression (case-like) is inserted between the key and its value.
For example, given a static expression:

```toml
key = "value"
```

one of its cases would be expressed by the following inline TOML table:

```toml
key.'case(var)'.var_value = "value"
```

Several expressions can be inserted between a property key and its value,
leading to a combinatorial explosion if all cases have specific values. The
equivalent to an `others` Ada clause in this format is a `'...'` entry.

Here is an example of a conditional boolean value.

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
values). Note that these and subsequent examples are not showing the
left-hand-side property to which such a value would be assigned.

A little variation allows building environment-dependent composite data. For
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

# Or in a more idiomatic TOML syntax
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

## Release Information

This section describes the actual properties that must or can appear in a
manifest file to describe a release. Unless specified, all the entries must be
static, i.e. they cannot depend on the context.

 - `name`: mandatory string. The name of the crate this release belongs to. Use
   `alr help identifiers` to see the rules such names must follow.

 - `version`: mandatory string. The semantic version of the release.

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

 - `maintainers`: mandatory (for indexing) array of strings. Flat list of
   human-readable names (optional) for the maintainers, with a contact email
   (mandatory); i.e. the people that maintain the crate metadata in Alire. For
   instance:

   ```toml
   maintainers = ["alice@example.com",
                  "Bob For Instance <bob@athome.com>"]
   ```

 - `maintainers-logins`: optional array of non-empty strings.
   For crates submitted to the community index, this is a mandatory flat list of
   the GitHub login usernames authorized to modify the crate.
   For instance:

   ```toml
   maintainers-logins = ["alicehacks", "bobcoder"]
   ```

   Private indexes may use whichever logins are appropriate for their
   hosting arrangement, or none at all.

 - `licenses`: mandatory (for indexing) string. A valid [SPDX
   expression](https://spdx.org/licenses/). Custom license identifiers are
   accepted with the format: `custom-[0-9a-zA-Z.-]+`

   ```toml
   licenses = "MIT"
   ```

   For a double license:

   ```toml
   licenses = "GPL-3.0-only OR MIT"
   ```

   For a custom license:

   ```toml
   licenses = "custom-my-license-1.2"
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

 - `depends-on`: optional array of dynamic dependencies expressions. For instance:

   ```toml
   [[depends-on]]  # A static dependency
   libfoo = "^1.2"

   [[depends-on]]  # A dynamic dependency
   [depends-on.'case(os)'.linux]
   libbar = "^2.0"

   [depends-on.'case(os)'.windows]
   libwinbar = "^3.0"
   ```

   Available constraint operators are the usual Ada relationals (`=`, `/=`, `>`, `>=`,
   `<`, `<=`) plus caret (`^`, any upwards version within the same major point)
   and tilde (\~, any upwards version within the same minor point).

   **Note that caret and tilde do not have any special behavior for pre-1
   versions.** This means, for example, that `^0.2` will still mean any release
   below `1.0`. The Semver specification does not make any promises about the
   compatibility of pre-1 versions, and there are differing interpretations of
   these operators out there for such versions. Bear in mind this when expressing
   your restrictions; for pre-1 versions you most likely want to use `~0.x`
   constraints (compatibility within a minor version).

   Logical operators for and (&), or (|) are accepted; see the `Semantic_Versioning`
   project documentation on [extended version
   sets](https://github.com/alire-project/semantic_versioning#types).

   See also the [section on compiler dependencies](#compiler-versions-and-cross-compilers)
   for more details on how to use the `depends-on` property for cross-compiling or
   compiler version selection.

 - `project-files`: optional list of strings. Each is a path, relative to the
   root of the source directory, to a `.gpr` project file to be made available.
   Expressions are accepted. For instance:

   ```toml
   project-files = ["my_project.gpr", "utils/utils_for_my_project.gpr"]

   [project-files.'case(word-size)']
   bits-64 = ["my_project.gpr"]
   bits-32 = ["my_project32.gpr"]
   ```

 - `gpr-externals`: optional table, giving a mapping from the name of external
   variables in the `.gpr` project files to sets of possible values (as array of
   strings), or an empty string if this set is infinite. For instance:

   ```toml
   [gpr-externals]
   BUILD_MODE = ["debug", "profile", "release"]
   TAG = ""
   ```

 - `gpr-set-externals`: optional dynamic table, setting values of project
   external variables when building the project. This should not be used to
   specify default values, the default values must be specified in the `.gpr`
   project file. Expressions are accepted before the mapping. For instance:

   ```toml
   [gpr-set-externals]
   BUILD_MODE = "release"

   [gpr-set-externals.'case(os)']
   linux   = { OS = "gnu-linux" } # Compact table syntax is convenient in this case
   windows = { OS = "ms-linux" }  # to see all enumeration values, one per row.
   ```

 - `environment`: optional dynamic table used to modify environment variables
   that will apply at build time. Variables and values are specified with the
   form `VARIABLE.<action> = "value"`, where `<action>` is one of `append`,
   `prepend`, or `set`. For instance:

   ```toml
   [environment]
   C_INCLUDE_PATH.append = "/usr/include/something"
   MY_PROJECT_ASSETS.set= "${CRATE_ROOT}/assets"
   PATH.append = "${DISTRIB_ROOT}/usr/bin"
   ```

   Path fragments in this table must use portable format, that is, '/' for path
   separation. Alire will take care of using the native separator when setting
   these variables.

   Predefined variables are provided by Alire and will be replaced in the
   value:

   - `${CRATE_ROOT}` absolute path to the deployment directory of the crate.
   - `${DISTRIB_ROOT}` absolute path to the root directory of the system
     distribution. On UNIX systems it will be `/`, on Windows `msys2` it will
     be the `msys2` installation directory (e.g.
     `C:\Users\user_name\.cache\alire\msys2`).

   The escaping `"\$"` can be used to prevent the expansion of a
   dollar-bracketed expression.

   Environment entries can use dynamic expressions:

   ```toml
   [environment.'case(distribution)']
   msys2 = { C_INCLUDE_PATH.append = "${DISTRIB_ROOT}/mingw64/include/SDL2" }
   ```

 - `executables`: optional dynamic list of strings. Each one is the simple name
   of an executable provided by the package. Executables are looked for by
   `alr` in the build tree and must not include a path. If only one executable is
   given, it is considered the default for `alr run`. For instance:

   ```toml
   executables = ["my_main"]
   ```

 - `actions`: optional dynamic list of actions to perform when certain events
   take place in a workspace. Actions are executed in the order they are
   defined in the manifest. The general action syntax is:

   ```toml
   [[actions]]
   type = <kind>
   command = <command>
   directory = <relative path>  # Optional
   ```

   `<command>` is an array of strings for a shell command to run in the
   source directory.

   For events that cause a workspace-wide triggering of actions (all
   `pre_/post_` actions described next), the actions are invoked in a
   dependency-safe order, starting at the leaves of the dependency graph
   (releases with no dependencies) and moving up to the root release (the working
   release, or the release being obtained with `alr get`). In this context, the
   root release is considered part of the dependency solution, and so its
   actions are executed too, always in the last place.

   `directory` is an optional portable relative path (forward-slashed) from the
   crate root, in which the action will be executed. This directory must exist
   or the action will error. Actions are executed by default in the crate root.

   `<kind>` can be either:

   - `post-fetch`: the command is to be run whenever there are new sources
     deployed in the workspace, in any release in the solution. All releases
     `post-fetch` actions are run after the new deployment is complete. Initial
     retrieval, subsequent modification of dependencies, pinning a
     directory or repository is considered a deployment of new sources. A manual
     `alr update`, even if it results in no changes, will also trigger this
     action in every release in the solution.

   - `pre-build`: the command is to be run right before the build of the
     workspace starts. This kind of action is run for all releases in the
     solution.

   - `post-build`: the command is to be run right after a build has
     successfully completed. This kind of action is run for all releases in the
     solution.

   - `test`: the command is run on demand for crate testing within the Alire
      ecosystem (using `alr test`). This kind of action is run only for the
      root crate being tested. The crate is not built beforehand when a test
      action is defined so, if a build is necessary, it should be explicitly
      given as part of the action sequence.

   Since actions may end being run more than once they should take this into
   account and allow multiple runs with the expected results intended by the
   packager.

   Actions accept dynamic expressions. For example:

   ```toml
   [[actions.'case(os)'.linux]]
   type = "post-fetch"
   command = ["make"]

   [[actions.'case(os)'.windows]]
   type = "post-fetch"
   command = ["cmd", "build"]

   [[actions.'case(os)'.'...']]
   # An explicit empty case alternative, which is not mandatory
   ```

   The aforementioned TOML syntax is valid when there is only one conditional
   action. For multiple conditional actions, one can write:

   ```toml
   [[actions]]
   [actions.'case(os)'.linux]
   # Regular contents of an action, applying to the Linux case
   [actions.'case(os)'.macos]
   # macOS case

   [[actions]]
   # Another action, that needs not be also conditional (but could be).
   ```

 - `auto-gpr-with`: optional Boolean value that specifies if the project (gpr) files
   of a crate can be automatically depended upon ('withed') directly by the root
   project file. (The default is true.) This feature is meant to simplify the process
   of using dependencies in Alire. However, not all project files are supposed to be
   direct dependencies. Some are intended to be extended, for example, and in that
   case a crate can disable the feature by setting `auto-gpr-with=false`.

 - `origin`: dynamic table. Mandatory for index manifests and forbidden in
   workspace manifests. This table describes how sources are obtained, using
   the following fields:

      - `url`: mandatory string which points to a source file or repository.
        If it points to a repository, this should be apparent from the URL;
        the prefixes `git+`, `hg+` or `svn+` can be prepended to the scheme
        (e.g. `git+https://`) to make this explicit, though a `.git` suffix or
        the hosts `github.com`, `gitlab.com` or `bitbucket.org` will also be
        recognized. For crates submitted to the community index, origins should
        be publicly accessible (i.e. should not require private ssh keys or
        other authentication).

      - `hashes`: mandatory string array for source archives.  An array
        of "kind:digest" fields that specify a hash kind and its value.  Kinds
        accepted are: `sha512`.

      - `archive-name`: optional string. If `url` points to a source archive,
        this can specify the name of the file to download, which is needed in
        order to properly extract the sources, in case the URL does not identify it.

      - `commit`: mandatory string for VCS origins that describes the
        VCS-specific revision to be checked out (a git/hg hash, a svn
        revision).

      - `subdir`: optional relative path, only valid for repository origins,
        that when provided indicates that the crate is not located at the
        repository root. This option enables the possibility of publishing
        several crates from the same repository (sometimes referred to as a
        *monorepo*).

      - `binary`: optional (defaults to false) boolean used to design the origin
        as binary. Binary origins are not compiled and can optionally use dynamic
        expressions to narrow down the platform to which they apply. An origin
        using a dynamic expression must be tagged as binary; see the
        example below.

   Examples of origin tables:

   ```toml
   # Clone a git repository with a crate at its root
   [origin]
   url = "git+https://github.com/example-user/example-project"
   commit = "ec8b267bb8b777c6887059059924d823e9443439"
   ```

   ```toml
   # Download and extract a source archive
   origin = "https://example.org/0123456789"
   archive-name = "archive.tar.gz"
   hashes = ["sha512:bf6082573dc537836ea8506a2c9a75dc7837440c35c5b02a52add52e38293640d99e90a9706690591f8899b8b4935824b195f230b3aa1c4da10911e3caf954c04ac"]
   ```

   ```toml
   # Clone a git repository with the crate in a subdirectory
   [origin]
   url = "git+https://github.com/example-user/example-project"
   commit = "ec8b267bb8b777c6887059059924d823e9443439"
   subdir = "examples"
   ```

   ```toml
   # A binary origin denoting a compiler
   [origin."case(os)".linux."case(host-arch)".x86-64]
   url = "https://github.com/alire-project/GNAT-FSF-builds/releases/download/gnat-12.1.0-1/gnat-x86_64-linux-12.1.0-1.tar.gz"
   hashes = ["sha256:df1f36b306359d528799b1de8629a793523347a90c9d4b72efd23c62a7279555"]
   binary = true
   ```

 - `available`: optional dynamic boolean expression.  If it evaluates to
   `false`, the package is not available for the current platform.

 - `notes`: optional string. Provides miscellaneous information about this
   release. For instance:

   ```json
   notes = "Experimental version"
   ```

 - `configuration`: optional table to control crate configuration code
   generators:

   For more information on crate configuration, see [Using crate
   configuration](#using-crate-configuration).

      - `disabled`: Completely disable configuration code generation for the
        crate (default: `false`)

      - `output_dir`: Path to the directory where the configuration code will
        be generated, relative to the crate root (default: `config`).

      - `generate_ada`: Enable generation of Ada configuration (default:
        `true`).

      - `generate_gpr`: Enable generation of GPR file configuration (default:
        `true`).

      - `generate_c`: Enable generation of C configuration (default: `true`).

      - `auto_gpr_with`: Enabled generation of list of withed project in the
        GPR file configuration (default: `true`).


 - `configuration.variables`: optional table of crate configuration variable
   definitions.

   For more information on crate configuration, see [Using crate
   configuration](#using-crate-configuration).

   The keys of the table are names of the variables. Variable definitions
   themselves are tables with the following entries:

      - `type`: mandatory string which defines the type of the variable, it can
        be:

         - `String`: any string

         - `Boolean`: either `True` or `False`

         - `Enum`: enumeration type

         - `Integer`: an integer value that can be encoded in 64-bit

         - `Real`: a real value that can be encoded in IEEE 754 binary64

      - `default`: optional default value for the variable. Will be used if no
        crates explicitly set a value for this variable. Must be a valid value
        for the type.

      - `first`: (optional) for `Real` and `Integer` types only. Defines the
         lower bound of valid values for the type (inclusive).

      - `last`: (optional) for `Real` and `Integer` types only. Defines the
         upper bound of valid values for the type (inclusive).

      - `values`: mandatory for `Enum` types. An array of strings containing
        all the possible values for the enumeration.


   Example:
   ```toml
   [configuration.variables]
   Device_Name = {type = "String", default = "no device name"}
   Print_Debug = {type = "Boolean", default = false}
   Debug_Level = {type = "Enum", values = ["Info", "Debug", "Warn", "Error"], default = "Warn"}
   Buffer_Size = {type = "Integer", first = 0, last = 1024, default = 256}
   Max_Power   = {type = "Real", first = 0.0, last = 100.0, default = 50.0}
   ```
 - `configuration.values` optional table of variables assignment:

   The keys of the table are crate names, and entries are sub-tables of
   `variable_name` and `value`. The type of the value has to match the
   definition of the variable type.

   Example:
   ```toml
   [configuration.values]
   crate_1.var1 = 42
   crate_1.var2 = true
   crate_2.var1 = "Debug"
   ```

 - `build-profiles`: optional table of strings that sets the build profile of
   crates in the solution.

   For more information on build profiles and switches, see [Build profiles and
   switches](#build-profiles-and-switches).

   There are 3 build profiles available in Alire:
    - `Development`
    - `Release`
    - `Validation`

   Example:
   ```toml
   [build-profiles]
   depend1 = "validation" # Set depend1 build profile to validation
   depend2 = "development" # Set depend2 build profile to development
   my_crate = "release" # Set my_crate build profile to release
   ```
   A wildcard key can be used to set the build profile of all crates that are
   not otherwise specified:
   ```toml
   [build-profiles]
   "*" = "validation" # Set all build profiles to validation
   ```

 - `build-switches`: optional table of build profile switches definitions.

   For more information on build profiles and switches, see [Build profiles and
   switches](#build-profiles-and-switches).

   The keys of the table are either build profiles or the wildcard `"*"`:
    - `Development`
    - `Release`
    - `Validation`

   The values are profile definitions, themselves tables with switch categories
   as keys and switches selection as values. This list of switch categories and
   their corresponding selection is as follow:
    - `Optimization`
      - `Performance`
      - `Size`
      - `Debug`
    - `Debug_Info`
      - `No`
      - `Yes`
    - `Runtime_Checks`
      - `None`
      - `Default`
      - `Overflow`
      - `Everything`
    - `Compile_Checks`
      - `None`
      - `Warnings`
      - `Errors`
    - `Contracts`
      - `No`
      - `Yes`
    - `Style_Checks`
      - `No`
      - `Yes`
    - `Ada_Version`
      - `Compiler_Default`
      - `Ada83`
      - `Ada95`
      - `Ada05`
      - `Ada12`
      - `Ada2022`
      - `GNAT_Extensions`
    - `Source_Encoding`
      - `Compiler_Default`
      - `UTF-8`

   For example, to enable all run-time checks in the release profile:
   ```toml
   [build-switches]
   release.runtime_checks = "Everything"
   ```
   To disable style checks for all profiles:
   ```toml
   [build-switches]
   "*".style_checks = "No"
   ```

    All switch categories also accept a custom list of switches, for instance:
   ```toml
   [build-switches]
   release.optimization = ["-O1"]
   validation.style_checks = ["-gnatyg"]
   ```

 - `provides`: specifies a list of releases of another crate for which the
   current release is a drop-in replacement. I.e., the crate is either
   API-compatible or call-compatible, depending on how it is to be used (as a
   source library, or providing some command-line tool).

   Example:
   ```toml
   name = "foo"
   provides = ["bar=1.1"]
   # A crate depending on `bar^1` might find this `foo` release in its solution instead.
   ```

 - `forbids`: an array of tables containing dependency specifications, just as
   the `depends-on` property. Releases matching one of the forbidden
   dependencies are prevented from appearing in a solution with the release
   doing the forbidding.

   There are two use cases for this property:

   1. To codify known conflicts between releases for some reason (for example,
   sources with the same name).
   2. To provide drop-in replacements for another crate, in conjunction with
   a `provides` field. In this case the release must both provide and forbid
   the crate for which it is a replacement.

   Example:

   ```toml
   name = "bar"
   version = "1.0"
   provides = [ "foo=1.0" ]
   [[forbids]]
   baz = "*" # This crate cannot coexist with ours for some reason
   foo = "*" # No other crate that provides foo is needed/allowed at the same time
   ```

## Work-in-progress dependency overrides

It is usual to develop several interdependent crates at the same time. In this scenario, it is often impractical to rely on indexed releases which are not intended to be modified. Instead, one would prefer to use a work-in-progress version of a crate to fulfill some dependency.

Alire provides *pins* to support this use case. Pins override dependencies, they are intended to be used locally, and to be fulfilled by proper dependencies once a crate is ready to be published. The use of pins is based on two ideas:

* Dependencies are given, as normally, in the `depends-on` array of the manifest, even for those dependencies to be pinned. This way, once the release is ready, pins are simply removed and the actual dependencies are used in their place.
* Dependency overrides, aka *pins*, are given under the `[[pins]]` array of the manifest.

Three kinds of pins are available, all of them with the syntax:

`crate_name = { pin_attributes }`

The specific pin kinds and their attributes are:

* Pins to versions: used to force the use of a particular version of an indexed crate.

  * `version`: a string containing a single version to be used.
  * `crate_name = { version = "1.2+hotfix-1" }`

* Pins to local crates: a local directory will fulfill the crate dependency, no matter what version is given in its local manifest. "Raw" Ada projects without an Alire manifest can be used too, as long as their project file matches the crate name and it is located in the directory given as override.

  * `path`: an absolute or relative path to the crate directory.
  * `crate_name  = { path = "../my/wip/crate" }`

  For the common case of directories containing an Alire manifest, dependencies and pins will be included recursively in the build context.

* Pins to git repositories: the repository will be cloned locally and its directory will be used as in the previous case. This pin may optionally include a commit to fix the checkout to be used, or a branch to track. Otherwise, the default branch will be used. Running `alr update` will refresh the checkout.

  * `url`: the URL of a git repository.
  * `commit` (optional): a complete git commit hash.
  * `branch` (optional, mutually exclusive with commit): a branch to track on `alr update`.
  * `subdir`: (optional): relative path that indicates where the crate is located when not at the repository root.
  * `crate_name = { url = "https://my/repo.git" } # Updatable pin to default branch`
  * `crate_name = { url = "https://my/repo.git", branch="feature" } # Updatable pin`
  * `crate_name = { url = "https://my/repo.git", commit="abcdef..." } # Fixed pin`
  * `crate_name = { url = "https://my/repo.git", subdir="mycrate"} # Crate located in a subdirectory`

### Using pins for crate testing

Pins are also useful to have a separate test project that depends on your main crate. The recommended setup is as follows:

```
/path/to/my_crate
├── alire.toml
└── tests
    └── alire.toml
```

I.e., a `tests` crate is initialized within the main `my_crate`. In `tests` manifest, you have a dependency and local relative path pin for `my_crate`:

```toml
# tests/alire.toml
[[depends-on]]
my_crate = "*"              # Any version of the main crate
aunit = "*"                 # We can have dependencies for testing only
[[pins]]
my_crate = { path = ".." }  # Overridden by the latest sources
```

 Then, `my_crate` is published normally, and `tests` can be used locally for any kind of testing needed on `my_crate` without polluting `my_crate` manifest with test specifics (like extra dependencies used by the test setup).

## External releases

The above information applies to regular releases distributed from sources
(that is, the Ada projects whose distribution is the main Alire goal). Some
special supporting releases also exist that are described differently.

A release is considered "external" when it is not built from sources and,
furthermore, its semantic version cannot be known until run time. Hence, the
availability and version of these releases is detected by `alr`.

Several definitions for these external releases may exist so they are
defined in a manifest as a vector with key `external`:

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

 - `hint`: optional dynamic string containing an explanation for the user on
   how to make the external entity available. This explanation is shown on request
   with `alr show --external`, or after `alr get`, for any external dependency
   that could not be detected.

### External kinds: hints

A plain undetectable external kind intended to simply serve as a hint. For
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

provides = "another_crate_name"
# This crate will be equivalent to `another_crate_name` for the solver. The
# version will be the same as detected for the current external. For example,
# all GNAT compilers provide the "gnat" crate, and so there cannot be two
# compilers in the same solution.
```

### External kinds: system packages

Systems that have their own package manager (e.g. Linux) can readily provide
many complex dependencies still unpackaged as source code in Alire. Alire can
use these on supported platforms during resolution. At this time, the supported
platforms are Arch, CentOS, Debian, Fedora, Homebrew, MacPorts, MSYS2, RHEL,
SUSE/openSUSE, and Ubuntu; do not hesitate to contact us if you would like to
maintain other distributions.

A system external gives a list of platform package names that supply the
dependency natively. The platform package manager will be used to detect their
availability and version. To that effect, the `origin` field is used (which can
accept dynamic expressions in this context):

```toml
kind = "system" # Identifies this external kind
origin = ["libncursesada3", "libncursesada5"]
# As versions appear this list will grow. To speed up detection, dynamic
# expressions may become recommended for certain system packages.
```

For Ada pre-compiled system libraries that require the platform compiler for
linking (e.g., in Debian/Ubuntu), and that cannot be used with other GNAT
compilers, this should be expressed with the `available` property, e.g.:

```toml
available.'case(toolchain)'.user = false
# `available` defaults to true, so it is enough to flag the user toolchains
```

## Parameters

 - `os`: name of the OS. Currently supported values are: `freebsd`, `openbsd`,
   `linux`, `macos`, `windows`, and `os-unknown`.

 - `distribution`: name of the Linux distribution or name of the software
   distribution platform if running on a different OS. Currently supported
   values are: `arch`, `centos`, `debian`, `fedora`,
   `homebrew`, `macports`, `msys2`, `rhel`, `suse`, `ubuntu`, and
   `distribution-unknown`.

 - `toolchain`: takes `system` value in distributions with the system Ada
   compiler first in PATH (GNAT FSF in Debian/Ubuntu), `user` otherwise (GNAT
   Community editions, other cross-target toolchains).

 - `word-size`: architecture word size. Currently supported values are:
   `bits-32`, `bits-64`, `bits-unknown`

## Using crate configuration

`Alire` provides a mechanism for crates to expose a list of variables that can
be set by other crates depending on them. The configuration variables will then
be converted to Ada, C and GPR source files that can be used to change the
behavior or feature set of the code.

Let's start with a simple example. A crate named `test` can print debug log on
the console. However printing on the console has a performance impact, for an
embedded project it can even have a significant code size impact. Therefore it
would be best if this logging can be disabled/enabled at compile time.

To achieve this, a crate maintainer can define a configuration variable in the
crate manifest `alire.toml`. The definition will be like so:
```toml
[configuration.variables]
Enable_Logs = {type = "Boolean", default = false}
```
A single variable of type `Boolean` with a default value of `false`.

From this definition, `Alire` will generate various source files, including an
Ada package specification:

```ada
package Test_Config is
   Enable_Logs : constant Boolean := False;
end Test_Config;
```

In the crate source code, this configuration package can be used like so:
```ada
   if Test_Config.Enable_Logs then
      Print_Log ("This is a log message.");
   end if;
```

If one of the crates depending on `test` sets the configuration variable to
`true`, e.g.:

```toml
[configuration.values]
test.Enable_Logs = true
```

The constant value will change in the generated configuration package:
```ada
package Test_Config is
   Enable_Logs : constant Boolean := True;
end Test_Config;
```
Which will enable logging in the `test` crate.

It is possible for multiple depending crates to set `test.Enable_Logs` to the
same value, however if two depending crates set the variable to a different
value then the configuration is invalid and `Alire` will print an error. If no
depending crates set the `test.Enable_Logs` variable, then its default value is
used.

### When to use crate configuration?

Usually when something has to be static or known at compiler-time, either for
performance or memory usage.

### When _not_ to use crate configuration?

When the Ada languages provides a better alternative. There are many ways to
provide an Ada API that will result in compile time optimization or static
memory usage.

For instance, discriminants are an effective way to let the user define the
size of a buffer:

```ada
   type Buffered_Thing (Size : Positive) is private;
private
   type Buffer_Array is array (Positive range <>) of Unsigned_8;
   type Buffered_Thing (Size : Positive) is record
      Buf : Buffer_Array (1 .. Size);
   end record;
```

With this definition, users are then able to allocate either statically, on the
stack or on the heap depending on their project.

```ada
   Thing : Buffered_Thing (Size => 256);
```

### Use cases

#### Log levels

Enumerations variables in crate configuration can be used to set a level of log
verbosity:
```toml
[configuration.variables]
Log_Level = {type = "Enum", values = ["Info", "Debug", "Warn", "Error"], default = "Warn"}
```

#### Buffer size

Integer variables can be used the define the size of a static buffer:

```toml
[configuration.variables]
Buffer_Size = {type = "Integer", first = 0, last = 1024, default = 256}
```
This is useful in particular for embedded projects where compile time memory
usage is preferred over dynamic allocation.

#### Server URL

String variables can be used to define the URL of a website or service:

```toml
[configuration.variables]
URL_Name = {type = "String", default = "example.com"}
```

#### PID coefficients

Real variables can be used for PID coefficients:
```toml
[configuration.variables]
Proportional = {type = "Real"}
Integral = {type = "Real"}
Derivative = {type = "Real"}
```
#### Worst case allocation

Integer variable can be used to define The maximum length of file names in a
file-system:
```toml
[configuration.variables]
Max_Filename_Length = {type = "Integer", first = 5, last = 128}
```

#### Select algorithm in GPR project file

Crate configuration also generates a GPR project file, therefore it can be used
to control which units are compiled in the project.
```toml
[configuration.variables]
Sort_Algorithm = {type = "Enum", values = ["bubble", "quick", "merge"]}
```

The generated GPR will look something like this:
```ada
project Test_Config is
   type Sort_Algorith_Kind is ("bubble", "quick", "merge");
   Sort_Algorith : Sort_Algorith_Kind := "quick";
end Test_Config;
```

It can be used in the main GPR file like so:

```ada
   package Naming is
      for Body ("Test.Sort") use "test-sort__" & Test_Config.Sort_Algorith;
   end Naming;
```
With the files `test-sort__bubble.adb`, `test-sort__quick.adb` and
`test-sort__merge.adb` each implementing a different algorithm.

## Platform Specific Code

In the crate configuration Alire also generates a few built-in values to
identify the host platform:
 - `Alire_Host_OS`
 - `Alire_Host_Arch`
 - `Alire_Host_Distro`

They can be used in the main GPR file to add a different source directory
based on the OS. For instance:
```ada
   for Source_Dirs use ("src",
                        "src/" & Test_Config.Alire_Host_OS);
```
with the following directory tree:
```
+-- src
    +-- host_specific.ads
    +-- linux
    |   +-- host_specific.adb
    +-- macos
    |   +-- host_specific.adb
    +-- windows
        +-- host_specific.adb
```

## Build Profiles and Switches

As part of crate configuration, Alire will generate a list of compiler switches
in the configuration GPR file. The list of switches for a given crate is
controlled from two features:
 - build-profiles
 - build-switches

There are 3 build profiles available in Alire:
 - `Development`
 - `Release`
 - `Validation`

By default, the root crate is in `Development` and the dependencies are in
`Release`. The defaults can be overridden in two ways:
 - The build profile of the root crate can be changed with a switch to the
   `alr build` command:
    - `$ alr build --release`
    - `$ alr build --validation`
    - `$ alr build --development` (default)
 - In the root crate manifest, the build profile of each crate in the solution
   can be changed with the `[build-profiles]` table.

   This can be used, for instance, in a unit test crate to set the crate under
   test in `validation` profile, or to debug one of the dependencies.

   Example:
   ```toml
   [build-profiles]
   lib_under_test  = "validation"
   lib_to_debug    = "development"
   ```

Each crate can customize the compiler switches corresponding to its profiles
using the `[build-switches]` table. In general, this should be avoided to
preserve consistency in the ecosystem. However, there are cases where it makes
sense for a crates to change its build switches. For instance, a SPARK crate
that is proved to be safe from errors can disable run-time checks in all
profiles:
```toml
[build-switches]
"*".runtime_checks = "none"
```

It is also possible to specify a custom list of compiler switches for a
category:
```toml
[build-switches]
release.optimization = ["-O1", "-gnatn"]
```

### Using switches in GPR file

Alire will generate a list of switches in the crate configuration GPR file. It
will look something like this:

```ada
abstract project my_crate_Config is
   [...]
   Ada_Compiler_Switches := External_As_List ("ADAFLAGS", " ") &
          (
            "-Os" -- Optimize for code size
           ,"-gnatn" -- Enable inlining
          );
   [...]
```

In the main GPR file, "with" the crate config GPR and use the
`Ada_Compiler_Switches` variable to define compiler switches:

```ada
with "config/my_crate_config.gpr";
project My_Crate is

   [...]

   package Compiler is
      for Default_Switches ("Ada") use My_Crate_Config.Ada_Compiler_Switches;
   end Compiler;
```

## Compiler versions and cross-compilers

Dependencies in Alire are used also to deal with compiler versions and
cross-compilers. Also related is the information on toolchains available in the
[Toolchain management](toolchains) document or via `alr help toolchains`.

### Excluding compiler versions

One may know that a particular compiler version has a problem with some code.
This may be expressed with dependencies on the generic `gnat` crate, which
although is not found in the catalog, is a crate that all GNAT compilers
provide. (Such a crate without actual releases, but provided by other crates,
is called an abstract crate.) For example:

```toml
gnat = ">=7"   # We require a minimum compiler version
gnat = "/=7.3" # We know a precise version is incompatible
```

Since only one dependency on a same crate may appear, the relational operators
`&` (and), `|` (or) can be used instead:

```toml
[[depends-on]]
gnat = "/=7.3 & >=7"
```

### Requesting a compiler for a concrete target

The other use of compiler dependencies is to specify that a compiler for a
particular target is needed. (Note that the project file **also** has to
specify the proper target and runtime.) This way Alire can configure the
appropriate environment for the build. For example:

```toml
gnat_arm_elf = "*" # Any compiler targeting ARM
```

Dependencies on cross-compilers should **only** be used in crates that actually
require a concrete target (e.g., final binaries) to avoid preventing their use
as general libraries with any compiler.

## Further reading ##

You can inspect [index files](https://github.com/alire-project/alire-index) to
get an idea of how projects are included into the catalog.
