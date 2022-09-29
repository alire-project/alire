# User-facing changes log

This document is a development diary summarizing changes in `alr` that notably
affect the user experience. It is intended as a one-stop point for users to
stay on top of `alr` new features.

## Release 1.3-dev

### Find dependents of a release with `alr show --dependents

PR [#1170](https://github.com/alire-project/alire/pull/1170)

A new switch for `alr show` lists the newest release that depends on another
given release. For example, to find direct dependencies on `libhello`:

```
$ alr show libhello --dependents
CRATE  VERSION  DEPENDENCY
hello  1.0.1    ^1.0.0
```

To identify all dependents, both direct and indirect,
use `--dependents=shortest`, which will also show the shortest dependency chain
from (indirect) dependent to dependee:

```
$ alr show aws --dependents=shortest
CRATE                    VERSION  DEPENDENCY  CHAIN
adabots                  1.2.0    ^21.0.0     adabots=1.2.0»aws=21.0.0
awa                      2.4.0    ~21.0.0     awa=2.4.0»utilada_aws=2.5.0»aws=21.0.0
awa_unit                 2.4.0    ~21.0.0     awa_unit=2.4.0»awa=2.4.0»utilada_aws=2.5.0»aws=21.0.0
matreshka_spikedog_awsd  21.0.0   *           matreshka_spikedog_awsd=21.0.0»aws=21.0.0
servletada_aws           1.6.0    ~21.0.0     servletada_aws=1.6.0»utilada_aws=2.5.0»aws=21.0.0
utilada_aws              2.5.0    ~21.0.0     utilada_aws=2.5.0»aws=21.0.0
webdriver                1.0.0    *           webdriver=1.0.0»aws=21.0.0
```

Finally, to obtain all paths through which dependents reach a dependency, use
the `all` value. In this case crates may appear more than once in the listing:

```
$ alr show --dependents=all cortex_m
CRATE               VERSION  DEPENDENCY  CHAIN
minisamd51_bsp      0.1.0    ^0.1.0      minisamd51_bsp=0.1.0»samd51_hal=0.2.0»cortex_m=0.5.0
minisamd51_example  0.1.1    ^0.1.0      minisamd51_example=0.1.1»minisamd51_bsp=0.1.0»samd51_hal=0.2.0»cortex_m=0.5.0
pico_bsp            2.0.0    ~0.5.0      pico_bsp=2.0.0»rp2040_hal=2.0.0»cortex_m=0.5.0
pico_examples       2.0.0    ~0.5.0      pico_examples=2.0.0»rp2040_hal=2.0.0»cortex_m=0.5.0
pico_examples       2.0.0    ~0.5.0      pico_examples=2.0.0»pico_bsp=2.0.0»rp2040_hal=2.0.0»cortex_m=0.5.0
pygamer_bsp         0.1.0    ^0.1.0      pygamer_bsp=0.1.0»cortex_m=0.5.0
pygamer_bsp         0.1.0    ^0.1.0      pygamer_bsp=0.1.0»samd51_hal=0.2.0»cortex_m=0.5.0
rp2040_hal          2.0.0    ~0.5.0      rp2040_hal=2.0.0»cortex_m=0.5.0
samd51_hal          0.2.0    ^0.1.0      samd51_hal=0.2.0»cortex_m=0.5.0
```

### Finer control of build profiles in `alr build`

PR [#1119](https://github.com/alire-project/alire/pull/1119)

Build profiles can be now tweaked from the command-line with a new switch:

- `alr build --profiles '*=development'`
  `# Set all profiles to development`
- `alr build --profiles '%=validation'`
  `# Set profiles without an override in a manifest to validation`

Explicit crates can be given, intermixed with one of the wildcards, which apply
to the rest of crates in the build:

- `alr build --profiles '*=development,libhello=release'`
  `# Set all profiles to development but for libhello`

The existing switches `--release`, `--validation`, `--development` continue to
control root crate profile and take the highest priority:

- `alr build --validation --profiles '*=development'`
  `# Set the working crate to validation and the rest to development`

### Reuse build profile of `alr build` when issuing `alr run`

PR [#1080](https://github.com/alire-project/alire/pull/1080)

`alr run` will trigger a build to have an up-to-date executable, and before
this PR this was always a development build. Now, the last profile used during
an `alr build` will be reused.

## Release 1.2

### New subcommand for listing and manual triggering of actions

PR [#983](https://github.com/alire-project/alire/pull/983)

Actions defined in a working release can be listed now with `alr action`. A
specific kind of action can be triggered by specifying its kind. Actions in the
complete dependency tree can be listed and triggered with the `--recursive`
switch.

```console
$ alr action                # Display actions defined in the root release
$ alr action --recursive    # Display all actions in the root and dependencies
$ alr action post-build     # Run post-build actions in the root release
$ alr action post-build -r  # Run post-build actions in the root and dependencies
```

### UTF-8 Source Encoding

PR [#972](https://github.com/alire-project/alire/pull/972)

As part of the build profile feature, the GNAT switch `-gnatW8` is
unconditionally added to the list of compiler switches in the configuration GPR
file. This switch enables the use of UTF-8 for source file encoding.

### Support for crates in repository subfolders (monorepos)

PR [#939](https://github.com/alire-project/alire/pull/939)

A crate can now be located nested within a repository and still be published
as a repository origin. This enables simpler publishing of such crates, as `alr
publish` will automatically recognize the situation. For example, let us say we
have this structure:

```
my_repo
  +-- my_crate
  +-- my_crate_examples
```

Running `alr publish` at `./git_repo/my_crate` or
`./git_repo/my_crate_examples` will detect that the crate is not at the
root and adjust the origin metadata to take into account the extra path.

Other typical hierarchies that should likewise work are:

```
my_crate (also a repository)
  +-- examples

my_repo
  +-- crate1
        +-- examples
  +-- crate2
        +-- examples
```

At this time `alr publish` will not remove pins, so that is still a manual
adjustment that the user may have to perform prior to publishing; that is, the
manifest at each nested crate must be manually readied for publishing (just as
for any other regular crate).

### Root crate build profile

PR [#896](https://github.com/alire-project/alire/pull/896)

The default build profile for the root crate is `Development`. This
can be changed with the `--release`, `--validation` and `--development`
switches for `alr build`.

```console
$ alr build --release     # build with release profile
$ alr build --validation  # build with validation profile
$ alr build --development # build with development profile
$ alr build               # build with development profile
```

### Build profiles and switches

PR [#895](https://github.com/alire-project/alire/pull/895)

As part of the crate configuration feature, Alire will generate a list of
compiler switches in the configuration GPR file. The list of switches is
controlled from two features:
 - build-profiles
 - build-switches

### User defined command aliases

PR [#853](https://github.com/alire-project/alire/pull/853)

It is now possible to define in configuration (local or global) aliases for the
`alr` commands.

```console
$ alr config --set --global alias.graph 'show --graph'
$ alr graph
```
Will run the `alr show` command with the `--graph` switch.

### New command `alr exec -- <command line>`

PR [#853](https://github.com/alire-project/alire/pull/853)

This new command takes an executable and arguments and run them in the Alire
environment/context of the current crate.

```console
$ alr exec -- sh -c 'echo ${ALIRE}'
True
```

### Pass alr clean switches to gprclean

PR [#853](https://github.com/alire-project/alire/pull/853)

Using the `--` delimiter the switches and arguments for `alr clean` can now be
passed to the underlying `gprclean` execution.

For instance:
```console
$ alr clean -- -XTEST=42
```


### Pass alr build switches to gprbuild

PR [#850](https://github.com/alire-project/alire/pull/850)

Using the `--` delimiter, the switches and arguments for `alr build` are now
passed to the underlying `gprbuild` execution.

For instance:
```console
$ alr build -- -f
```
will force recompilation.

### Global switches only allowed before sub-command

PR [#850](https://github.com/alire-project/alire/pull/850)

Before this change the global switches (`-f`, `-n`, `--config=`, etc.) could be
placed anywhere on the command line. For instance, the following two commands
were equivalent:
```console
$ alr -f show
$ alr show -f
```

Global switches are now only allowed before the sub-command name. Such that:
```console
$ alr -f show # Is OK
$ alr show -f # Is not OK (unrecognized option '-f' for 'show')
```

## Release `1.1`

### Lockfile moved to `alire` folder

PR [#789](https://github.com/alire-project/alire/pull/789)

The lock file (`alire.lock`) is now a purely internal file, regenerated
as needed from scratch, and needs not be put under version control. Since,
furthermore, this file is not intended for user edition or inspection, it is
now created inside the `alire` folder of a crate.

Existing lock files at the root of a crate will be automatically migrated to
their new location the first time an `alr` command that uses the lock file is
run inside a crate with the old situation.

This change obsoletes the recommendation that accompanied PR
[#501](https://github.com/alire-project/alire/pull/501) about putting the lock
file under version control.

### Conflicting releases

PR [#781](https://github.com/alire-project/alire/pull/781)

For releases that have known incompatibilities (duplicated source names,
drop-in equivalent crates), it is now possible to express this information
through a `forbids` table array, with the same syntax as dependencies. For
example:

```
[[forbids]]
conflicting_crate = "^1"
```

Releases related by a `forbids` property will not appear simultaneously as
dependencies in a solution, as the solver will discard these combinations.

### Toolchain management

PR [#775](https://github.com/alire-project/alire/pull/775)

A variety of GNAT compilers (native and cross-target) is now available through
Alire. These compilers are managed with the `alr toolchain` new command. The
available compilers can be listed with `alr search --full gnat_`.

Toolchain configuration is common to all crates in the active configuration
prefix (which can be switched with the global `-c` option or by providing a
path with the `ALR_CONFIG` environment variable).

The `alr toolchain --select` subcommand allows selecting the preferred default
compiler (or none at all, to continue using the previous mode of operation) for
crates that do not specify one.

Crates that require a particular cross-compiler may now specify it as a regular
dependency on, e.g., `gnat_riscv_elf`.

In addition to a default compiler, the preferred version of a compiler for a
target may be made available with `alr toolchain --install <crate[=version]>`.
When launching a build, Alire will use preferably the default selected compiler
or, if the default is for a different target, one of the other installed
compilers. If no installed compiler is available for the crate target, Alire
will offer to download the appropriate cross-target compiler.

Finally, running `alr toolchain` without arguments will list the currently
installed compilers and gprbuild versions.

### Pins to git branches

PR [#754](https://github.com/alire-project/alire/pull/754)

A new option for remote pins exist to track branches:

```
[[pins]]
wip = { url = "https://gitrepo.com/wip.git" branch="feature" }
```

Running `alr update` will pull any changes from the branch.

### Pins stored in the manifest

PR [#743](https://github.com/alire-project/alire/pull/743).

The options to modify pins through the command-line  (`with --use`, `alr pin
[--unpin] crate` have been disabled in favor of direct edition of the manifest.
This way, pins are more robust against lockfile format changes. These kinds of
pins exist:

```
[[pins]]
foo = { version = "1.3.2+bugfix" } # Require a specific version
bar = { path = "../my/bar" } # Use a local crate to override a dependency
baz = { url = "https://github.com/baz.git" } # No commit, will use HEAD, will update on `alr update`
gru = { url = "https://gitlab.com/gru.git" commit="123456890abcdef..." } # Explicit commit, won't update
```

### Automatic GPR 'with' now in crate configuration

PR [#740](https://github.com/alire-project/alire/pull/740).

When adding or removing dependency with `alr with`, the list of `with`
statement for each project files of the dependencies is now automatically added
to the GPR crate configuration file instead of the root project file.

### Git remotes for pinned releases

PR [#715](https://github.com/alire-project/alire/pull/715)

The pinning commands (`alr with --use`, `alr pin --use`) now also accept a git
repository URL, which will be downloaded and used to override a dependency, as
previously could be done only with local directories. The pinning feature works
recursively, so unpublished crates can now have complete dependencies prior to
submission to the community index (which relies only on indexed dependencies).

### Switch to help with publishing of multi-crate repositories

PR [#635](https://github.com/alire-project/alire/pull/635).

The `alr publish` command now supports a new `--manifest <file>` switch, to
help with packaging sources that provide several crates. Maintainers can now
prepare different manifest files for the corresponding crates, and select each
one in turn for publishing, without the repository itself being an actual Alire
crate.  Source management must still be taken care of by maintainers; sources
should not be shared by project files in different crates intended to be
simultaneously included.

### Configuration of crates

PR [#699](https://github.com/alire-project/alire/pull/679).
PR [#673](https://github.com/alire-project/alire/pull/673).

Pre-compilation parameterization of source files can be now achieved by
declaring variables and initial constant values for these variables in the
Alire manifests. This allows customizing code in both the root crate and
dependencies. For example:

```toml
[configuration.variables]
Device_Name = {type = "String", default = "no device name"}
Debug_Level = {type = "Enum", values = ["Info", "Debug", "Warn", "Error"], default = "Warn"}
Buffer_Size = {type = "Integer", first = 0, last = 1024, default = 256}

[configuration.values]
crate_1.var1 = 42
crate_1.var2 = true
crate_2.var1 = "Debug"
```

Check more examples and details in the catalog specification section ["Using
configuration"](https://github.com/mosteo/alire/blob/master/doc/catalog-format-spec.md#using-crate-configuration).

## Release `1.0`

### Narrow down versions for dependencies given without restrictions

PR [#675](https://github.com/alire-project/alire/pull/675).

When a user requests a dependency without narrowing down its version set (e.g.,
`alr with foo`), the solved version will be used to instead add an
"update-safe" dependency (e.g., `foo^1.x`, `foo~0.x`). To truly request any
version, this can be explicitly entered as `alr with 'foo>=0'`.

This behavior can be disabled by setting the `solver.autonarrow` configuration
option to false.

### The command `alr list` has been renamed to `alr search --crates`

PR [#671](https://github.com/alire-project/alire/pull/671).

To consolidate search functionality under the single `alr search` command, the
old behavior of `alr list` can now be achieved with `alr search --crates`. By
default, `alr search` looks into releases, but now it can look too into crates
with the new `--crates` switch.

### Document caret/tilde use for pre-1.0 versions, and warn about it

PR [#669](https://github.com/alire-project/alire/pull/669).

Alire does not change the meaning of caret (^) and tilde (~) operators for
pre/post-1.0 versions. This interpretation has been clarified in the catalog
specification, and `alr` will warn about any suspicious usage. This warning may
be disabled by the user with the new `warning.caret` configuration option.

### Do not perform build relocations

PR [#667](https://github.com/alire-project/alire/pull/667).

GPRBuild machinery for build relocation is incompatible with some use cases, so
now all builds are performed in place, using the locations given in project
files. This should only have a user-visible impact for pinned dependencies,
which will see changes in their build directory when Alire builds for dependent
crates are run.

### Switch to check for unknown enumeration values in the index

PR [#656](https://github.com/alire-project/alire/pull/656).

To allow backwards-compatible use of new supported environment configurations
in the index, unknown values in dynamic case expressions are silently ignored
when loading an index. In order to allow pinpointing these values (or truly
wrong entries), a new switch `alr index --check` can be used that will reject
an index containing unknown values.

This error, either in indexes or a local manifest, can be downgraded to a
warning with `--force`.

### Switch manifest `licenses` field to SPDX expressions

PR [#629](https://github.com/alire-project/alire/pull/629).

The `licenses` in crate manifests now expects a valid [SPDX
expression](https://spdx.org/licenses/). Custom license identifiers are
accepted with the format: `custom-[0-9a-zA-Z.-]+`.

Example:
```toml
licenses = "MIT OR custom-my-own-license"
```

For the `1.x` release, usage of the previous `licenses` format is obsolete and
will trigger a warning. In future major releases this format will not be
accepted at all.

### Custom editor command for `alr edit`

PR [#611](https://github.com/alire-project/alire/pull/611).

The code editor launched by `alr edit` can now be configured instead of using
the hard-coded GNATstudio. Use
`alr config --set --global editor.cmd "<BINARY> <ARGS>"`
for custom editor and command line arguments. The token ${GPR_FILE} is
replaced by a path to the project file to open.

For instance:
```shell
$ alr config --set --global editor.cmd "emacs ${GPR_FILE}"
```

The default editor is still GNATstudio.

## Release `0.7-beta`

### Assistance to generate and publish as tarball

PR [#529](https://github.com/alire-project/alire/pull/529).

By using `alr publish --tar`, the publishing assistant starts with the
creation of a tarball of the sources in an Alire workspace. The user must
upload this tarball to an online location, after which the assistant proceeds
as if it had been invoked with `alr publish http[s]://url/to/tarball.tgz`.

### First publishing assistant

PR [#527](https://github.com/alire-project/alire/pull/527).

A new publishing assistant can be invoked with `alr publish [URL [commit]]`. At
this time, the assistant requires that all necessary metadata for a release,
excepting the `[origin]` table, is informed in the `alire.toml` file.

The assistant has local and remote modes of operation. In the local mode, the
user invokes the assistant from within a repository on its computer that is
up-to-date with its remote, and that contains an Alire workspace. In this case,
it is enough to run `alr publish`.

In the remote mode, the user must prepare a source file or repository in their
final online locations, and use `alr publish <URL> [<commit>]`, with the commit
being mandatory for repositories and not needed for source archives.

In all cases, `alr` will fetch the sources, perform a few checks on the
completeness of the information, and generate a final metadata file, intended
to be submitted to the community index via pull request. An upload link is
provided for convenience that can be used to create this pull request.

Complete information about this feature is available in the updated
[Publishing](publishing.md) page.

Other features of the assistant are that, in the local mode, a branch or tag
can be specified to pinpoint a commit, and that the test build of the crate can
be skipped with `--skip-build`.

### Move manifest and lock files to top-level folder

PR [#501](https://github.com/alire-project/alire/pull/501).

The metadata information about a crate/release has been reworked to simplify
user workflows and internal operation. Metadata is stored in the manifest file,
which as of this PR is always called `alire.toml` and located at the root
directory of an Alire-enabled workspace. A companion lock file, `alire.lock`,
stores information about the dependency solution and overrides.

These two files can be safely put under version control. The manifest, in
particular, is intended to evolve with your Ada project, by being an up-to-date
record of any necessary dependencies and other properties (version, project
files, executables, maintainers, etc.).

The manifest internal format has been simplified by eliminating the possibility
of multiple releases from its contents, which removes some nesting, and
removing or making optional some fields that only make sense at the time of
publishing a crate to some index. Check the [catalog-format-spec.md] file for
details.

The `alire` directory continues to exist, and it is used to store the source
code of dependencies, local configuration and backup files. It can be safely
ignored for VCS, as its contents are either not critical or can be
reconstructed from the manifest information.

### New `alr with --versions` switch

PR [#464](https://github.com/alire-project/alire/pull/464).

A new `alr with --versions` switch is available to obtain version-focused
information of dependencies. Namely, the combined dependencies on a crate are
shown, with the release in the solution, and the last known version for the
crate:
```
CRATE      DEPENDENCY      SOLVED  LATEST
a_project  (root)          0.0.0   unknown
hello      ^1              1.0.1   4.0.0
libhello   (^1.0) & (~1.0) 1.0.1   2.0.0
superhello *               1.0.0   1.0.0
unobtanium *               missing unknown
wip        *               /fake   unknown
```

### New `alr with --graph` and `alr with --tree` switches

PR [#465](https://github.com/alire-project/alire/pull/465).

The ASCII art dependency graph generated with `graph-easy`, that was printed at
the end of `alr with --solve` output, is moved to its own `alr with --graph`
switch. A fallback tree visualization is generated when `graph-easy` is
unavailable. This new tree visualization can also be obtained with `alr with
--tree`:
```
my_project=0.0.0
├── hello=1.0.1 (^1)
│   └── libhello=1.0.1 (^1.0)
├── superhello=1.0.0 (*)
│   └── libhello=1.0.1 (~1.0)
├── unobtanium* (direct,missed) (*)
└── wip* (direct,linked,pin=/fake) (*)
```

### Automatically 'with' GPR project files from dependencies

PR [#458](https://github.com/alire-project/alire/pull/458).

When adding or removing dependency with `alr with`, a list of `with` statement
for each project files of the dependencies can be automatically added to the
root project file:
```
-- begin auto-gpr-with --
--  This section was automatically added by Alire
with "libhello.gpr";
-- end auto-gpr-with --

project Test is
...
```

This feature can be permanently enabled or disabled with a local or global
configuration option:
```bash
alr config --global --set auto-gpr-with false
```

Crates with project files not compatible with this feature can disable it using
the `auto-gpr-with` entry:
```toml
auto-gpr-with=false
```

### Show release-specific dependency sets in solutions

PR [#453](https://github.com/alire-project/alire/pull/453).

The dependency solution shown with the `--solve` switch now details for each
release the particular version set with which a dependency is brought into the
dependency closure. For example:

```
Dependencies (graph):
   hello=1.0.1      --> libhello=1.0.1 (^1.0)
   superhello=1.0.0 --> libhello=1.0.1 (~1.0)
```

### Use crate metadata when pinning to a directory

PR [#450](https://github.com/alire-project/alire/pull/450).

When pinning a dependency to a directory (`alr pin|with <crate> --use`), detect
if the target contains Alire metadata (as usual, in an `alire` subdir). In that
case, use it to determine further dependencies and project file scopes. Also,
the target dependency name is verified.

For such a target directory, a shortcut `with` command is available since the
crate name can be determined from the metadata: `alr with --use
/path/to/target` (note the absence of a crate name).

### Allow working with incomplete solutions

PR [#447](https://github.com/alire-project/alire/pull/447).

Before this patch, any change in dependencies that resulted in an incomplete
solution caused a final "invalid solution" error. Now, any incomplete solution
will be presented to the user with details about the unfulfilled dependencies.
This solution can be accepted and worked with normally, although the user is
responsible to provide in the environment any missing project files.

This change affects all commands that compute a dependency solution, i.e.,
`get`, `pin`, `update`, `with`.

### Use a directory to fulfill a dependency

PR [#439](https://github.com/alire-project/alire/pull/439)

A local path can now be used to fulfill a dependency. The path can be supplied
during initial dependency addition or afterwards during pinning, via the
`--use` switch. Such a path will be added to the environment generated by `alr
setenv`. Examples:

```bash
$ alr with some_crate --use /some/absolute/or/relative/path
# To simultaneously add a dependency and the directory to use for its GPR file.
# The dependency needs not to exist in the loaded indexes.

$ alr with indexed_crate
$ alr pin indexed_crate --use /path/to/gpr/containing/folder
# To pin a previously added dependency.
```
