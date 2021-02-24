# User-facing changes log

This document is a development diary summarizing changes in `alr` that notably
affect the user experience. It is intended as a one-stop point for users to
stay on top of `alr` new features.

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
