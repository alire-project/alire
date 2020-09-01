# User-facing changes log

This document is a development diary summarizing changes in `alr` that notably
affect the user experience. It is intended as a one-stop point for users to
stay on top of `alr` new features.

### New `alr with --versions` switch

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

When pinning a dependency to a directory (`alr pin|with <crate> --url`), detect
if the target contains Alire metadata (as usual, in an `alire` subdir). In that
case, use it to determine further dependencies and project file scopes. Also,
the target dependency name is verified.

For such a target directory, a shortcut `with` command is available since the
crate name can be determined from the metadata: `alr with --url
/path/to/target` (note the absence of a crate name).

### Allow working with incomplete solutions

PR [#447](https://github.com/alire-project/alire/pull/447).

Before this patch, any change in dependences that resulted in an incomplete
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
