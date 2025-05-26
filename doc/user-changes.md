# User-facing changes log

This document is a development diary summarizing changes in `alr` that notably
affect the user experience. It is intended as a one-stop point for users to
stay on top of `alr` new features.

## Release `2.1`

### New `--format` global switch to produce structured output

PR [#1851](https://github.com/alire-project/alire/pull/1851)

The global switch `--format` can be used to produce JSON format with some
commands, e.g., `index`, `search`, `show`.

This new switch accepts an optional parameter that can be JSON, TOML or YAML, to
select the desired output language:

```
$ alr --format=TOML search --crates hello
[[data]]
description = "'Hello, world!' demonstration project"
name = "hello"
[[data]]
description = "Basic library demonstration project"
name = "libhello"
```

### Allow pinning a crate in a subdirectory of a git repository

PR [#1857](https://github.com/alire-project/alire/pull/1857)

Until now, monorepos were supported in origins of indexed crates but not in user
pins. A workaround was to manually clone a repository and pin the appropriate
local path. This can now be achieved entirely within Alire, e.g.:

`alr with --use=https://github.com/myuser/mymonorepo --subdir=mycrate`

This way, `alr update` works as expected and it removes the need to manually
update these repositories.

### Configurable trusted sites list for Git repositories

PR [#1819](https://github.com/alire-project/alire/pull/1819)

The list of hosts which the `alr publish --for-private-index` and
`alr index --check` commands consider to be trusted for Git repository origins
can now be configured with the `origins.git.trusted_sites` settings key. The
existing hard-coded list still applies when using `alr publish` to submit to the
community index.

### Custom download command for archive crates

PR [#1815](https://github.com/alire-project/alire/pull/1815)

The command used to download a crate as a source archive can now be configured
using the `origins.archive.download_cmd` key of `alr settings`, instead of using
a hard-coded `curl` command. The tokens `${URL}` and `${DEST}` are replaced by
the origin URL and destination file path respectively.

For example
```sh
alr settings --set --global origins.archive.download_cmd 'curl ${URL} --netrc -L -s -o ${DEST}'
```
configures `alr` to use the default command with the addition of the switch
`--netrc` (which instructs `curl` to use the login information in the `.netrc`
file found in the user's home directory).

The default behavior is unchanged.

### Abbreviated `--tree` output for repeating dependencies

PR [#1814](https://github.com/alire-project/alire/pull/1814)

By default, repeated dependencies are now omitted by `--tree` output, e.g.:

```
$ alr show --tree libgpr2
...
Dependencies (tree):
   gnat=14.1.3 (gnat_native) (>=14)
   gnatcoll=25.0.0 (~25.0.0)
   ├── gnat=14.1.3 (gnat_native) (>=13)
   └── libgpr=25.0.0 (~25.0.0)
       ├── gnat=14.1.3 (gnat_native) (/=2020)
       └── xmlada=25.0.0 (~25.0.0)
           └── gnat=14.1.3 (gnat_native) (>=11)
   gnatcoll_gmp=25.0.0 (~25.0.0)
   ├── gnatcoll=25.0.0 (~25.0.0)
   │   └── ...
   └── libgmp=6.3.0 (*)
   gnatcoll_iconv=25.0.0 (~25.0.0)
   └── gnatcoll=25.0.0 (~25.0.0)
       └── ...
```

Whenever '...' appears, it means that the preceding release has its
dependencies already printed somewhere in the preceding tree lines.

The old behavior can be obtained by increasing verbosity with the global `-v`
switch.

### Faster `alr search` without resolving dependencies

PR [#1799](https://github.com/alire-project/alire/pull/1799)

`alr search` no longer solves dependencies of releases by default, in order to
speed up the command. The `--solve` switch can be used to achieve the old
behavior.

In the new default situation, releases that have dependencies are marked with a
'?' symbol in the STATUS column. The `--solve` switch will solve the
dependencies and replace the '?' with either nothing for a solvable release or
the usual 'X' if dependencies are unsatisfiable.

### Support for private indexes with `alr publish --for-private-index`

PR [#1745](https://github.com/alire-project/alire/pull/1745)

Automated manifest generation with `alr publish` can now be performed for crates
which are not intended for submission to the community index by supplying the
`--for-private-index` switch. This has the same effects as `--skip-submit`, and
additionally disables a number of checks that enforce submission requirements
specific to the community index.

## Release `2.0`

### `ALIRE_SETTINGS_DIR` replaces `ALR_CONFIG`

PR [#1625](https://github.com/alire-project/alire/pull/1625)

This reflects the new nomenclature of Alire settings versus crate
configuration. Also, it better reflects that the effect is on the whole library
and not only the `alr` command-line tool.

### `alr settings` replaces `alr config`

PR [#1617](https://github.com/alire-project/alire/pull/1617)

The `alr settings` command replaces the `alr config` command. This change is
introduced to tackle the confusion between the configuration of the Alire
commands and operations, and the configuration of crates.

`alr config` is still available and should work as before with the exception of
a deprecation warning message.

### Deprecation of `toolchain --install/--uninstall/--install-dir`

PR [#1614](https://github.com/alire-project/alire/pull/1614)

Toolchain selection for use by Alire is still done by using
`alr toolchain --select`.

For the installation of toolchains outside of Alire management (i.e., for
direct use with other tools, but not with Alire), the recommended
method now is to use `alr install`, e.g.:
```
# Install to the default location, <user home>/.alire/bin
$ alr install gnat_native gprbuild

# Install elsewhere
$ alr install --prefix=/path/to/installation gnat_native gprbuild
```

Removal of managed toolchains can be done by simply removing their folders
inside the toolchain cache (reported by `alr version`).

### Cache and toolchain storage location overridding

PR [#1593](https://github.com/alire-project/alire/pull/1593)

The cache directory can now be set independently of the configuration
directory, by setting the `cache.dir` config builtin to an absolute path. For
example:
```
alr config --global --set cache.dir /path/to/my/global/cache
```
Since the cache by default also contains installed toolchains, which may not be
needed to be moved elsewhere, the `toolchain.dir` can be used to dissociate
toolchain location from cache location in a similar way:
```
alr config --global --set toolchain.dir /path/to/my/toolchains
```

### New switch `alr build --stop-after=<build stage>`

PR [#1573](https://github.com/alire-project/alire/pull/1573)

From `alr help build`:

**Build stages**

   Instead of always doing a full build, the process can be stopped early using `--stop-after=<stage>`, where `<stage>` is one of:

   * sync: sync pristine sources to build location
   * generation: generate configuration-dependent files
   * post-fetch: running of post-fetch actions
   * pre-build: running of pre-build actions
   * build: actual building of sources
   * post-build: running of post-build actions

### Enable shared dependencies by default

PR [#1449](https://github.com/alire-project/alire/pull/1449)

Pre-2.0, Alire worked always in "sandboxed" mode; that is, all source
dependencies were found under `<workspace>/alire/cache`. This behavior can be
now enabled with `alr config --set dependencies.shared false`, locally or
globally.

By default, post-2.0, Alire works in "shared" mode, where sources are
downloaded once (to `~/.cache/alire/releases`) and unique builds are created
(under `~/.cache/alire/builds`) for unique configurations. This should minimize
rebuilds across crate configurations and workspaces, and eliminate risks of
inconsistencies.

Disk use is decreased by unique source downloads, but might be increased by
unique build configurations. Cache management and cleanup will be provided down
the road. The build cache can always be deleted to retrieve disk space, at the
cost of triggering rebuilds.

Unique builds are identified by a build hash which takes into account the
following inputs for a given release:

- Build profile
- Environment variables modified in the manifest
- GPR external variables declared or set
- Configuration variables declared or set
- Compiler version
- Vaue of `LIBRARY_TYPE` and `<CRATE>_LIBRARY_TYPE` variables.
- Hash of dependencies

### Automatic index updates

PR [#1447](https://github.com/alire-project/alire/pull/1447)

A new configuration option, `index.auto_update`, allows setting the refresh
period of indexes. It defaults to 24 hours and the user will be asked the first
time to allow automatic updates. Setting this option to 0 will also disable
automatic updates.

When enabled, updates may happen before executing commands that rely on
indexes: `get`, `search`, `with`, etc.

### Deprecation of `dependencies.dir` in favor of `dependencies.shared`

PR [#1419](https://github.com/alire-project/alire/pull/1419)

A new system of shared sources and builds is being implemented, which will
ensure full consistency and reuse of builds.

In the new system (still disabled; enable it by setting `alr config --set
dependencies.shared true`), dependencies will no longer be stored under
`<workspace>/alire/cache/dependencies`. Instead, three new directories are
introduced:

- `$HOME/.cache/alire/releases`: contains sources for read-only purposes and
  binary releases (except toolchains, see below).
- `$HOME/.cache/alire/builds`: contains source builds for a unique combination
  of build profile, GPR externals and environment variables.
- `$HOME/.cache/alire/toolchains`: contains downloaded toolchains.

The previous `$HOME/.cache/alire/dependencies` that contained both toolchains
and binary releases is no longer in use.

Users wanting to modify dependencies in tandem within a workspace are
encouraged to use the pin system.

If these default locations for shared dependencies must be relocated, this can
be achieved by using a new configuration path (`ALR_CONFIG` or `-c` global
switch). In that case, the aforementioned paths will be found under
`/path/to/config/cache`.

### Set working directory with `--chdir`

PR [#1479](https://github.com/alire-project/alire/pull/1479)

A new switch `--chdir` (short form `-C`) is introduced which requires a target
directory argument. `alr` then runs as though it were invoked in that
directory.

### Request review of an index submission with `alr publish --request-review`

PR [#1409](https://github.com/alire-project/alire/pull/1409)

When a submission has passed all server-side tests, for the time being it must
be reviewed and merged manually. This can now be done with `alr publish
--request-review <num>`.

### Cancel an index submission with `alr publish --cancel`

PR [#1406](https://github.com/alire-project/alire/pull/1406)

A pending submission can be closed with
`alr publish --cancel <num> --reason <text>`.

### Track user's index submissions with `alr publish --status`

PR [#1400](https://github.com/alire-project/alire/pull/1400)

The new `alr publish --status` switch will print a table with unmerged pull
requests opened by the user against the community index repository.

### Automatic release submission during `alr publish`

PR [#1398](https://github.com/alire-project/alire/pull/1398)

`alr publish` will now prompt to continue after manifest creation into a series
of steps culminating on the creation of a draft pull request on the community
index repository.

The new steps will perform all necessary actions: forking of the community
repository into the user account, cloning, committing of the new manifest, and
pull request creation.

For `alr` to be able to do these steps on the user's behalf, the user has to
provide a 'Personal Access Token (PAT)' with 'repo' permissions.

The old behavior, ending the assistant after manifest creation, can be achieved
with the new `--skip-submit` flag.

### Removal of `alr test --docker`

PR [#1366](https://github.com/alire-project/alire/pull/1366)

The option to test indexed releases with the local `alr` using a Docker image
has been removed, as it never made too much sense for `alr` to invoke itself,
and it introduced unwanted complexity into the `alr test` command.

### Global sharing of dependencies via config setting

PR [#1367](https://github.com/alire-project/alire/pull/1367)

A new built-in configuration key can be used to define a directory where all
dependencies will be stored:

`alr config --set --global dependencies.dir /abs/path/to/existing/dir`

Without `--global`, as usual, the setting will only affect the working crate.

The use of this feature entails a penalty in that crate configuration files will
be regenerated before every build to ensure consistent build profiles.

Caveat emptor: dependencies built by several dependents with different
configuration options or scenario variables might cause race conditions or
other unexpected issues. Use this feature with caution.

### Test of a working crate with `alr test`

PR [#1356](https://github.com/alire-project/alire/pull/1356)

This PR enables the use of `alr test` on local crates. Previously, it could only
be used on indexed ones.

By default, running `alr test` will build the crate in release mode. This
behavior can be overridden by defining one or more [test
actions](https://alire.ada.dev/docs/#release-information).

### Binary releases moved to system cache from system config directory

PR [#1349](https://github.com/alire-project/alire/pull/1349)

Alire was storing large binary releases like compilers in the config location,
which is against best practices.

Users are advised to delete the old location to recover disk space, or to
manually move the contents to avoid redownloading toolchains.

- Old location: `<user home>/.config/alire/cache`
- New location: `<user home>/.cache/alire`

### Installation of indexed crates

PR [#1335](https://github.com/alire-project/alire/pull/1335)

It is now possible to install an indexed crate directly:
```
$ alr install hello
```
This is roughly equivalent to
```alr get hello && cd hello* && alr install```

The main differences are:
- Cleanup is automatic.
- Several crates can be installed in one go, e.g.: `alr install hello hangman`.
- `alr get` will always retrieve the latest version, whereas `alr install` will
also require a complete solution to dependencies.

### Installation of local crates

PR [#1322](https://github.com/alire-project/alire/pull/1322)

`alr install` without arguments performs the installation of the current crate.
With `--info`, it shows the contents of an installation prefix. For example:

```
$ alr -n init --bin mycrate && cd mycrate
$ alr install
$ alr install --info
Installation prefix found at /home/user/.alire
Contents:
   mycrate=0.1.0-dev
```

Or, to install the hangman game:

```
$ alr get hangman && cd hangman*
$ alr install
```

### New subcommand `alr install`

PR [#1302](https://github.com/alire-project/alire/pull/1302)

A new subcommand `alr install` allows the installation of binaries to a location
intended to be added to the user's PATH. The default install location is
`$HOME/.alire`, with binaries going into `$HOME/.alire/bin`.

This is a experimental feature that will see improvements and tweaks in further
PRs and as we gather feedback on its usage.

At present, only binary releases can be installed (e.g., compilers, `gprbuild`,
`gnatprove`). There is no ability to uninstall releases either
(but reinstallation can be forced).

Only one version per executable can be installed, meaning that, for example,
only one toolchain version can exist in an installation prefix. So, this
feature is intended to make the user's preferred version of a crate generally
available in the system for use outside of Alire, but not to replace e.g. the
ability of Alire to use several compilers, or to reuse compiled libraries as
dependencies in several workspaces.

Examples:

```
$ alr install gnatprove
ⓘ Installing gnatprove=12.1.1...
ⓘ Installation complete.

$ alr install
Installation prefix found at /home/jano/.alire
Contents:
   gnatprove=12.1.1

$ PATH+=:$HOME/.alire/bin gnatprove --version
Usage: gnatprove -Pproj [switches] [-cargs switches]
...

$ alr install gnatprove^11
error: Requested release gnatprove=11.2.3 has another version already
installed: gnatprove=12.1.1. (This error can be overridden with --force.)

$ alr --force install gnatprove^11  # Downgrade installation
```

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

## Release `1.2`

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

Check more examples and details in the catalog specification section
[Using configuration](catalog-format-spec#using-crate-configuration).

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
`alr config --set --global editor.cmd '<BINARY> <ARGS>'`
for custom editor and command line arguments. The token ${GPR_FILE} is
replaced by a path to the project file to open.

For instance:
```shell
$ alr config --set --global editor.cmd 'emacs ${GPR_FILE}'
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
[Publishing](publishing) page.

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
publishing a crate to some index. Check the [catalog-format-spec](catalog-format-spec)
file for details.

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
