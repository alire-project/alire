# User-facing changes log

This document is a development diary summarizing changes in `alr` that notably
affect the user experience. It is intended as a one-stop point for users to
stay on top of `alr` new features.

## Release `3.0`

### New `--builtin` switch for `alr settings`

PR [#1912](https://github.com/alire-project/alire/pull/1912)

A new `--builtin` switch has been added to the `alr settings` command to avoid
unintended silent errors. This switch can only be used with `--set`, `--get`,
or `--unset` and ensures that the setting being operated on is a built-in
setting.

When `--builtin` is used with a non-built-in setting, an error is raised.
Conversely, when operating on a built-in setting without using `--builtin`, a
warning is printed suggesting the use of `--builtin`.

### New test command

PR [#1874](https://github.com/alire-project/alire/pull/1874)

The `alr test` command can now be configured with a new `[test]` section in the
Alire manifest, with the option to use a built-in test runner or an external
command.

```toml
[test]
runner = 'alire'
# OR
command = ["my", "custom", "runner"]
```

The built-in test runner allows crate authors to compile and run tests from
simple `.adb` files. It compiles and runs every `.adb` file of the subcrate's
`/src` folder as a separate test. A skeleton test subcrate is now generated
with `alr init` (this can be prevented with the `--no-test` flag).

For backwards compatibility, running `alr test` without a `[test]` section in
the manifest will still run local test actions, but they should be considered
deprecated. The remote testing capabilities of `alr test` have been removed.

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

PR [#775](https://github.com/
