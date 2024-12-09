# Using Alire with private crates

By default, Alire uses the open-source crates available through the community
index, but it can also be configured to fetch non-public crates
(e.g. during development or when working with proprietary code). There are two
key components required to make a crate available to Alire; the crate must be
searchable in an index (though this requirement can be temporarily circumvented
with `alr pin`), and the crate's source files must be fetchable from an origin.

## Using a private index

### Creating an index

An Alire index is nothing more than a Git repository containing manifest files
corresponding to each crate release in the correct directory structure.
Repositories which require user authentication can be used in the same way as
crate repositories (see [below](#git-repositories)).

It is often useful to have other files in the same repository as the index
(a `README`, CI configuration, templates etc.), so the index itself is located
in a first-level subdirectory of the repository (conventionally called `index/`,
though `alr` searches for any directory containing an `index.toml` file).

This subdirectory should contain only an `index.toml`
file and one or more `cr/crate_name` subdirectories within which the crate
manifests themselves are located. The `index.toml` file contains one line with
the form `version = "x.x.x"`, specifying the index format used. The range of
versions Alire is compatible with can be found by running `alr version`, and
breaking changes are listed in
[BREAKING.md](https://github.com/alire-project/alire/blob/master/BREAKING.md).

See [the catalog format specification](catalog-format-spec) for more detail on
the format of the index.

#### Local indexes

For testing purposes, indexes can be configured on the local filesystem. These
need not be Git repositories, but can instead be a plain directory specified
with a `file:/some/path` URL (which, unlike for remote indexes, may optionally
refer directly to the index directory instead of its parent).

### Configuring an index

To start using such an index, run

`alr index --add=<URL> --name=<name>`,

where `<name>` is a human-friendly label that `alr` will use to refer to it.

It is possible to configure multiple indexes (with any conflicts resolved by a
priority order specified by the `--before` switch). For example, you may wish to
configure both the community index and a private index of your own unpublished
crates.

Once configured, there is no practical difference between the community index
and a private index stored locally on disk or on your own infrastructure.

### Adding a new crate to an index

The majority of the process of adding a crate to a private index is automated
through the `alr publish` command; submission to the community index can be
disabled by supplying the `--for-private-index` switch. This will produce a
manifest file, which must then be uploaded manually, according to the
requirements of your index's hosting arrangement. See [publishing](publishing)
for more detail on this process.

Newly added crates will become available when `alr` performs an index
update, either with `alr index --update-all`, or through a scheduled
auto-update. Local indexes do not require an update and additions will take
effect immediately, unless the crate being published contains `"provides"`
definitions.

## Private crate origins

In addition to listing crates in a private index, you may wish to limit access
to authenticated users only. This is supported for crates made available as Git
repositories or as archive files.

### Git repositories

Crates can be fetched as clones of a git repository, and for most users this
option will require minimal additional configuration.

Crate repositories are cloned and updated by calling out to the system's `git`
command, so `git` must be configured with appropriate credentials to
authenticate with any private origin. This usually means either Git-over-SSH
(see your SSH client's documentation), or HTTPS with `git`'s credential handling
(documented [here](https://git-scm.com/docs/gitcredentials)). As a general rule,
if a `git clone` or `git fetch` on a crate origin succeeds, then so will
any corresponding dependency updates with `alr`.

The main consideration when using such repositories with Alire is ensuring that
the the manifest in the index (or the pin) uses the correct form of the URL. It must be
apparent to `alr` that the URL refers to a Git repository (which can be
disambiguated with a `git+` prefix), and it must be apparent to `git` which
protocol should be used (documented
[here](https://git-scm.com/docs/git-clone#URLS)). It is therefore recommended
that URLs use the schemes `git+https://` or `git+ssh://` as appropriate.

Repositories on the local filesystem can be specified with
`git+file:/some/path`, though this is not recommended except as a temporary
arrangement for testing purposes.

### Source archives

Crates can also be fetched as an archive file (either tarball or Zip) containing
the relevant sources. `alr` simply requires some means of downloading this file
from a URL to a local filesystem location.

The command used to fetch such files is specified by the `alr settings` key
`origins.archive.download_cmd`. By default `alr` uses the command
`curl ${URL} -L -s -o ${DEST}`, which does not attempt
any form of authentication, but this can be changed to any equivalent
alternative which implements a desired authentication scheme.

The simplest way to enable user authentication is to supply `curl` with the
switch `--netrc` (or equivalently `-n`), which instructs it to scan the `.netrc`
file in the user's home directory, and performs HTTP Basic authentication with
any credentials found there (see `man curl` and `man netrc` for more detail).
This is achieved by invoking
```sh
alr settings --set --global origins.archive.download_cmd 'curl ${URL} -n -L -s -o ${DEST}'
```
Note that most terminals will perform substitutions on double-quoted strings
containing `${SOMETHING}`, so it is important to enclose the value in
single-quotes.

If you wish to use a `.wgetrc` configuration file instead, the
equivalent `wget` command is `'wget ${URL} -q -O ${DEST}'`.

This setting only accepts a simple space-separated command, with no scripting
functionality. If this is not sufficient, you can write more complex logic or
arguments containing spaces to a separate script, for instance by setting the
value to `'python /path/to/my_script.py ${URL} ${DEST}'`.
