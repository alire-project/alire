# Using Alire with private crates

By default, Alire uses the open-source crates available through the community
index, but it can also be configured to fetch non-public crates (e.g. during
development or when working with proprietary code). There are two key components
required to make a crate available to Alire; the crate must be searchable in an
*index* (though this requirement can be temporarily circumvented with
[pins](catalog-format-spec#work-in-progress-dependency-overrides)), and the
crate's source files must be fetchable from an *origin*.


## Using a private index

### Creating an index

An Alire index is nothing more than a directory containing a collection of
release manifest files in a certain directory structure. Full details of the
format of an index can be found in
[the catalog format specification](catalog-format-spec).

To create a new index, simply create an empty directory at a location of your
choice, and add to it a file called `index.toml` containing one line with the
form `version = "x.x.x"`, specifying the index format used. The range of
versions Alire is compatible with can be found by running `alr version`; when
creating a new index you should simply use the highest version listed under
`compatible index versions:`.

If future updates to Alire affect compatibility with existing indexes, they will
be listed in
[BREAKING.md](https://github.com/alire-project/alire/blob/master/BREAKING.md).

### Configuring an index

To start using your new index, run
```sh
alr index --add=<path> --name=<name>
```
where `<name>` is a human-friendly label that `alr` will use to refer to it.

It is possible to configure multiple indexes (with any conflicts resolved using
a priority order specified by the `--before` switch). For example, you may wish
to configure both the community index and a private index of your own
unpublished crates.

If you intend to use your private index as the only index (i.e. without also
keeping the community index configured), you will need to add crates for the
compiler and GPRbuild. See the community index's
[`gnat_*` crates](https://github.com/alire-project/alire-index/tree/HEAD/index/gn) and
[`gprbuild` crate](https://github.com/alire-project/alire-index/tree/HEAD/index/gp/gprbuild)
for more details on how this can be achieved.

### Adding a new crate to an index

In order to add a crate to the index, you must create a suitable manifest file
to describe the release, and place this manifest at the appropriate location in
the index directory.

The creation of the manifest file is automated through the `alr publish`
command; submission to the community index can be disabled by supplying the
`--for-private-index` switch. See [publishing](publishing) for more detail on
this process.

The resulting manifest file must then be copied to the index directory. The
`alr publish` command will provide instructions on the correct location at which
to place it.

The newly added crate will become available for use with `alr` immediately,
unless the crate being published contains `"provides"` definitions, in which
case a call to `alr index --update-all` will be required.

### Remote indexes

You may wish to share your private index outside of your local filesystem. Any
means of synchronizing the contents of the index directory will suffice, but
Alire will manage this process automatically if you use a remote Git repository.

It is often useful to have other files in the same repository as the index
(a `README`, CI configuration, templates etc.), so the index itself is located
in a first-level subdirectory of the repository (conventionally called `index/`,
though `alr` searches for any directory containing an `index.toml` file).

To start using a remote Git repository as an index, run
```sh
alr index --add=<URL> --name=<name>
```
Note that `<URL>` can point directly to the remote repository, so no local clone is required.

Changes on the remote index will not take effect until `alr` performs an index
update, either with `alr index --update-all`, or through a scheduled auto-update
(which is performed every 24 hours by default).

Once configured, there is no practical difference between the community index
and a private index on your own remote Git repository.


## Requiring user authentication

In addition to listing crates in a private index, you may wish to limit access
to authenticated users only. This is supported for crates and indexes made
available as Git repositories, and for crates made available as archive files.

### Git repositories

Crates and indexes can be fetched as clones of a git repository, and for most
users this option will require minimal additional configuration.

Repositories are cloned and updated by calling out to the system's `git`
command, so `git` must be configured with appropriate credentials to
authenticate with any private origin. This usually means either Git-over-SSH
(see your SSH client's documentation), or HTTPS with `git`'s credential handling
(documented [here](https://git-scm.com/docs/gitcredentials)). As a general rule,
if a `git clone` or `git fetch` on a crate/index origin succeeds, then so will
any corresponding dependency updates with `alr`.

The main consideration when using such repositories with Alire is ensuring that
the correct form of URL is used. It must be apparent to `alr` that the URL
refers to a Git repository (which can be disambiguated with a `git+` prefix),
and `git` must be able to infer which protocol should be used (documented
[here](https://git-scm.com/docs/git-clone#URLS)). It is therefore recommended
that URLs use the schemes `git+https://` or `git+ssh://` as appropriate.

Repositories on the local filesystem can be specified with the form
`git+file:/some/path`, though this is not recommended except as a temporary
arrangement for testing purposes.

### Source archives

Crates can also be fetched as an archive file (either tarball or Zip) containing
the relevant sources. `alr` simply requires some means of downloading this file
from a URL to a local filesystem location.

The command used to fetch such files is specified by the [settings](settings)
key `origins.archive.download_cmd`. By default `alr` uses the command
`curl ${URL} -L -s -o ${DEST}`, which does not attempt any form of
authentication, but this can be changed to any equivalent alternative which
implements a desired authentication scheme. The command should download the file
to `${DEST}` (the full file path, not a directory), and must not pollute the
containing directory with other files.

The simplest way to enable user authentication is to configure the server to
accept HTTP Basic authentication and supply `curl` with the switch `--netrc`
(or equivalently `-n`), which instructs it to scan the `.netrc` file in the
user's home directory and attempt to authenticate with any credentials found
there (see `man curl` and `man netrc` for more detail). This is achieved by
invoking
```sh
alr settings --set --global origins.archive.download_cmd 'curl ${URL} -n -L -s -o ${DEST}'
```
Note that most terminals will perform substitutions on double-quoted strings
containing `${SOMETHING}`, so it is important to enclose the value in
single-quotes.

If you wish to use a `.wgetrc` configuration file instead, the equivalent `wget`
command is `'wget ${URL} -q -O ${DEST}'`.

This setting only accepts a simple space-separated command, with no scripting
functionality. If this is not sufficient, you can write more complex logic (or
commands with arguments containing spaces) to a separate script, for instance
by setting the value to `'python /path/to/my_script.py ${URL} ${DEST}'`.
