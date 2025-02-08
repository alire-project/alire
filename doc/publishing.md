# Publishing your projects in Alire

Publishing a project in Alire is done with the help of the `alr publish`
command. The steps to take are described after some introductory concepts (jump
to these steps directly [here](#detailed-steps); you can also ask for help on
the [gitter channel](https://gitter.im/ada-lang/Alire) of the project.

## Automated publishing (TL;DR.)

The simplest publishing experience, provided you have a GitHub account and
Personal Access Token, consist on issuing
```
alr publish
```
at the root of your workspace, when said workspace is an up-to-date clone of a
git repository.

The publishing assistant will review your submission, point out any necessary
fixes or additional information required, and request a pull into the community
index on GitHub on your behalf.

Read on for the details underlying these automated steps, or in case you need
to perform further tweaking.

## Creating a GitHub Personal Access Token

A Personal Access Token (PAT) allows Alire to act on your behalf to fork the
community index, push the new release manifest to a new branch in your own fork,
and finally open a pull-request against the community repository.

The PAT, once created, is a plain string. You can either export the environment
variable `GH_TOKEN` set to this string, or provide it when Alire asks for it.

There are two kinds of PATs on GitHub: classic and fine-grained. The latter are
in beta and not documented here yet. Follow these steps to create a classic PAT:

1. On the main https://github.com page, after having logged in, click on your
   profile photo on the top-right corner.
1. Click on "Settings" in the list of options in the profile menu.
1. Click on "Developer settings" entry at the bottom in your Settings page.
1. Click on "Personal access tokens" and then "Tokens (classic)".
1. Click on "Generate new token" and then select the classic variant.
1. In the "Select scopes" section, under "repo", check "public_repo". That is
   the only permission needed for this PAT.
1. Click on "Generate token" at the bottom.

You will get the PAT string after completing the generation.

## General concepts

The community index is a collection of
[TOML](https://github.com/toml-lang/toml) files stored in the
[alire-index](https://github.com/alire-project/alire-index) repository, under
the [index](https://github.com/alire-project/alire-index/blob/master/index)
directory. Each file contains a release for a crate and is named after the
crate and version it contains. A file contains the description of a release,
with other metadata.

The complete specification of such TOML files is available in this
[document](catalog-format-spec).

## New crates and releases

Publishing a new crate is achieved through a pull-request against the index
repository, in which the TOML file for the release must be provided.

### Index branches

The community index is supported through two kinds of branches:

- `stable-x.x` branches are used by stable versions of `alr`.
- `devel-x.x` branches are used to introduce breaking changes in the index
  format, during the development of `alr`.

Your `alr` version knows which branch to use, so you do not need to manually
select one. When using `alr publish` to assist on creating a release, `alr`
will either create the pull request against the proper branch, or you will
be provided with an upload link for the branch your `alr` is using.

However, when submitting releases manually, you can decide to which branch
they will be added: selecting the latest stable branch results in the release
becoming immediately available to the latest stable `alr`. Conversely, using
the latest development branch will make the releases available for testing by
unstable clients, and will become generally available with the next stable
release of `alr`.

## Checks on contributions

Each crate is "owned" by a list of maintainers, provided with the
`maintainers-logins` property of the crate file. After the initial submission,
which will be manually approved (see the [policies](policies) for details),
the maintainers of a crate are the only people allowed to submit new releases
or metadata modifications to the corresponding crate.

Other checks your submission will go through are:

- It contains all required metadata.
- It builds on all of our CI configurations.
    - You can disable an unsupported target with the `available` property.

## Best practices

See the section on [best practices](policies#best-practices) for crates
before publishing your first release.

## Detailed steps

Depending on how you develop your project, you can use one of the following
methods to prepare your release submission:

### Starting from a git repository that contains an Alire workspace

For this common use case, you need:

- A git repository that is clean and up-to-date with its remote.
   - The repository already contains the release you want to publish.
   - The commit with the release must exist both locally and at the remote.
- The repository must also be an Alire-enabled workspace:
   - It contains a top-level `alire.toml` manifest describing the release.
- The remote host must be one of a few trusted major open-source sites.
   - This requirement is motivated by vulnerabilities identified with SHA1,
     whose migration to a stronger hash is
     [not yet complete](https://git-scm.com/docs/hash-function-transition/) in `git`.
   - `alr` will inform you if your host is not supported. Please contact us if
     you think a site should be allowed. The complete list can be consulted by
running `alr publish --trusted-sites`.
   - This is a temporary measure until more sophisticated publishing automation
     is supported. See the [Starting with a remote source archive](#starting-with-a-remote-source-archive) case
for alternatives to this scenario (you are not forced to change your code
hosting, or even have an online repository).

By default, the last commit is used for the release. You can alternatively
provide another commit, tag, or branch. In any case, the git revision will be
used to obtain a final commit. That is, a release cannot evolve with a branch,
or be updated by moving a tag.

- Within the repository, issue

`alr publish`

to use the last commit. You can, alternatively, issue:

`alr publish . <commit|tag|branch>`

Note the path between `publish` and your non-commit revision. Likewise, you can
run this command from outside your repository, as long as you supply the proper
path to it.

At this point, `alr publish` will carry out a few tests and, if everything
checks out, it will create a `${repo_root}/alire/releases/crate-version.toml`
file. This file must be submitted to the community index via a PR. `alr` will
offer to create the pull request for you, unless you specify `--skip-submit`.
If so, a link for conveniently creating this PR will also be provided by `alr`:

- Upload the generated index manifest file (`crate-version.toml`) to the
  supplied page link on GitHub and create a pull-request.

### Starting with a remote repository, without local clone

This case is analogous to the previous one, but you don't need the local
repository. The same considerations about allowed hosts discussed in the
previous scenario apply:

- The repository already contains the commit with release you want to publish.
- The repository must also be an Alire-enabled workspace:
   - It contains a top-level `alire.toml` manifest describing the release.
- The remote host must be one of a few trusted major open-source sites.
   - This requirement is motivated by vulnerabilities identified with SHA1,
     whose migration to a stronger hash is [not yet complete]
     (https://git-scm.com/docs/hash-function-transition/) in `git`.
   - `alr` will inform you if your host is not supported. Please contact us if
     you think a site should be allowed. The complete list can be consulted by
running `alr publish --trusted-sites`.

The only difference when invoking `alr` is that you must supply the remote URL
and commit (not a tag or branch). The commit must exist in the repository:

`alr publish <URL> <commit>`

The checks will be carried out and the outcome will be the same as in the
previous scenario.

### Starting with a remote source archive

This case can be used when you use another VCS other than `git`, or do not work
with an online repository.

In this use case, you start from an already prepared final remote tarball/zipball:

- The archive **must** contain a single directory (name not important)
   containing, in turn, the sources. This is the kind of archives
   automatically generated by GitHub, GitLab, Sourceforge... or through
   `git archive`.
- The `alire.toml` manifest must be placed at the top-level with the rest of
   your sources (inside the same single directory just described), containing
   all required information except for the `[origin]` table, which will be created
   by `alr`.
- This archive **must not** contain the `alire` directory generated by
   `alr` in working directories. The `alire` directory is designed to be
    put in your VCS ignore list.

With the source archive already uploaded to the online host where it is going
to be served (there are no restrictions on this host), you should issue

`alr publish <URL>`

and the publishing process will carry on as in the previous cases, performing
the checks and providing you with a file to submit to the index, and an upload
URL to do so.

### Starting with a local source folder

Invoking `alr publish --tar` inside an Alire workspace will result in the
creation of a source archive at `${CRATE_ROOT}/alire/archives/`. This archive
must be manually uploaded by the user to a publicly accessible hosting service.

After the upload, the user can supply the URL to fetch this archive to the
publishing assistant (which will be waiting for this information), and the
assistant will resume as if it had been invoked with `alr publish <URL>`
(see [Starting with a remote source archive](#starting-with-a-remote-source-archive)).

### Support for complex projects whose sources become multiple Alire crates

In case your project does not easily map to a single Alire crate (e.g., because
you manage multiple project files with different dependencies, or there are
other reasons to keep the sources together even if they generate several
crates), you have several options.

The simplest one is to store each crate in a subdirectory within the
repository, with its corresponding Alire manifest, sources and project files.
With the repository up-to-date with the remote, and the local copy checked out
at the desired commit, issuing `alr publish` in each subdirectory will properly
recognize that the crate is nested below the repository root. Furthermore, when
using this method, all nested crates will share the same storage when retrieved
as dependencies.

A similar alternative would be to publish each crate relying on source archives
In this case you can use `alr publish --tar` normally inside each subdirectory.
Compared with the previous options, there is no disadvantage to this method if
you favor source archives.

Another possibility would be to use a bit of scripting to create temporary
subfolders with the described organization, and again using `alr publish --tar`
normally.

Finally, the `alr publish` command provides a `--manifest <file>` switch to
work in place with several crates. You can have different manifests at custom
locations (other than the expected `./alire.toml`) and provide each one in turn
with the `--manifest` switch to create their respective crate. In this case,
`alr` temporarily uses the given file as the root manifest, so all sources will
be packaged for each crate. This is a bit wasteful, but as long as each crate's
project files are properly defined (no shared sources), this remains an option
to split the sources into crates. With the current support for autodetection of
crates in subdirectories, this option is not recommended for new repositories.

### Starting from other configurations

If your case does not fit well into any of the situations above we definitely
want to hear about it, to see how it can be brought into existing or new Alire
workflows.

### Creating the PR via cloning.

Instead of uploading the generated index manifest file via the GitHub upload
link, you can follow the usual procedure to submit a PR to a GitHub repository:

1. Fork the community index to your GitHub account.
1. Clone your fork locally and place generated manifest at the intended folder.
1. Commit and push the changes to your fork.
1. Create the pull request from your fork against the community repository
   through the GitHub web interface (or the
   [`hub`](https://github.com/github/hub) tool).
    1. The base branch you select for the pull request will determine where
       your changes will become available; see the section on [index
       branches](#index-branches) for details.

## Publishing outcome

Once the pull request is verified and merged, the new release will become
available for normal use after running `alr index --update-all`. The open
source Ada ecosystem needs all the help it can get, so thank you for
contributing!

## ALR Badge

If you like, you can add a nice, shiny badge to your project page which links
back to the [Alire website](https://alire.ada.dev). This can even serve as a
reminder to republish your project once you published a new release, because
the badge shows the latest version of your project that is known to Alire.

The [Alire website](https://alire.ada.dev) is updated once a day, every day.
Hence, after we accepted and merged your pull request, it might take up to a
day for your changes to appear there, usually less.

To add the badge, all you need to do is add the line

```
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/YOUR_CRATE.json)](https://alire.ada.dev/crates/YOUR_CRATE.html)
```

to your `README.md`. Of course, you need to replace the string `YOUR_CRATE`  with
your actual project's crate name.

Here's an example:

```
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/hal.json)](https://alire.ada.dev/crates/hal.html)
```

This will be shown as:

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/hal.json)](https://alire.ada.dev/crates/hal.html)

## Publishing to a local/private index

Having a local or private index may be useful sometimes, be it for local
testing, or for private crates not intended for publication. For more
information on private indexes, see
[this guide](private-crates#using-a-private-index).

To "publish" a crate to a private index, run
```
alr publish --for-private-index [<path|URL> [<commit|tag|branch>]]
```
where the `--for-private-index` switch disables the submission step and certain
checks which are only applicable to the community index, and the remaining
arguments function as described above. This will generate a manifest file which
you can place at the indicated path (relative to the location of `index.toml`)
in your private index. If you are using a remote Git repository which is not on
one of the community index's trusted hosts, you will need to configure it with
the `origins.git.trusted_sites` [setting](settings).

One important thing to note is that publishing from a local repository will
detect the URL configured as the Git remote (as displayed by
`git remote show origin`). If this is not configured with the recommended form
(as discussed [here](private-crates#git-repositories)), you may wish to pass the
desired URL explicitly.
