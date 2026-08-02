    AEP: 2
    Title: A complete develop/publish workflow for Alire crates
    Author: Alejandro R. Mosteo <amosteo@unizar.es>
    Status: Implemented
    Created: 06-jul-2020

Abstract
========

Until date, we do not offer guidelines for a complete closed loop on how to use
Alire to develop/publish/update a crate. Likewise, we do not have official
guidelines on how to distribute, outside of the community index, a project that
uses Alire to obtain its dependencies. 

This document discusses a possible solution for both the closed-loop lifecycle
of a crate, and the open-loop distribution of projects/releases that are
themselves not (yet) indexed in Alire.

Nomenclature
============
- Index manifest: The manifest for a release stored in an index repository.
- Local/User/Bundled manifest: The manifest stored with the sources of a
  release.

Rationale
=========

The root of the problem
-----------------------

The file `${crate}/alire/crate.toml` (henceforth, the manifest file) is
currently regenerated from index information during `alr get`, and rewritten
when dependencies are modified via `alr with`. This is a lossy
process (although this could be fixed) in which the manifest becomes
platform-dependent. Hence, we advice against including this
file in version control, and we ask for it to be excluded from sources
submitted to the community index.

Even if this file regeneration were not lossy, machine-generated output mangles the
original formatting. Even with a good pretty-printer for TOML, any comments in
the original file would be lost. This goes against maintainers, that have to
track this manual file somehow, yet exclude it from submissions.

On the other hand, developers publishing code outside of Alire, with
dependencies from Alire, could distribute the manifest file in place and ask
the users to run `alr update` to obtain all dependencies. This is currently
frowned upon, again because this can only work today for manifests that are
platform-independent.

Proposal
--------

The key change is to fully embrace manual edition of the manifest, and put it
under version control, in a similar vein to other package managers like
`cargo`.

The only caveat concerns (at the moment) `alr with`, which is leveraged to
comfortably make dependencies available in one step (and behind the scenes
updating the manifest and the project file(s). Thus, there is a need to keep
`alr with` functionality without regenerating the manifest file, to respect
user's manual editions.  To meet both ends, the proposal is to allow `alr with`
to modify the manifest in a non-destructive incremental way, only for the
purpose of editing dependencies.

Advantages for packagers are:

1. They can format/comment the manifest without their editions being lost.
1. The whole publish process can be automated (see the
[examples](#workflow-examples) section).

Advantages for regular users:

1. They can use `alr with` as currently, without manually editing the manifest
(for now, for platform-independent crates).
1. They can instruct their users to simply use `alr update`, `alr build`, or
`alr run` to obtain dependencies, build, or run their projects, respectively.
This is analogous to `cargo` usage.

Why not `requirements.txt`
--------------------------

Another possibility would be to go pip's way and have something similar to its
[requirements.txt](#references) file. This file contains a sequence of commands
to pass to `pip install` to recreate the build environment for a project.

The analogy for Alire would be to have a similar file with commands for `alr`.
This has a few drawbacks with no clear advantage over `cargo`'s solution:

1. It does not help packagers, unless we have complete manipulation of the 
manifest file via `alr` commands.
    1. This would be a huge undertaking, just to generate a file that is 
easily maintained by hand.
    1. We would have in practice a duplication of metadata; one copy in the
manifest file and another copy in the requirements file, in two different 
formats.
    1. Same problems with manifest formatting/comments being lost.

Implementation
==============

The proposal relies on the following changes:

- The manifest is moved up to top-level and is always named `alire.toml`.
- The manifest is under version control and manually edited.
- The manifest contains a now a single release, hence all its atomic properties
  and tables are now found at top-level in the file, which simplifies manual
and automatic edition.
- `alr with` editions to the manifest are localized and do not regenerate the
  manifest file from scratch. This is achieved by storing dependencies as an
array of tables. Arrays can be safely appended at the end of a TOML document.

Migration plan
--------------

The existing manifests in the community index can be for the most part
machine-migrated.

The local manifest bundled with sources is not used for dependencies (the
index-provided one is used instead). When retrieving a crate with `alr get`,
there will be two manifests at play: the one stored with the index and the one
provided with the sources. Both are, after retrieval, located at the same
physical location. It is clear that, to avoid misleading effects, the
one in the index must be used.

To resolve this conflict, a bundled manifest is renamed and stored for
reference, but not used. The bundled manifest is only used for crates obtained
directly from upstream providers (not through Alire), if these sources are
pinned as a directory dependency. This enables the use of sources
simultaneously for an Alire-retrieved dependency (the bundled manifest is not
used), and when using a work-in-progress version from the same repository (the
bundled manifest is used for the external dependency fulfilled via pin).

Workflow examples
=================

Several use cases are described, with the changes happening to every file
described in this proposal.

Final user downloading an Alire-aware project
---------------------------------------------

1. The user clones a repository or unpacks a source file.
    1. The sources include the manifest file.
1. The user runs `alr run` to test the application, or
1. The user runs `alr update` + `alr setenv` to edit, or
1. The user runs `alr edit` directly.

In all cases, the manifest is already at the expected location and directly
usable by `alr`, transparently to the user.

The user can contribute changes to upstream directly (even if they involve
changes in dependencies, that will be stored in the manifest).

Final user developing with Alire dependencies
---------------------------------------------

1. The user initializes a project with `alr init`.
1. The user adds dependencies with `alr with`.

The dependencies are stored in the manifest private part, but this is
transparent to the user.

Final user distributing an Alire-aware project
----------------------------------------------

1. The user initializes a project with `alr init`.
1. The user adds dependencies with `alr with`.
1. The manifest is put under version control.
1. The sources are distributed including the Alire manifest.

The user needs to know the relevance of this manifest and that it is intended
for distribution.

Maintainer publishing to the Alire community index
--------------------------------------------------

1. The maintainer has an Alire-initialized repository.
    1. The manifest file is ready after `init` or manual edition.
1. Regular work on a new release happens.
    1. Some dependency changes end stored in the manifest.
    1. It does not matter if these dependencies are stored manually (e.g. for
platform-dependent dynamic expressions) or using `alr with`; the dependencies
are loaded from the same array of tables.
1. The maintainer runs `alr publish`:
    1. All necessary information is collected from the manifest/configuration,
or generated (e.g. the origin), or requested from the user (for a first release
with missing mandatory fields in the manifest).
    1. The collected information is used for the manifest copy to be submitted
via PR.

References
==========

1. Rust Cargo `Crate.toml` file.
    - https://doc.rust-lang.org/cargo/guide/cargo-toml-vs-cargo-lock.html
1. Python `pip` `requirements.txt` file.
    - https://pip.pypa.io/en/stable/user_guide/#requirements-files

Copyright
=========

This document has been placed in the public domain.
