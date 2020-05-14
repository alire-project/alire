# Policies

## Crate ownership

Because Alire comes late in the history of the Ada and SPARK languages we will
not apply a strict "first come, first served" policy on crates name. At least for
the first months or years, we allow ourselves a form of moderation on the
projects/crates published in the index with the following goals:

- Long term support: Owner and maintainers of a project are most likely in the
  best position to maintain the corresponding Alire crate.
- Respect the ownership of projects: Owner and maintainers of a project deserve
  to be credited for their work.
- Avoid user confusion on the name of crates: Crate names should be clear with
  regards to the project they contain. For instance, do not try to impersonate
  existing crates or projects.

To that extent we will potentially reject a crate or transfer the ownership of
a crate.

We count on the goodwill of the contributors to help us conduct this moderation
in a kind and courteous way. Do not submit a crate to the Alire index if you
are not willing to comply with this policy.

As the Alire project matures, we expect to do less moderation and potentially
remove this policy in favor of a "first come, first served" policy.

## Release immutability

A release (identified by a unique semantic version) is protected against
changes by its integrity hashes. If errors are identified post-publication, a
release could be withdrawn, or superseded by a new one (using the appropriate
major/minor/patch/build version changes), but not modified.
