    AEP: 6
    Title: Hardening the community index for automatic merging of releases
    Author: Alejandro R. Mosteo <amosteo@unizar.es>
    Status: Draft
    Created: 2026-10-17

Abstract
========

Every release submitted to the community index
(https://github.com/alire-project/alire-index) is currently reviewed and merged
by hand. This AEP proposes to automatically merge releases of crates that
already have at least one release in the index, while keeping manual review for
new crates and for any submission that changes security-relevant properties of
a crate.

The document identifies the weaknesses in `alr` and in the index CI that a
malicious (or compromised) crate maintainer could exploit once human review is
removed. It then lays out a phased roadmap to close them before enabling
auto-merge.

Rationale
=========

Manual review is a bottleneck for maintainers and reviewers alike, and for
follow-up releases it mostly consists of checking that nothing unexpected has
changed. That check can be mechanized, but only if every safeguard that today
relies on a human reading the diff is replaced by an automated check that
cannot be bypassed.

Threat model
------------

Attacker:

- A crate maintainer who passed the initial manual review of their crate and
  later turns malicious.
- An attacker who takes over the GitHub account of a legitimate maintainer.

Assets to protect:

1. **Index and CI integrity.** The index repository, its branches, its CI
   runners and tokens, and the files of crates the attacker does not own.
2. **The ecosystem.** Users and crates that never chose to depend on the
   attacker's crate: dependency hijacking via `provides`, replacement of
   already reviewed releases, denial of service of `alr` for every user,
   spam.
3. **Crate ownership.** Silent transfer of a crate to another account, or
   removal of the legitimate maintainers.

Out of scope: harm to users who knowingly depend on the attacker's crate. Such
a crate already runs arbitrary code on their machines through builds and
`[[actions]]`, by design, and a human reviewer cannot reliably spot malicious
code in upstream sources either.

Two facts shape the design:

- A PR to the index can be written by hand. Every check performed by
  `alr publish` is therefore advisory. The **index CI is the authority** and
  must re-derive everything from the PR diff and the base branch.
- `alr` uses the same checks in CI and on users' machines. A check
  implemented in the `alr` loader protects users even if a bad manifest gets
  merged. A check implemented only in CI scripts does not.

Current state
=============

Findings from an audit of this repository (at commit 338896ab, branch
`feat/harden-meta`) and of the `alire-index` and `alire-index-checks`
repositories (branch `stable-1.4.0`, as of the creation date). References are
given to ease follow-up. They will drift as the code evolves.

Index repository and CI
-----------------------

1. **Author check bypass.** `check-author.sh` (alire-index-checks) skips the
   check when `alr show "crate<version"` finds nothing, i.e. it treats any
   release with no *lower* version as a first release. Submitting
   `crate-0.0.0.1.toml` for an existing crate skips the check. Also:
   - the "previous" release is resolved from the PR's own index, so a PR can
     add an intermediate release that it controls;
   - `*-external.toml` files are skipped entirely.
1. **Unchecked kinds of change.** `changed_manifests` in `common.sh`:
   - silently skips deleted files, so a PR can remove other crates' releases;
   - does not enforce that a PR touches a single crate, adds only new files,
     or stays under `index/`;
   - does not check `index/index.toml` or non-TOML files.
1. **Script injection.** `check-crate-name.yml` passes the PR title into a
   `run:` step via `${{inputs.script_arg}}`. A crafted title runs commands in
   the runner. On fork PRs the token is read-only, but the attacker can still
   forge or skip that check's result.
1. **CI does not gate merging.**
   - Branch protection on `stable-1.4.0` has no required status checks.
   - Default workflow permissions are `write` and Actions can approve PRs.
   - Third-party actions and Docker images use mutable tags
     (`setup-alire@latest`, `docker-run@v2`, ...).
   - `GITHUB_TOKEN` is exported to the environment where crate builds (that
     is, untrusted code) run.
   - The JSON schema is taken from `alire` HEAD, unpinned.
   - Changes in `alire-index-checks` take effect on the index without review
     in `alire-index`.
1. **The diff check is informational only.** `diff-release.yml` runs
   `diff-release.sh || true`, its labeling is commented out, and it treats a
   change of `url` as trivial (an attacker can point to their own fork).
1. **Weak build evidence.** A release that is unavailable, or that has missing
   dependencies, on every platform passes all build jobs without anything being
   built.

`alr` loader
------------

Unknown keys are already rejected at load time, and crate names are restricted
to ASCII `[a-z0-9_]`, which rules out Unicode homoglyphs. However:

1. **`provides` hijacking.** Any release can declare `provides` for any crate
   name, including `gnat` or `gprbuild`
   (`src/alire/alire-provides.adb:46`, `src/alire/alire-index.adb:83-107`).
   The solver merges providers with the real crate, so the attacker's release
   can be picked for users who never asked for it.
1. **Code runs without the user's consent.** An external of kind
   `version-output` runs its command during solving and toolchain detection
   (`src/alire/alire-externals-from_output.adb:51`,
   `src/alire/alire-index.adb:171-198`). Combined with `provides`, any crate
   owner can run commands on the machines of every user who resolves the
   provided name. For example, providing `gnat` reaches nearly everyone.
1. **Shadowing reviewed releases.** Index versions may carry build metadata
   and leading zeros:
   - Semver equality ignores build metadata, so `crate-1.0.0+x.toml` and
     `crate-01.0.0.toml` count as the same release as `crate-1.0.0.toml`.
   - The index keeps the first one loaded and silently drops the rest
     (`src/alire/alire-index.adb:50-57`).
   - Files load in alphabetical order, so the new file wins.

   The duplicate check exists only in `alr publish`.
1. **Index-wide denial of service.**
   - `Index_On_Disk.Load` catches only `Checked_Error`
     (`src/alire/alire-index_on_disk.adb:118`).
   - Other errors abort the load of the whole index for every user after
     their next update:
     - `Constraint_Error` for a local origin path that does not exist
       (`src/alire/alire-toml_index.adb:549-556`);
     - `Constraint_Error` for version numbers that overflow;
     - `Storage_Error` from deep nesting in the recursive TOML parser.
   - There is no limit on file size, nesting depth, key count or string
     length.
   - New syntax (`[[mirror]]`) is accepted regardless of the index version,
     so older `alr` versions that read the same branch reject it.
1. **Argument injection.**
   - System package names are not validated. A name starting with `-` becomes
     an option to `dpkg`/`apt-cache` during detection, and to `apt-get install`
     under `sudo`
     (`src/alire/alire-origins-deployers-system-apt.adb`).
   - Git URLs are passed to `git clone` without a `--` separator
     (`src/alire/alire-vcss-git.adb`).
1. **Path traversal.** Nothing rejects `..` or absolute paths in `subdir`
   (`src/alire/alire-origins.adb:693-702`), `project-files`,
   `actions.directory`, `executables` or `archive-name`.
1. **Origin rules enforced only by `alr publish`.** These are all accepted at
   load time:
   - `file:` and local origins, and `ssh` URLs;
   - SVN origins with a non-immutable revision;
   - hosts outside `Community_Trusted_Sites` (`src/alire/alire.ads:58`);
   - mirrors on any host.

   `alr index --check` only checks VCS hosts, and against the user's setting
   rather than the community list (`src/alire/alire-index.adb:126-147`).
1. **Minor issues.**
   - `alr printenv` output is not escaped
     (`src/alire/alire-environment.adb:81`), and environment variable names
     are not restricted.
   - `long-description`, `website` and `notes` have no length limit and are
     not filtered for terminal escape sequences.
   - Symlinks inside the index are followed.
   - Pins in index manifests are parsed and silently ignored.

Ownership and distribution
--------------------------

1. **Ownership is not anchored to the index.** `maintainers-logins` is
   declared in each release, so a new release can add, remove or replace
   maintainers. The documentation does not say which release is
   authoritative.
1. **No client-side defense.** Clients follow the tip of the index branch with
   `git pull`, by default every 24 hours. There is no signing, pinning, or
   delay. Anything merged reaches all users within about a day.
1. **Monorepo vault collision.** Shared-build vault folders for monorepos are
   named after the repository basename plus 8 hex digits of the commit
   (`src/alire/alire-releases.adb:160-209`), with no host or owner. A crate
   pointing to `github.com/attacker/<victim-repo-name>` with a commit whose
   first 8 hex digits match (about 2^32 attempts) can poison the folder later
   reused by the victim's crates. The vault is never re-verified once
   deployed.

Eligibility policy for auto-merge
=================================

A PR is merged automatically only if **all** of the following hold. Otherwise
it gets a `needs-review` label and follows the manual process.

1. It adds exactly one file, `index/xx/crate/crate-X.Y.Z.toml`, and modifies or
   deletes nothing.
1. The crate already has at least one release in the **base branch**.
1. The version:
   - has no build metadata and no leading zeros;
   - does not match any existing release, comparing with build metadata
     ignored.
1. The PR author is listed in `maintainers-logins` of the latest release of the
   crate **in the base branch**. The PR's own files are never used to decide
   authorship.
1. Compared with the latest release in the base branch, these properties are
   unchanged:
   - `maintainers-logins`, `provides`, `forbids`;
   - `[[actions]]`, `[environment]`, `[gpr-set-externals]`;
   - origin kind, and origin host, owner and repository.

   Version, commit, hashes, dependencies, description and similar fields may
   change freely.
1. Origins and mirrors are on a trusted host. There are no `file:`, `ssh`,
   `svn` or system origins.
1. The manifest is within the size and field limits (Phase 1).
1. The strict community check (Phase 2) passes, run with both the oldest and
   the newest `alr` supported on the target branch.
1. The release builds or tests successfully on at least one platform.
   "Unavailable everywhere" or "missing dependencies everywhere" does not count
   as success.
1. Rate limits are not exceeded, and auto-merge has not been disabled (Phase 4).

Always manual:

- first releases of a crate;
- any `*-external.toml` file;
- changes to `index/index.toml` or to anything outside `index/`;
- deletions;
- modifications to existing release files, except withdrawals (below).

**Withdrawals.** A release is withdrawn today by editing its manifest to set
`available = false`. This becomes the only modification allowed to an existing
file. A PR is eligible for auto-merge when:

- its diff is exactly that change;
- its author is a current maintainer.

Withdrawals only reduce risk, so it is desirable for them to be fast.

Roadmap
=======

The phases are ordered by urgency. Phase 0 fixes weaknesses that exist today,
independently of auto-merge. Phases 0 to 4 must be complete before auto-merge
is enabled. Phase 5 is defense in depth.

Phase 0: fix existing CI holes (alire-index, alire-index-checks)
----------------------------------------------------------------

1. Take author authority from the latest release in the base branch.
   - Apply the author check to every non-first release, whatever its version.
   - Treat a release as "first" only if the crate has no release at all in
     the base branch.
1. Fail on deletions, and on modifications of existing release files except
   the `available = false` withdrawal described above.
1. Fail PRs that touch anything outside `index/`, or more than one crate. Keep
   `index/index.toml` and `.github/` for maintainers, via CODEOWNERS.
1. Remove the PR-title injection. Pass untrusted values through `env:`, never
   through `${{ }}` inside `run:`.
1. Set workflow permissions:
   - add explicit `permissions: contents: read` blocks;
   - set the default token permission to read;
   - disable Actions approving PRs.
1. Do not export `GITHUB_TOKEN` to steps that run crate builds. Apply labels
   from a separate job that does not run crate builds.
1. Pin third-party actions to commit SHAs. Pin the manifest schema to the
   `alr` release used by CI.
1. Make the check and build workflows required status checks in branch
   protection.
1. Protect `alire-index-checks` with the same review requirements as
   `alire-index`. Consider pinning the checks repo to a commit or tag from
   `alire-index`.

Phase 1: harden the `alr` index loader
--------------------------------------

These changes protect users regardless of how a manifest was merged.

1. Add limits, configurable through settings with conservative built-in
   defaults, checked before or during parsing and raising `Checked_Error`:
   - maximum manifest size in bytes;
   - maximum TOML nesting depth;
   - maximum array length and string length.
1. Isolate errors per manifest. A manifest that fails to load is reported and
   skipped instead of aborting the whole index load. Convert unexpected
   exceptions during loading (`Constraint_Error`, `Storage_Error`) into
   checked errors tied to the offending file.
1. Versions in index manifests:
   - reject build metadata, leading zeros, and numbers that overflow;
   - detect duplicate versions regardless of load order, and report them as
     an error rather than silently keeping the first one.
1. Validate path fields (`subdir`, `project-files`, `executables`,
   `archive-name`, `actions.directory`): relative paths only, and no `..`
   segments.
1. Validate system package names (restricted character set, no leading `-`).
   Add `--` before positional arguments in package-manager and VCS
   invocations.
1. Reject URLs whose remainder after the `vcs+` prefix starts with `-`.
1. Tie new manifest syntax (such as `[[mirror]]`) to the index version, so
   older clients never see manifests they cannot parse.
1. Escape values and validate variable names in `alr printenv` output.
1. Reject or strip control characters in free-text fields, and add length
   limits to `long-description`, `notes` and `website`.
1. Do not follow symlinks inside the index.

Phase 2: strict community checks in `alr`
-----------------------------------------

1. Add a community-strict mode, for example `alr index --check --community
   [files...]`. It enforces at load time the rules that today exist only in
   `alr publish`:
   - origin and mirror hosts in `Community_Trusted_Sites` (the built-in list,
     not the user's setting);
   - no `file:`, `ssh` or `svn` origins;
   - valid GitHub logins;
   - no pins;
   - the text-field limits from Phase 1.
1. Add a release diff command, for example `alr index --diff-release <old>
   <new>`, that emits a machine-readable list of risk-relevant changes:
   - maintainers, `provides`/`forbids`, actions and environment;
   - origin kind or repository, and new external kinds.

   CI uses it to decide eligibility, instead of the current `grep`-based
   `diff-release.sh`.
1. Make `alr publish` run the same checks locally, so contributors learn early
   whether their submission will be auto-merged.

Phase 3: ownership model
------------------------

1. Document that ownership authority is the `maintainers-logins` list of the
   latest release in the base branch. Optionally, introduce later a
   crate-level ownership file in the index, editable only via manual review.
1. Any change to `maintainers-logins` requires manual review. Adding
   maintainers could later be auto-merged if an existing maintainer approves
   the PR.
1. Document the ownership transfer procedure (abandoned crates, disputes) in
   `doc/policies.md`.
1. Consider requiring two-factor authentication for maintainers of
   auto-merged crates. GitHub does not expose this for arbitrary users, so it
   would be policy rather than a check.

Phase 4: auto-merge bot
-----------------------

1. Add a dedicated workflow triggered by `workflow_run` (after the checks
   complete) or `pull_request_target`. It must:
   - never check out or execute PR content, and read the diff through the
     GitHub API only;
   - run with the minimum token scopes, ideally as a GitHub App.
1. The bot recomputes eligibility itself, using the trusted `alr` and the base
   branch, rather than trusting labels or results produced by PR-controlled
   jobs.
1. Rate limits. Exceeding a limit falls back to manual review, never to
   rejection.
   - per crate (for example, a few releases per day);
   - per author;
   - global (per hour).
1. A cooling-off period (for example 24–48 hours) during which the PR is
   public and labeled as pending auto-merge, and any index maintainer can veto
   it. Withdrawals skip this period.
1. A global kill switch (a repository variable) that disables auto-merge
   immediately.
1. Audit trail: a label and a comment stating which checks passed, and a log
   of auto-merged PRs.

Phase 5: defense in depth and incident response
-----------------------------------------------

1. Restrict `provides`:
   - a crate may only provide names it owns, or names on a reviewed
     allowlist (for example, toolchain variants);
   - any new `provides` requires manual review.
1. Consider requiring an opt-in for `version-output` externals before their
   commands run, or limit such externals to an allowlist of commands.
1. Name monorepo vault folders after a hash of the full origin URL, not just
   its basename, and verify the vault folder against the origin before reuse.
1. Index distribution:
   - let clients lag behind the branch tip, by following a tag that is moved
     forward only after the cooling-off period;
   - longer term, sign the index or publish it to a transparency log (see
     TUF and Go's checksum database).
1. Write down an incident runbook:
   - revert or withdraw a release (`available = false`);
   - disable auto-merge;
   - block an account;
   - notify users.

Comparison with other ecosystems
================================

- **crates.io (Rust):**
  - published versions are immutable and can only be yanked;
  - owners are managed separately from the manifest;
  - publishing is rate-limited, separately for new crates and new versions;
  - uploads have a size limit;
  - "trusted publishing" via OIDC ties uploads to a CI workflow.

  Worth adopting: separate ownership, rate limits, size limits.
- **PyPI (Python):**
  - two-factor authentication is mandatory;
  - trusted publishers;
  - projects can be quarantined when reported as malicious;
  - files have size limits.

  Worth adopting: the quarantine concept (our cooling-off period and
  withdrawal).
- **npm (JavaScript):** provenance attestations link a package to its source
  repository and build. Worth adopting: checking that the origin repository
  does not change between releases.
- **Go modules:** a public checksum database (transparency log) ensures that
  every user sees the same content for a given version. Worth adopting, longer
  term, for index distribution.
- **opam-repository (OCaml):** the model closest to ours, with PRs to a git
  repository, linting CI and human merges. It shows that rich CI checks
  significantly reduce reviewer load even without auto-merge.
- **TUF (The Update Framework):** a general framework for signing package
  repositories, used by PyPI (PEP 458), RubyGems and others. A reference
  design if the index is ever signed.

Notes on the initial proposals
==============================

- **Maximum manifest size.** Necessary, but not sufficient. A small file can
  still exhaust the parser through deep nesting, so the limit must be paired
  with a nesting-depth limit, and loading must be fault-isolated per manifest
  (Phase 1).
- **Bounded fields.** A size cap bounds memory use, but per-field limits are
  still valuable for display (`long-description`, `notes`) and for filtering
  control characters and terminal escape sequences.
- **Anti-spam.** GitHub's own limits protect GitHub, not the index. Rate limits
  per crate, per author and global, with fallback to manual review, are
  needed (Phase 4).
- **Beyond these.** The most critical issues found are not about size:
  - authorship checks that can be bypassed;
  - dependency hijacking through `provides`;
  - automatic command execution through externals;
  - shadowing of reviewed releases via build metadata;
  - a CI that does not gate merges.

Implementation checklist
========================

PRs in the intended merge order. Each item is meant to be one small PR. The
target repository is given in brackets.

Conventions for `alr` PRs:

- Each PR adds a regression test under `testsuite/tests/` (usually
  `testsuite/tests/index/<name>/`, with a crafted index as fixture) that fails
  before the change and passes after it.
- Each PR updates `doc/catalog-format-spec.md`, the manifest schema and
  `doc/user-changes.md` when it changes what manifests may contain.
- New loader restrictions apply to the community index. Where they could break
  private indexes, they are behind a setting that is enabled by default for
  the community index.

Phase 0: CI holes
-----------------

These require no `alr` release.

- [ ] **[alire-index-checks] Strict changed-file classification.**
  - Use `git diff --name-status` against the base branch.
  - Fail on deleted, renamed or modified manifests, on files outside
    `index/`, on changes to `index/index.toml`, and on more than one crate per
    PR.
  - Exception: a diff to an existing release that only sets
    `available = false`.
  - Add self-test fixtures for each case.
- [ ] **[alire-index-checks] Base-branch author authority.**
  - Resolve the latest release of the crate from the base branch, not the
    PR's index.
  - "First release" means no release at all in the base branch.
  - Stop skipping `*-external.toml`.
  - Add fixtures for a lower version, an intermediate version, and an
    external.
- [ ] **[alire-index] Remove the PR-title injection** in
  `check-crate-name.yml`. Drop the unused argument, or pass it through `env:`.
- [ ] **[alire-index] Least-privilege workflows.** Add explicit
  `permissions:` blocks to every workflow.
- [ ] **[alire-index settings] Repository settings.** Set the default token
  permission to read, and disallow Actions from approving PRs. This is a
  settings change, not a PR.
- [ ] **[alire-index] Keep tokens away from crate builds.** Remove
  `GITHUB_TOKEN` from build steps, and move labeling to a separate job that
  consumes build results as artifacts.
- [ ] **[alire-index] Pin dependencies.** Pin third-party actions to commit
  SHAs and the manifest schema to the `alr` release used in CI. Add
  Dependabot for action updates.
- [ ] **[alire-index settings] Require checks to merge.** Make the check and
  build workflows required status checks. Give `alire-index-checks` the same
  branch protection as `alire-index`. This is a settings change, not a PR.

Phase 1: `alr` loader hardening
-------------------------------

- [ ] **Fault isolation per manifest.**
  - A manifest that fails to load is reported and skipped, so the rest of the
    index still loads.
  - `alr index --check` still fails on it.
  - Test: an index with one broken and one valid crate. The valid crate is
    still usable, and `--check` fails.
- [ ] **No unexpected exceptions from loading.** Convert `Constraint_Error`
  and `Storage_Error` raised while loading a manifest into checked errors
  naming the file. This includes the missing local origin path in
  `alire-toml_index.adb` and version numbers that overflow. Add a test for
  each trigger.
- [ ] **Manifest size limit.** Add a setting (for example
  `index.max_manifest_size`) with a conservative default, checked before
  parsing. Test with an oversized manifest.
- [ ] **TOML nesting depth limit.** Add a depth limit to the parser, either
  upstream in `ada-toml` or as a pre-parse scan. Test with a deeply nested
  manifest.
- [ ] **Canonical index versions.** Reject build metadata and leading zeros
  in versions of index manifests (not of workspace manifests). Test
  `crate-1.0.0+x.toml` and `crate-01.0.0.toml`.
- [ ] **Duplicate versions.** Report duplicate versions as an error regardless
  of load order, instead of silently keeping the first one. Test with
  duplicates whose files sort before and after the original.
- [ ] **Path validation helper.** Add one helper that rejects absolute paths
  and `..` segments, and apply it to `subdir` and `archive-name`. Test each
  field.
- [ ] **Path validation, remaining fields.** Apply the same helper to
  `project-files`, `executables` and `actions.directory`. Test each field.
- [ ] **System package names.**
  - Restrict package names to a safe character set with no leading `-`.
  - Add `--` before package names in every system deployer that supports it.
  - Test with a `-o...` package name.
- [ ] **VCS URL arguments.** Reject VCS URLs whose part after the `vcs+`
  prefix starts with `-`, and add `--` before positional arguments in
  `git`/`hg` invocations. Test with a `git+--upload-pack=...` URL.
- [ ] **Index version gating for new syntax.**
  - Bump the index format version.
  - Accept `[[mirror]]` only from that version on.
  - Document the policy that new manifest syntax requires an index version
    bump.
  - Test with mirrors in an old-version index.
- [ ] **`printenv` safety.**
  - Escape values for each output shell.
  - Validate environment variable names (`[A-Za-z_][A-Za-z0-9_]*`) at load
    time.
  - Test with quotes and `$(...)` in values, and an invalid name.
- [ ] **Text fields.** Add length limits to `long-description`, `notes` and
  `website`, and reject control characters (except newline and tab where
  appropriate) in all free-text fields. Test with an over-long field and an
  embedded escape sequence.
- [ ] **No symlinks in the index.** Reject symlinked files and directories
  inside an index. Test with a symlinked manifest.
- [ ] **Pins in index manifests.** Pins are currently ignored silently; make
  them an error. Test.

Phase 2: community checks in `alr`
----------------------------------

- [ ] **Extract publish checks into a shared package.**
  - Move the trusted-host, local-origin and GitHub-login checks out of
    `alire-publish.adb` into a reusable package, for example
    `Alire.Index.Checks`.
  - No behavior change. Existing `testsuite/tests/publish/` tests must pass.
- [ ] **`alr index --check --community`.**
  - Apply the shared checks to every origin and mirror, using the built-in
    `Community_Trusted_Sites` list.
  - Add the rules for no `file:`, `ssh` or `svn` origins.
  - Test one fixture per rule.
- [ ] **Check selected files only.** Allow `alr index --check` to be limited
  to the given manifest files, for speed in CI. Test.
- [ ] **`alr index --diff-release <old> <new>`.**
  - Emit a machine-readable (TOML or JSON) list of the risk-relevant changes
    defined in the eligibility policy.
  - Test one fixture pair per category, plus a "no risky change" pair.
- [ ] **`alr publish` pre-check.** Run the community check and the release
  diff against the latest indexed release, and tell the user whether the
  submission will need manual review. Extend the publish tests.
- [ ] **[alire-index-checks] Use the new `alr` commands.**
  - Replace `diff-release.sh` with `alr index --diff-release`.
  - Add `--community` to the index check.
  - Apply an informational `eligible` / `needs-review` label.

Phase 3: ownership
------------------

- [ ] **Document ownership in `doc/policies.md`.** Cover the authority of the
  latest base-branch release, ownership transfer, and withdrawal via
  `available = false`.
- [ ] **[alire-index-checks] Gate maintainer changes.** Any change to
  `maintainers-logins` relative to the latest base-branch release sets
  `needs-review`. Add fixtures.

Phase 4: auto-merge bot
-----------------------

- [ ] **[alire-index] Bot in dry-run mode.**
  - A `workflow_run` workflow with minimal permissions that recomputes
    eligibility from the API diff and the base branch.
  - It only comments and labels; it does not merge.
  - Run it for a few weeks and compare its decisions with manual reviews.
- [ ] **[alire-index] Rate limits.** Enforce per-crate, per-author and global
  limits, falling back to `needs-review`.
- [ ] **[alire-index] Cooling-off period, veto label and kill switch.** The
  kill switch is a repository variable.
- [ ] **[alire-index] Enable merging.** Merge through a dedicated GitHub App
  with a narrowly scoped token, and post an audit comment on each merged PR.
  Announce the change to the community.

Phase 5: defense in depth
-------------------------

- [ ] **Monorepo vault naming.** Derive monorepo vault folder names from a
  hash of the full origin URL. Test two monorepos with the same basename.
- [ ] **Restrict `provides`.** In the community check, a crate may provide
  only names it owns or names on an allowlist. Any new `provides` sets
  `needs-review`. Add one test per case.
- [ ] **`version-output` externals.** Require user opt-in, or an allowlist of
  commands, before running `version-output` detectors. Test.
- [ ] **Incident runbook.** Document how to withdraw a release, disable
  auto-merge, block an account and notify users.
- [ ] **Delayed or signed index distribution.** Write a follow-up AEP for
  clients following a lagging tag, and later for index signing.

References
==========

1. Alire index policies: `doc/policies.md`.
1. Publishing documentation: `doc/publishing.md`.
1. Catalog format specification: `doc/catalog-format-spec.md`.
1. AEP-5, origin mirrors: `doc/AEPs/aep-0005-mirrors.md`.
1. GitHub Actions security hardening:
   https://docs.github.com/en/actions/security-for-github-actions/security-guides/security-hardening-for-github-actions
1. The Update Framework: https://theupdateframework.io/
1. Go checksum database: https://go.dev/ref/mod#checksum-database
1. crates.io policies: https://crates.io/policies
1. PyPI trusted publishers: https://docs.pypi.org/trusted-publishers/

Copyright
=========

This document has been placed in the public domain.
