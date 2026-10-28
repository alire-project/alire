    AEP: 6
    Title: Hardening the community index for automatic merging of releases
    Author: Alejandro R. Mosteo <amosteo@unizar.es>
    Status: Implemented
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
changed. That check can be automated, but only if every safeguard that today
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

   *Fixed in [alire-index-checks#24](https://github.com/alire-project/alire-index-checks/pull/24): authority now comes from the latest base-branch
   release, externals included.*
1. **Unchecked kinds of change.** `changed_manifests` in `common.sh`:
   - silently skips deleted files, so a PR can remove other crates' releases;
   - does not enforce that a PR only adds new files, or that it stays under
     `index/`;
   - does not check `index/index.toml` or non-TOML files.

   *Fixed in [alire-index-checks#23](https://github.com/alire-project/alire-index-checks/pull/23) and [alire-index#2240](https://github.com/alire-project/alire-index/pull/2240): `check-changes.py` runs on every
   PR and rejects these changes.*
1. **Script injection.** `check-crate-name.yml` passes the PR title into a
   `run:` step via `${{inputs.script_arg}}`. A crafted title runs commands in
   the runner. On fork PRs the token is read-only, but the attacker can still
   forge or skip that check's result.

   *Fixed in [alire-index#2244](https://github.com/alire-project/alire-index/pull/2244): script arguments go through `env:`, and the title
   is no longer passed.*
1. **CI does not gate merging.**
   - Branch protection on `stable-1.4.0` has no required status checks.
   - Default workflow permissions are `write` and Actions can approve PRs.
   - Actions and Docker images use mutable tags (`setup-alire@latest`,
     `docker-run@v2`, ...). A retargeted tag would run new code without any
     change in the index repository.
   - `GITHUB_TOKEN` is exported to the environment where crate builds (that
     is, untrusted code) run.
   - Changes in `alire-index-checks` take effect on the index without review
     in `alire-index`.

   *Fixed , except that Docker images keep mutable tags by design:*
   - *required checks and protection of alire-index-checks, through
     `stable-*` rules in both repositories;*
   - *read-only default token, and no PR approval by Actions;*
   - *pinning where it matters
     ([alire-index#2246](https://github.com/alire-project/alire-index/pull/2246));*
   - *no token in crate builds
     ([alire-index#2245](https://github.com/alire-project/alire-index/pull/2245)).*
1. **The diff check is informational only.** `diff-release.yml` runs
   `diff-release.sh || true`, its labeling is commented out, and it treats a
   change of `url` as trivial (an attacker can point to their own fork).

   *Addressed by the auto-merge eligibility
   ([alire-index-checks#33](https://github.com/alire-project/alire-index-checks/pull/33)): a release that changes a risk-relevant
   property, its origin repository included, is never merged without review.
   The diff stays informational, for reviewers.*
1. **Weak build evidence.** A release that is unavailable, or that has missing
   dependencies, on every platform passes all build jobs without anything being
   built.

   *Fixed in [alire-index-checks#34](https://github.com/alire-project/alire-index-checks/pull/34): builds report whether each
   release was built, fetched or skipped, and a release is only merged
   automatically if built (or fetched) somewhere
   ([alire-index-checks#35](https://github.com/alire-project/alire-index-checks/pull/35)).*

`alr` loader
------------

Unknown keys are already rejected at load time, and crate names are restricted
to ASCII `[a-z0-9_]`, which rules out Unicode homoglyphs. However:

1. **`provides` hijacking.** Any release can declare `provides` for any crate
   name, including `gnat` or `gprbuild`
   (`src/alire/alire-provides.adb:46`, `src/alire/alire-index.adb:83-107`).
   The solver merges providers with the real crate, so the attacker's release
   can be picked for users who never asked for it.

   *Kept in `alr`, as crates legitimately provide others (e.g. toolchain
   variants): a release that adds or changes `provides` is never merged
   automatically ([alire-index-checks#33](https://github.com/alire-project/alire-index-checks/pull/33)), and first releases are
   always reviewed by hand. Dropped releases no longer register their aliases
   ([#2198](https://github.com/alire-project/alire/pull/2198)).*
1. **Code runs without the user's consent.** An external of kind
   `version-output` runs its command during solving and toolchain detection
   (`src/alire/alire-externals-from_output.adb:51`,
   `src/alire/alire-index.adb:171-198`). Combined with `provides`, any crate
   owner can run commands on the machines of every user who resolves the
   provided name. For example, providing `gnat` reaches nearly everyone.

   *Fixed in [#2218](https://github.com/alire-project/alire/pull/2218): only commands in an allowlist (by default,
   those of the community index) are run.*
1. **Shadowing reviewed releases.** Index versions may carry build metadata
   and leading zeros:
   - Semver equality ignores build metadata, so `crate-1.0.0+x.toml` and
     `crate-01.0.0.toml` count as the same release as `crate-1.0.0.toml`.
   - The index keeps the first one loaded and silently drops the rest
     (`src/alire/alire-index.adb:50-57`).
   - Files load in alphabetical order, so the new file wins.

   The duplicate check exists only in `alr publish`.

   *Fixed in [#2198](https://github.com/alire-project/alire/pull/2198):
   duplicates are rejected order-independently, leading zeros are
   rejected by the version parser, and dropped releases no longer register
   their `provides` aliases.*
1. **Index-wide denial of service.**
   - `Index_On_Disk.Load` catches only `Checked_Error`
     (`src/alire/alire-index_on_disk.adb:118`).
   - Other errors abort the load of the whole index for every user after
     their next update:
     - `Constraint_Error` for a local origin path that does not exist
       (`src/alire/alire-toml_index.adb:549-556`);
     - `Constraint_Error` for version numbers that overflow (now
       `Malformed_Input`, see
       [semantic_versioning#28](https://github.com/alire-project/semantic_versioning/pull/28));
     - `Storage_Error` from deep nesting in the recursive TOML parser.
   - There is no limit on file size, nesting depth, key count or string
     length.

   *A manifest that fails to load is skipped with a warning instead of
   aborting the whole load ([#2194](https://github.com/alire-project/alire/pull/2194)). Constraint errors fixed in
   [#2204](https://github.com/alire-project/alire/pull/2204) and by the
   semantic_versioning bump in [#2198](https://github.com/alire-project/alire/pull/2198). Size limited in
   [#2205](https://github.com/alire-project/alire/pull/2205) and [alire-index-checks#29](https://github.com/alire-project/alire-index-checks/pull/29), nesting in
   [alire-index-checks#30](https://github.com/alire-project/alire-index-checks/pull/30). Measured: every kind of nesting (inline
   arrays and tables, dotted keys, table headers) exhausts the stack of `alr`
   between 5,000 and 10,000 levels, which fits in 20 KB.*
   - New syntax (`[[mirror]]`) is accepted regardless of the index version,
     so older `alr` versions that read the same branch reject it.

     *By design: the current stable `alr` parses the next index format, to
     ease upgrades. Each stable `alr` reads its own index branch, so new
     syntax only reaches older clients if their branch's CI accepts it.*
1. **Argument injection.**
   - System package names are not validated. A name starting with `-` becomes
     an option to `dpkg`/`apt-cache` during detection, and to `apt-get install`
     under `sudo`
     (`src/alire/alire-origins-deployers-system-apt.adb`).
     *Fixed in
     [#2197](https://github.com/alire-project/alire/pull/2197).*
   - Git URLs are passed to `git clone` without a `--` separator
     (`src/alire/alire-vcss-git.adb`). URLs starting with `-` after the
     `vcs+` prefix are in fact not recognised as VCS URLs and are rejected,
     but `ssh://-oProxyCommand=...` and git's `ext::` transport load, and are
     only stopped by `git` and `hg` themselves (recent versions) or by the
     trusted-host check of `alr index --check`.
     *Fixed in [#2206](https://github.com/alire-project/alire/pull/2206).*
1. **Path traversal.** Nothing rejects `..` or absolute paths in `subdir`
   (`src/alire/alire-origins.adb:693-702`), `project-files`,
   `actions.directory`, `executables` or `archive-name`.

   *Fixed in [#2202](https://github.com/alire-project/alire/pull/2202) and [#2203](https://github.com/alire-project/alire/pull/2203).*
1. **Origin rules enforced only by `alr publish`.** These are all accepted at
   load time:
   - `file:` and local origins, and `ssh` URLs;
   - SVN origins with a non-immutable revision;
   - hosts outside `Community_Trusted_Sites` (`src/alire/alire.ads:58`);
   - mirrors on any host.

   `alr index --check` only checks VCS hosts, and against the user's setting
   rather than the community list (`src/alire/alire-index.adb:126-147`).

   *Addressed in Phase 2: local, `ssh` and `svn` origins are rejected by the
   index CI ([alire-index-checks#32](https://github.com/alire-project/alire-index-checks/pull/32)), and `alr index --check` warns about VCS mirrors on
   untrusted hosts ([#2213](https://github.com/alire-project/alire/pull/2213)). The setting is the community list by
   default, which is what CI uses, so a separate built-in check adds nothing.
   Archive origins and mirrors need no trusted host, as SHA-512 pins them.*
1. **Minor issues.**
   - `alr printenv` output is not escaped
     (`src/alire/alire-environment.adb:81`), and environment variable names
     are not restricted.
     *Fixed in [#2207](https://github.com/alire-project/alire/pull/2207).*
   - `long-description`, `website` and `notes` have no length limit and are
     not filtered for terminal escape sequences.
     *Fixed in [#2210](https://github.com/alire-project/alire/pull/2210).*
   - Symlinks inside the index are followed.
     *Rejected by default in
     [#2208](https://github.com/alire-project/alire/pull/2208).*
   - Pins in index manifests are parsed and silently ignored. *Kept, as they
     may make sense in private indexes; the index CI rejects new ones
     ([alire-index-checks#31](https://github.com/alire-project/alire-index-checks/pull/31)).*

Ownership and distribution
--------------------------

1. **Ownership is not anchored to the index.** `maintainers-logins` is
   declared in each release, so a new release can add, remove or replace
   maintainers. The documentation does not say which release is
   authoritative.

   *Fixed in [alire-index-checks#24](https://github.com/alire-project/alire-index-checks/pull/24): authority is the latest release
   in the base branch, as documented in [#2214](https://github.com/alire-project/alire/pull/2214). A release that
   changes `maintainers-logins` is never merged automatically
   ([alire-index-checks#33](https://github.com/alire-project/alire-index-checks/pull/33)).*
1. **No client-side defense.** Clients follow the tip of the index branch with
   `git pull`, by default every 24 hours. There is no signing, pinning, or
   delay. Anything merged reaches all users within about a day.

   *Kept by decision: users who want to lag behind can disable automatic
   index updates (`index.auto_update`), and signing is out of scope.
   Automatic merges wait one hour, during which maintainers can veto them
   ([alire-index-checks#35](https://github.com/alire-project/alire-index-checks/pull/35)).*
1. **Monorepo vault collision.** Shared-build vault folders for monorepos are
   named after the repository basename plus 8 hex digits of the commit
   (`src/alire/alire-releases.adb:160-209`), with no host or owner. A crate
   pointing to `github.com/attacker/<victim-repo-name>` with a commit whose
   first 8 hex digits match (about 2^32 attempts) can poison the folder later
   reused by the victim's crates. The vault is never re-verified once
   deployed.

   *Fixed in [#2215](https://github.com/alire-project/alire/pull/2215), with
   [diskflags#1](https://github.com/mosteo/diskflags/pull/1): folder names
   include a hash of the origin URL, and the full origin is stored in the
   checkout and verified before reuse.*

Eligibility policy for auto-merge
=================================

A PR is merged automatically only if **all** of the following hold. Otherwise
it gets an `auto-merge: manual` label, with a comment giving the reasons, and
follows the manual process.

1. It only adds release manifests, `index/xx/crate/crate-X.Y.Z.toml`, and
   modifies or deletes nothing. It may add several releases, of one or more
   crates (e.g. from a monorepo), provided each of them meets the rules below.
   The `name` and `version` in each manifest are those in its file name.
1. The crate already has at least one release in the **base branch**.
1. The version:
   - is a strict semantic version: `X.Y.Z` with no leading zeros, and valid
     pre-release and build identifiers;
   - has at most 32 characters after `X.Y.Z` (pre-release and build metadata,
     separators included);
   - does not match any existing release, nor another release in the PR,
     comparing with build metadata ignored.

   Build metadata itself is allowed: 17 community releases use it, and once
   duplicates are rejected it can no longer shadow a release.
1. The PR author is listed in `maintainers-logins` of the latest release of
   each crate **in the base branch**. The PR's own files are never used to
   decide authorship.
1. Compared with the latest release in the base branch, these properties are
   unchanged:
   - `maintainers-logins`, `provides`, `forbids`;
   - `[[actions]]`, `[environment]`, `[gpr-set-externals]`, `mirror`;
   - origin kind (VCS or archive), and origin host, owner and repository: the
     host and first two path segments of each origin URL, compared without
     regard to case, `git+`/`hg+` or `.git` (for archives, without the file
     name, which usually contains the version).

   Version, commit, hashes, dependencies, description and similar fields may
   change freely. So may `[build-switches]`, even though some compiler
   switches (e.g. GCC's `-wrapper`) run programs during the build: the
   crate's project files, which change freely with its sources, can do the
   same, and code run by the build is out of scope (see the threat model).
1. VCS origins are on a trusted host (their commits are SHA-1). There are no
   `file:`, `ssh`, `svn` or system origins.
1. The manifest is within the size and field limits.
1. All the index CI checks pass, and none is still running.
1. The release builds or tests successfully on at least one platform, or is
   fetched for binary and system crates. "Unavailable everywhere" or "missing
   dependencies everywhere" does not count as success.
1. No index maintainer vetoed it, with the `auto-merge: veto` label or a
   review requesting changes.
1. It has cooled off for one hour since it last changed: since its checks
   first ran on its head, or it was opened, left draft (`alr publish` opens
   drafts), was reopened, retargeted or force-pushed.
1. Its author has had at most 10 releases merged automatically in the last
   24 hours, counting this PR, and auto-merge has not been disabled.

Always manual:

- first releases of a crate;
- any `*-external.toml` file;
- changes to `index/index.toml` or to anything outside `index/`;
- deletions;
- modifications to existing release files, except withdrawals (below).

**Withdrawals.** A release is withdrawn today by editing its manifest to set
`available = false`. This becomes the only modification allowed to an existing
file. A PR is eligible for auto-merge when:

- its diff is exactly that change, for one or more releases;
- its author is a current maintainer;
- its checks passed, and no maintainer vetoed it.

Withdrawals only reduce risk, so it is desirable for them to be fast: they
skip the cooling-off period, the build requirement and the rate limit.

Roadmap
=======

The phases are ordered by urgency. Phase 0 fixes weaknesses that exist today,
independently of auto-merge. Phases 0 to 4 must be complete before auto-merge
is enabled. Phase 5 is defense in depth.

Phase 0: fix existing CI holes (alire-index, alire-index-checks)
----------------------------------------------------------------

*Completed. See the implementation checklist for the PRs.*

1. Take author authority from the latest release in the base branch.
   - Apply the author check to every non-first release, whatever its version.
   - Treat a release as "first" only if the crate has no release at all in
     the base branch.

   *Done in [alire-index-checks#24](https://github.com/alire-project/alire-index-checks/pull/24).*
1. Fail on deletions, and on modifications of existing release files except
   the `available = false` withdrawal described above. *Done in [alire-index-checks#23](https://github.com/alire-project/alire-index-checks/pull/23)
   and [alire-index#2240](https://github.com/alire-project/alire-index/pull/2240).*
1. Fail PRs that touch anything outside `index/`. Keep
   `index/index.toml` and `.github/` for maintainers, via CODEOWNERS.

   *The check is done in [alire-index-checks#23](https://github.com/alire-project/alire-index-checks/pull/23) and [alire-index#2240](https://github.com/alire-project/alire-index/pull/2240). CODEOWNERS for
   `index/index.toml` was added in [alire-index#2250](https://github.com/alire-project/alire-index/pull/2250); `.github/` was already covered.*
1. Remove the PR-title injection. Pass untrusted values through `env:`, never
   through `${{ }}` inside `run:`. *Done in [alire-index#2244](https://github.com/alire-project/alire-index/pull/2244).*
1. Set workflow permissions:
   - add explicit `permissions: contents: read` blocks;
   - set the default token permission to read;
   - disable Actions approving PRs.

   *Done: blocks in
   [alire-index#2245](https://github.com/alire-project/alire-index/pull/2245),
   settings changed by hand in both repositories.*
1. Do not export `GITHUB_TOKEN` to steps that run crate builds. Build
   labeling is disabled rather than moved elsewhere: fork PRs get a
   read-only token, so it only ever worked for same-repo PRs. *Done in
   [alire-index#2245](https://github.com/alire-project/alire-index/pull/2245),
   which also added explicit `permissions:` blocks.*
1. Pin actions to commit SHAs where it matters, not everywhere. Actions from
   GitHub (`actions/*`) and from alire-project or its maintainers are trusted
   like the repository itself and keep their `@vX` tags. Two rules apply:
   - third-party actions are always pinned;
   - any workflow holding a write token or secrets pins every action it uses
     (today `cleaner.yml`, later the auto-merge bot).

   Once checks gate merging, the same reasoning may justify pinning
   everything. Today all check workflows run with read-only tokens and no
   secrets, so a hijacked action could only falsify results that a human
   still reviews. *Done in
   [alire-index#2246](https://github.com/alire-project/alire-index/pull/2246),
   which also documents these rules in `.github/WORKFLOWS.md`.*
1. Make the check and build workflows required status checks in branch
   protection. *Done on the `stable-*` rule, after
   [alire-index#2247](https://github.com/alire-project/alire-index/pull/2247)
   and [alire-index#2249](https://github.com/alire-project/alire-index/pull/2249)
   gave the checks stable, unique names.*
1. Protect `alire-index-checks` with the same review requirements as
   `alire-index`. *Done as a `stable-*` rule. Pinning the checks
   repository to a commit or tag from `alire-index` was considered and
   dropped: protection already ensures that checks changes are reviewed, and
   pinning would require a lockstep `alire-index` PR for every checks
   change. `devel-*` branches are out of scope, since releases are never
   merged automatically into them.*

Phase 1: harden the `alr` index loader
--------------------------------------

*Completed. See the implementation checklist for the PRs.*

These changes protect users regardless of how a manifest was merged.

1. Add limits, configurable through settings with conservative built-in
   defaults, checked before or during parsing and raising `Checked_Error`:
   - maximum manifest size in bytes;
   - maximum TOML nesting depth;
   - maximum array length and string length.

   *Size: 64 KiB in `alr` (setting `index.max_manifest_size`,
   [#2205](https://github.com/alire-project/alire/pull/2205)), 16 KiB in the
   index CI ([alire-index-checks#29](https://github.com/alire-project/alire-index-checks/pull/29)).
   Nesting: 128 levels in the index CI only
   ([alire-index-checks#30](https://github.com/alire-project/alire-index-checks/pull/30)), as real manifests have at most 7 and
   `alr`'s stack is exhausted between 5,000 and 10,000; a limit in
   `ada-toml` was dropped (Phase 5). Array and string lengths are bounded by
   the size limit.*
1. Isolate errors per manifest. A manifest that fails to load is reported and
   skipped instead of aborting the whole index load. Convert unexpected
   exceptions during loading (`Constraint_Error`, `Storage_Error`) into
   checked errors tied to the offending file. *Done in
   [#2194](https://github.com/alire-project/alire/pull/2194) and [#2204](https://github.com/alire-project/alire/pull/2204).*
1. Versions in index manifests:
   - require strict semantic versions, with a bounded length after `X.Y.Z`,
     and report numbers that overflow as errors;
   - detect duplicate versions (equal when ignoring build metadata)
     regardless of load order, and report them as an error rather than
     silently keeping the first one.

   *Done in [#2198](https://github.com/alire-project/alire/pull/2198), with
   [semantic_versioning#28](https://github.com/alire-project/semantic_versioning/pull/28)
   and [#29](https://github.com/alire-project/semantic_versioning/pull/29).
   Build metadata is kept, since rejecting duplicates already prevents
   shadowing.*
1. Validate path fields (`subdir`, `project-files`, `executables`,
   `archive-name`, `actions.directory`): relative paths only, and no `..`
   segments. *Done in [#2202](https://github.com/alire-project/alire/pull/2202) and [#2203](https://github.com/alire-project/alire/pull/2203), allowing `..`
   that stays within the deployed origin, as monorepo crates share project
   files.*
1. Validate system package names (no leading `-`, no paths, URLs or
   patterns). Add `--` before positional arguments in package-manager and VCS
   invocations. *Package names in
   [#2197](https://github.com/alire-project/alire/pull/2197), VCS invocations
   in [#2206](https://github.com/alire-project/alire/pull/2206).*
1. Reject VCS URLs that `git`, `hg` or `svn` could take as options or
   commands: an option-like host (`ssh://-o...`), transports other than the
   supported ones (e.g. git's `ext::`), and whitespace. (A remainder after
   the `vcs+` prefix that starts with `-` is already rejected.) *Done in
   [#2206](https://github.com/alire-project/alire/pull/2206).*
1. Tie new manifest syntax (such as `[[mirror]]`) to the index version, so
   older clients never see manifests they cannot parse. *Dropped by design:
   the current stable `alr` must parse the next index format, so that two
   stable versions can coexist and upgrades are smooth.*
1. Escape values and validate variable names in `alr printenv` output.
   *Done in [#2207](https://github.com/alire-project/alire/pull/2207).*
1. Reject or strip control characters in free-text fields, and add length
   limits to `long-description`, `notes` and `website`. *Done in
   [#2210](https://github.com/alire-project/alire/pull/2210), which uncovered alr writing non-ASCII text mangled
   through `Ada.Text_IO` ([#2211](https://github.com/alire-project/alire/pull/2211)).*
1. Do not follow symlinks inside the index. *Done in [#2208](https://github.com/alire-project/alire/pull/2208):
   rejected unless the `index.allow_symlinks` setting is enabled, as links
   may be convenient in private indexes.*
1. Keep pins out of the community index. *Pins are ignored in index
   manifests, and may make sense in private ones, so the index CI rejects
   them in new manifests instead ([alire-index-checks#31](https://github.com/alire-project/alire-index-checks/pull/31)).*

Phase 2: community policy checks
--------------------------------

*Completed.*

The rules of the community index that are not about loading manifests safely
are policy, and are checked in alire-index-checks rather than in `alr`: they
take effect without an `alr` release, and do not add to `alr` options that only
CI would use.

1. Reject `file:` and local origins (meaningless in a shared index), `ssh`
   ones (they need the user's keys) and `svn` ones (their revisions do not pin
   content) in new manifests. *Done in [alire-index-checks#32](https://github.com/alire-project/alire-index-checks/pull/32). Mirrors are not
   concerned, as their content must match the origin's commit or hashes.*
1. Require trusted hosts where content is pinned by SHA-1 commits, which are
   open to collisions: VCS origins (already checked by `alr index --check`)
   and VCS mirrors. *Mirrors are fallbacks for availability, often on less
   common hosts, so `alr index --check` only warns about untrusted ones
   ([#2213](https://github.com/alire-project/alire/pull/2213)), and the new `origins.mirrors.untrusted` setting (warn,
   reject or allow) is to decide when `alr` deploys from a mirror, once it
   does. Archives, pinned by SHA-512, need no trusted host.*

Dropped from the original plan:

- `alr index --check --community`: CI already runs with the default trusted
  hosts, which are the community list, and the remaining rules are policy
  (above).
- Checking selected files only: an index must be correct as a whole
  (duplicates, `provides`, dependencies), and a full check takes seconds.
- `alr index --diff-release`, and an `alr publish` pre-check using it: the
  release diff has a single consumer, the auto-merge eligibility, so it is
  part of Phase 4, in Python like the other CI checks.

Phase 3: ownership model
------------------------

*Completed.*

Ownership is already enforced since Phase 0 (authority of the latest base
release); this phase documents it, and the governance around it.

1. Document that ownership authority is the `maintainers-logins` list of the
   latest release in the base branch. Optionally, introduce later a
   crate-level ownership file in the index, editable only via manual review.
   *Documented in `doc/policies.md` ([#2214](https://github.com/alire-project/alire/pull/2214)).*
1. Any change to `maintainers-logins` requires manual review. Adding
   maintainers could later be auto-merged if an existing maintainer approves
   the PR. *Part of the eligibility computed in Phase 4: until then every PR
   is reviewed by hand, so a separate signal would have no consumer.*
1. Document the ownership transfer procedure (abandoned crates, disputes) in
   `doc/policies.md`. *Same PR.*

Requiring two-factor authentication for maintainers was considered and
dropped: GitHub does not expose it for arbitrary users, so it could only be an
unenforceable policy.

Phase 4: auto-merge bot
-----------------------

*Completed. See the implementation checklist for the PRs.*

1. Add a dedicated workflow triggered by `workflow_run` (after the checks
   complete) or `pull_request_target`. It must:
   - never check out or execute PR content, and read the diff through the
     GitHub API only;
   - run with the minimum token scopes, ideally as a GitHub App;
   - pin every action it uses to a commit SHA, as it holds a write token.

   *`auto-merge.yml` in alire-index runs `automerge-bot.py` from
   alire-index-checks hourly instead: `workflow_run` events carry no PR for
   fork PRs, which most submissions are, and a schedule also handles the
   cooling-off period. The bot fetches each PR's head without file contents
   into a bare repository and merges it there with the base branch (`git
   merge-tree`), as GitHub's own merge commits can be hours out of date; only
   manifests are parsed. In dry-run the workflow token can only write issues;
   merging uses a GitHub App token minted in `on` mode, and every action is
   pinned ([alire-index-checks#35](https://github.com/alire-project/alire-index-checks/pull/35)).*
1. The bot recomputes eligibility itself, using the trusted `alr` and the base
   branch, rather than trusting labels or results produced by PR-controlled
   jobs. Eligibility is a Python script in alire-index-checks, which includes
   the release diff against the latest base release (risk-relevant fields, as
   listed in the eligibility policy) and the `maintainers-logins` gate.

   *`automerge.py` ([alire-index-checks#33](https://github.com/alire-project/alire-index-checks/pull/33)) parses manifests with
   Python's `tomllib` rather than `alr`; a manifest crafted so that `tomllib`
   and `alr`'s parser disagree is a known residual risk. Check results are
   trusted because an eligible PR only changes manifests, so it runs the base
   workflows; the bot requires every check (not only the required ones) to
   pass. Builds report their outcome in an `alire-build` notice
   ([alire-index-checks#34](https://github.com/alire-project/alire-index-checks/pull/34)); a crate could fake it from its own build
   output or manifest text, which only defeats a quality gate. Replayed on
   the last 3 months, 115 of 204 merged PRs (56%) would have been eligible;
   most others were first releases.*
1. Rate limits. Exceeding a limit falls back to manual review, never to
   rejection.
   - per crate (for example, a few releases per day);
   - per author;
   - global (per hour).

   *Only a per-author limit is kept: 10 releases merged automatically per 24
   hours, counting each release of a multi-crate PR. Registries limit new
   names rather than new versions: crates.io allows per user a burst of 30
   new versions (then 1 per minute) and of 5 new crates (then 1 per 10
   minutes); PyPI limits only new projects (20 per hour per user); Go has no
   publishing step; opam reviews every PR by hand. First releases here always
   go to manual review, which is stricter than all of them.*
1. A cooling-off period (for example 24–48 hours) during which the PR is
   public and labeled as pending auto-merge, and any index maintainer can veto
   it. Withdrawals skip this period.

   *One hour since the PR last changed (see the eligibility policy). A veto is the
   `auto-merge: veto` label or a request for changes by an owner, member or
   collaborator; either sends the PR to manual review.*
1. A global kill switch (a repository variable) that disables auto-merge
   immediately.

   *The `AUTO_MERGE` variable: `off` or unset, `dry-run` or `on`.*
1. Audit trail: a label and a comment stating which checks passed, and a log
   of auto-merged PRs.

   *In dry-run, each change of decision is logged in a tracking issue
   (alire-index#2299). In `on` mode the bot labels PRs (`auto-merge: pending`
   while checks run, `auto-merge: cooling` while cooling off,
   `auto-merge: manual`, `auto-merged`) and keeps one comment on each with the
   decision and its reasons. Each run also writes a table of decisions.*

Phase 5: defense in depth and incident response
-----------------------------------------------

*Completed.*

1. Restrict `provides`:
   - a crate may only provide names it owns, or names on a reviewed
     allowlist (for example, toolchain variants);
   - any new `provides` requires manual review.

   *Dropped: a change to `provides` makes a release ineligible for
   auto-merge, and first releases are always reviewed by hand, so no
   `provides` escapes manual review.*
1. Consider requiring an opt-in for `version-output` externals before their
   commands run, or limit such externals to an allowlist of commands.
   *`alr` only runs the commands in the `externals.version_output.allowlist`
   setting, by default those of the community index, where `*` matches one
   restricted argument (`pkg-config --modversion *`). The
   `externals.version_output.policy` setting (`allowlist`, `all` or `none`)
   changes this ([#2218](https://github.com/alire-project/alire/pull/2218)).*
1. Name monorepo vault folders after a hash of the full origin URL, not just
   its basename, and verify the vault folder against the origin before reuse.
   *Done in [#2215](https://github.com/alire-project/alire/pull/2215). The full origin (URL and commit) is stored
   in the checkout, and a checkout that does not match it is deployed again
   with a warning. Both parts are needed: the folder name only has 8
   characters of each hash, so a checkout could be planted under it, and
   without the URL hash, different URLs for the same commit (forks, case
   variants) would share a folder and evict each other's checkout on every
   deploy.*
1. Index distribution:
   - let clients lag behind the branch tip, by following a tag that is moved
     forward only after the cooling-off period;
   - longer term, sign the index or publish it to a transparency log (see
     TUF and Go's checksum database).

   *No change: clients keep following the branch tip, and users can disable
   automatic index updates (`index.auto_update`). Signing and transparency
   logs are out of scope for this AEP.*
1. Write down an incident runbook:
   - revert or withdraw a release (`available = false`);
   - disable auto-merge;
   - block an account;
   - notify users.

   *`INCIDENTS.md` in the index repository
   ([alire-index#2295](https://github.com/alire-project/alire-index/pull/2295)).*

Comparison with other ecosystems
================================

- **crates.io (Rust):**
  - published versions are immutable and can only be yanked;
  - owners are managed separately from the manifest;
  - publishing is rate-limited, separately for new crates and new versions;
  - uploads have a size limit;
  - "trusted publishing" via OIDC ties uploads to a CI workflow.

  Worth adopting: separate ownership, rate limits, size limits.

  Adopted: ownership decided by the index rather than by each submission (the
  latest reviewed release); a per-author limit on releases merged
  automatically, modeled on its per-user publishing limit; manifest size
  limits; withdrawal (`available = false`) as the equivalent of yanking.
- **PyPI (Python):**
  - two-factor authentication is mandatory;
  - trusted publishers;
  - projects can be quarantined when reported as malicious;
  - files have size limits.

  Worth adopting: the quarantine concept (our cooling-off period and
  withdrawal).

  Adopted: a cooling-off period with a maintainer veto before every automatic
  merge, and an incident runbook to withdraw or delete harmful releases.
  Mandatory two-factor authentication was not adopted: GitHub does not expose
  it for arbitrary users.
- **npm (JavaScript):** provenance attestations link a package to its source
  repository and build. Worth adopting: checking that the origin repository
  does not change between releases.

  Adopted: a release whose origin host, owner or repository differs from the
  latest one is never merged automatically.
- **Go modules:** a public checksum database (transparency log) ensures that
  every user sees the same content for a given version. Worth adopting, longer
  term, for index distribution.

  Adopted: nothing; signing and transparency logs are out of scope for this
  AEP.
- **opam-repository (OCaml):** the model closest to ours, with PRs to a git
  repository, linting CI and human merges. It shows that rich CI checks
  significantly reduce reviewer load even without auto-merge.

  Adopted: index CI checks that gate merging (required checks), rich enough
  to verify most releases mechanically.
- **TUF (The Update Framework):** a general framework for signing package
  repositories, used by PyPI (PEP 458), RubyGems and others. A reference
  design if the index is ever signed.

  Adopted: nothing, as signing is out of scope.

Implementation checklist
========================

PRs in merge order, each with its title, which is also its commit message.

Phase 0: CI holes
-----------------

- [x] [alire-index-checks#23](https://github.com/alire-project/alire-index-checks/pull/23) feat: stricter checks on manifest editions
- [x] [alire-index#2240](https://github.com/alire-project/alire-index/pull/2240) feat: stricter checks on manifest editions
- [x] [alire-index#2242](https://github.com/alire-project/alire-index/pull/2242) dev: python dependencies for checks
- [x] [alire-index-checks#24](https://github.com/alire-project/alire-index-checks/pull/24) dev: better authorship checks
- [x] [alire-index-checks#25](https://github.com/alire-project/alire-index-checks/pull/25) dev: restore mirror checks
- [x] [alire-index#2244](https://github.com/alire-project/alire-index/pull/2244) dev: sync with alire-index-checks
- [x] [alire-index#2245](https://github.com/alire-project/alire-index/pull/2245) dev: tighten workflow permissions
- [x] Repository settings: read-only default token, no PR approval by
  Actions (alire-index and alire-index-checks).
- [x] [alire-index#2246](https://github.com/alire-project/alire-index/pull/2246) dev: pin 3rd-party actions
- [x] [alire-index#2247](https://github.com/alire-project/alire-index/pull/2247) dev: rename workflow jobs for uniqueness
- [x] [alire-index-checks#26](https://github.com/alire-project/alire-index-checks/pull/26) dev: rename workflow jobs for uniqueness
- [x] [alire-index#2249](https://github.com/alire-project/alire-index/pull/2249) dev: stable-named summary jobs
- [x] [alire-index-checks#27](https://github.com/alire-project/alire-index-checks/pull/27) dev: stable job names for matrix builds
- [x] Branch protection: required checks on the `stable-*` rules of
  alire-index and alire-index-checks.
- [x] [alire-index#2250](https://github.com/alire-project/alire-index/pull/2250) dev: add index version to codeowners

Phase 1: `alr` loader hardening
-------------------------------

- [x] [alire#2194](https://github.com/alire-project/alire/pull/2194) feat: warn on bad index manifests rather than hard failure
- [x] [alire-index#2241](https://github.com/alire-project/alire-index/pull/2241) fix: avoid macos-latest for builds from source
- [x] [alire#2204](https://github.com/alire-project/alire/pull/2204) fix: misclassified error as exception in origins
- [x] [alire#2205](https://github.com/alire-project/alire/pull/2205) feat: limit max manifest size from index
- [x] [alire-index-checks#29](https://github.com/alire-project/alire-index-checks/pull/29) feat: check manifest size limit
- [x] [alire-index-checks#30](https://github.com/alire-project/alire-index-checks/pull/30) feat: detect excess nesting in manifest
- [x] [semantic_versioning#28](https://github.com/alire-project/semantic_versioning/pull/28) fix: reject leading zeroes according to semver spec
- [x] [semantic_versioning#29](https://github.com/alire-project/semantic_versioning/pull/29) feat: more compliance with semver spec
- [x] [alire-index#2263](https://github.com/alire-project/alire-index/pull/2263) fix: leading zeroes in manifests and versions
- [x] [alire-index#2264](https://github.com/alire-project/alire-index/pull/2264) fix: remove duplicate release uxstrings=0.3.0+...
- [x] [alire-index-checks#28](https://github.com/alire-project/alire-index-checks/pull/28) feat: check version suffix max length
- [x] [alire#2198](https://github.com/alire-project/alire/pull/2198) feat: prevent injection of manifests via version build metadata
- [x] [alire#2202](https://github.com/alire-project/alire/pull/2202) fix: ensure portable paths in index are contained
- [x] [alire#2203](https://github.com/alire-project/alire/pull/2203) fix: ensure path properties of releases are contained in origin
- [x] [alire#2197](https://github.com/alire-project/alire/pull/2197) fix: harden handling of system package names
- [x] [alire#2196](https://github.com/alire-project/alire/pull/2196) fix: pacman regex incomplete escaping
- [x] [alire#2206](https://github.com/alire-project/alire/pull/2206) fix: harden VCS URL usage
- [x] Index version gating for new syntax. *Dropped by design (see the
  roadmap).*
- [x] [alire#2207](https://github.com/alire-project/alire/pull/2207) fix: escape printenv output
- [x] [alire#2210](https://github.com/alire-project/alire/pull/2210) feat: enforce length limits on some properties
- [x] [alire#2211](https://github.com/alire-project/alire/pull/2211) fix: remove remaining uses of Text_IO producing mojibake
- [x] [den#27](https://github.com/mosteo/den/pull/27) fix: traverse symlink when root of walk
- [x] [alire#2208](https://github.com/alire-project/alire/pull/2208) feat: optionally reject symlinks in indexes
- [x] [alire-index-checks#31](https://github.com/alire-project/alire-index-checks/pull/31) feat: vet no pins in manifests

Phase 2: community policy checks
--------------------------------

- [x] [alire-index-checks#32](https://github.com/alire-project/alire-index-checks/pull/32) feat: vet origin URL kinds
- [x] [alire#2213](https://github.com/alire-project/alire/pull/2213) feat: vet mirror origins against trusted sites

Phase 3: ownership
------------------

- [x] [alire#2214](https://github.com/alire-project/alire/pull/2214) doc: document crate ownership

Phase 4: auto-merge bot
-----------------------

- [x] [alire-index-checks#33](https://github.com/alire-project/alire-index-checks/pull/33) feat: script to determine automerge elegibility
- [x] [alire-index-checks#34](https://github.com/alire-project/alire-index-checks/pull/34) feat: report outcomes in build script
- [x] [alire-index-checks#35](https://github.com/alire-project/alire-index-checks/pull/35) feat: automerge bot
- [x] [alire-index-checks#36](https://github.com/alire-project/alire-index-checks/pull/36) dev: minor tweaks to bot output
- [x] [alire-index-checks#37](https://github.com/alire-project/alire-index-checks/pull/37) dev: separate pending and cooling states
- [x] [alire-index#2300](https://github.com/alire-project/alire-index/pull/2300) feat: automerge bot workflow
- [x] Dry run: the bot's labels, `AUTO_MERGE_ISSUE=2299` and
  `AUTO_MERGE=dry-run` in alire-index.
- [x] Merging enabled: the GitHub App, its bypass in the `stable-*` rule,
  and `AUTO_MERGE=on`.

Phase 5: defense in depth
-------------------------

- [x] [diskflags#1](https://github.com/mosteo/diskflags/pull/1) feat: binary data stored in flag
- [x] [alire#2215](https://github.com/alire-project/alire/pull/2215) feat: avoid monorepo dependency folder clash
- [x] Restrict `provides`. *Dropped (see the roadmap).*
- [x] [alire#2218](https://github.com/alire-project/alire/pull/2218) feat: use allowlist for version-output externals
- [x] [alire-index#2295](https://github.com/alire-project/alire-index/pull/2295) doc: formalize response to a malicious release
- [x] TOML nesting depth in `ada-toml`. *Dropped (see the roadmap).*
- [x] Delayed or signed index distribution. *Dropped (see the roadmap).*

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
