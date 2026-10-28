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

   *Fixed 2026-10-18, except that Docker images keep mutable tags by design:*
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

   *Constraint errors fixed in [#2204](https://github.com/alire-project/alire/pull/2204) and by the
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
     (`src/alire/alire-origins-deployers-system-apt.adb`). *Fixed in
     [#2197](https://github.com/alire-project/alire/pull/2197).*
   - Git URLs are passed to `git clone` without a `--` separator
     (`src/alire/alire-vcss-git.adb`). URLs starting with `-` after the
     `vcs+` prefix are in fact not recognised as VCS URLs and are rejected,
     but `ssh://-oProxyCommand=...` and git's `ext::` transport load, and are
     only stopped by `git` and `hg` themselves (recent versions) or by the
     trusted-host check of `alr index --check`. *Fixed in [#2206](https://github.com/alire-project/alire/pull/2206).*
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
     are not restricted. *Fixed in [#2207](https://github.com/alire-project/alire/pull/2207).*
   - `long-description`, `website` and `notes` have no length limit and are
     not filtered for terminal escape sequences. *Fixed in [#2210](https://github.com/alire-project/alire/pull/2210).*
   - Symlinks inside the index are followed. *Rejected by default in
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
1. The manifest is within the size and field limits (Phases 1 and 2).
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
   24 hours, counting this PR (see Phase 4), and auto-merge has not been
   disabled.

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

*Completed 2026-10-18. See the implementation checklist for the PRs.*

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
   protection. *Done 2026-10-18 on the `stable-*` rule, after
   [alire-index#2247](https://github.com/alire-project/alire-index/pull/2247)
   and [alire-index#2249](https://github.com/alire-project/alire-index/pull/2249)
   gave the checks stable, unique names.*
1. Protect `alire-index-checks` with the same review requirements as
   `alire-index`. *Done 2026-10-18 as a `stable-*` rule. Pinning the checks
   repository to a commit or tag from `alire-index` was considered and
   dropped: protection already ensures that checks changes are reviewed, and
   pinning would require a lockstep `alire-index` PR for every checks
   change. `devel-*` branches are out of scope, since releases are never
   merged automatically into them.*

Phase 1: harden the `alr` index loader
--------------------------------------

*Completed 2026-10-27. See the implementation checklist for the PRs.*

These changes protect users regardless of how a manifest was merged.

1. Add limits, configurable through settings with conservative built-in
   defaults, checked before or during parsing and raising `Checked_Error`:
   - maximum manifest size in bytes;
   - maximum TOML nesting depth;
   - maximum array length and string length.

   *Size: 64 KiB in `alr` (setting `index.max_manifest_size`,
   [#2205](https://github.com/alire-project/alire/pull/2205)), 16 KiB in the index CI. Nesting: 128 levels in the
   index CI only (see the checklist); a limit in `ada-toml` is left as defense
   in depth (Phase 5). Array and string lengths are bounded by the size
   limit.*
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

*Completed 2026-10-26.*

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

*Completed 2026-10-26.*

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

   *One hour since the PR last changed (see the eligibility policy), long
   enough for maintainers to notice a submission. A veto is the
   `auto-merge: veto` label or a request for changes by an owner, member or
   collaborator; either sends the PR to manual review.*
1. A global kill switch (a repository variable) that disables auto-merge
   immediately.

   *The `AUTO_MERGE` variable: `off` or unset, `dry-run` or `on`.*
1. Audit trail: a label and a comment stating which checks passed, and a log
   of auto-merged PRs.

   *In dry-run, each change of decision is logged in a tracking issue
   (alire-index#2299). In `on` mode the bot labels PRs (`auto-merge: pending`,
   `auto-merge: manual`, `auto-merged`) and keeps one comment on each with the
   decision and its reasons. Each run also writes a table of decisions.*

Phase 5: defense in depth and incident response
-----------------------------------------------

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
   changes this ([#2218](https://github.com/alire-project/alire/pull/2218), in review).*
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
   ([alire-index#2295](https://github.com/alire-project/alire-index/pull/2295), in review).*

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

*Completed 2026-10-18.*

These require no `alr` release. Every check that alire-index runs needs a
mirror workflow in alire-index-checks, which runs it on that repository's
sample index. See "Mirror workflows" in the alire-index-checks README.

- [x] **[alire-index-checks] Strict changed-file classification.** ([#23](https://github.com/alire-project/alire-index-checks/pull/23)) A new
  script, `scripts/check-changes.py`, classifies the PR diff with
  `git diff --name-status --no-renames` against the base commit. It parses
  manifests with `tomllib`, so comparisons are semantic rather than textual.
  - It accepts:
    - added manifests at `index/<shelf>/<crate>/<crate>-<version>.toml`,
      which must be valid TOML;
    - withdrawals: a modified manifest is accepted if, once parsed, it is
      identical to the base version except that the top-level `available`
      is now `false`;
    - formatting- or comment-only modifications of `libhello` manifests,
      which are used to test the index CI.
  - It fails, listing every problem, on:
    - deletions and renames;
    - other modifications;
    - symlinks;
    - invalid TOML;
    - malformed or misplaced manifest paths;
    - files outside `index/`;
    - changes to `index/index.toml`.
  - It allows several crates in one PR, since releases from the same project
    (monorepos, runtimes, cross-compilers) are common: about 10% of recent
    PRs. Who may change each crate is left to the authorship check.
    Auto-merge eligibility can still require a single crate.
  - Replayed over the last 300 index commits, it accepts every pure release
    PR. It flags metadata fixes to existing manifests, edits to externals,
    and CI or documentation PRs, all of which need manual review.
  - `tests/test-check-changes.sh` builds throwaway repositories to test each
    case offline, and runs as the `Self-test / check-changes` check.
- [x] **[alire-index] Run `check-changes.py` on every PR.** ([#2240](https://github.com/alire-project/alire-index/pull/2240)) Add a caller of
  `check-with-script.yml` with no `paths:` filter, so that PRs touching only
  non-manifest files are classified too. A new script has no effect on the
  index until this workflow is added. Merge it after the checks PR: the
  workflow fetches the checks repo at the PR's base branch, so the script
  must already be there. This PR fails its own check (it touches
  `.github/`), as intended.
- [x] **[alire-index] Install Python for the author check.** ([#2242](https://github.com/alire-project/alire-index/pull/2242))
  Add `setup_python: true` to `check-authorship.yml`, keeping
  `setup_alr: true` so that it works with both the old and the new author
  script. Merge before the next item, otherwise the new script lacks its
  dependencies.
- [x] **[alire-index-checks] Base-branch author authority.** ([#24](https://github.com/alire-project/alire-index-checks/pull/24))
  `scripts/check-author.py` replaces the `alr`-based script.
  `check-author.sh` remains as a wrapper, so the index workflow keeps
  calling it unchanged.
  - The PR author must be in `maintainers-logins` of the latest release of
    each touched crate in the base branch. For crates
    without releases, the external manifest is used instead. The PR's own
    files are never consulted, and GitHub logins are compared
    case-insensitively.
  - "Latest" follows semver precedence, using the `semver` library (added to
    `requirements.txt`) rather than `alr`. Ordering fails closed: if the
    latest base releases tie (e.g. `2.0.0` and `2.0.0+x`, since build
    metadata is ignored) or a version is not strict semver, the check fails
    and the PR goes to manual review. Versions where `alr` would apply its
    own lenient interpretation can therefore never decide authority.
  - A crate is new only if it has no manifest at all in the base branch.
    New crates are skipped and left to manual review.
  - Every change to an existing crate is subject to the check: new
    releases, externals, withdrawals and deletions. Which kinds of change
    are acceptable at all remains the job of `check-changes.py`. Together
    they accept withdrawals by current maintainers.
  - Shared Python helpers live in `scripts/common.py`, and shared test
    helpers in `tests/lib.sh`.
  - `tests/test-check-author.sh` covers:
    - lower, intermediate and self-declared releases;
    - pre-release ordering and numeric ordering;
    - ties and non-semver versions;
    - externals, withdrawals and deletions;
    - multi-crate PRs.

    It runs as the `Self-test / check-author` check. That PR also removed the
    `Maintainer` mirror workflow and made the scripts ignore `ALL_CRATES`.
    Both mistakes are fixed by the mirror item below.
  - Replayed over the last 80 index PRs, it flags only one: a release by a
    non-maintainer (#2128), which the old check also flagged.
- [x] **[alire-index-checks] Restore mirror workflows.** ([#25](https://github.com/alire-project/alire-index-checks/pull/25))
  - `ALL_CRATES` now checks every sample manifest in both Python scripts,
    instead of doing nothing.
  - Restore the author-check mirror and add mirrors for "Check Files" and
    "Check Index".
  - Rename all mirrors after their alire-index sibling: file
    `mirror-<file>.yml`, workflow name `Mirror: <name>`.
  - Document the convention, `ALL_CRATES` and `DRY_RUN` once, in the
    checks repo README. Code and workflows point there.
  - Fix older mirrors that checked nothing on script-only PRs:
    - the crate name check now treats every crate as new under
      `ALL_CRATES`, and its mirror uses `DRY_RUN`, as the sample `libhello`
      is flagged;
    - the schema mirror's `check-changed` job now sets `ALL_CRATES`;
    - the diff-release mirror lost its `paths:` filter and its `|| true`;
    - `website.yml` now sets `ALL_CRATES`.
  - New self-test `Self-test / check-crate-name`. `tests/lib.sh` now takes a
    per-script failure pattern (`FAIL_PATTERN`).
- [x] **[alire-index] Drop `alr` from the author check and close the
  script-argument injection.** ([#2244](https://github.com/alire-project/alire-index/pull/2244))
  - Set `setup_alr: false` in `check-authorship.yml`, since the new script
    no longer needs `alr`.
  - `check-with-script.yml` now passes the script name and argument through
    `env:` and quotes them in the shell, instead of expanding `${{ }}` into
    the `run:` script. This protects every caller, including the author
    login.
  - `check-crate-name.yml` no longer passes the PR title, which the script
    ignored anyway.
- [x] **[alire-index] Least-privilege workflows and no token in crate
  builds.** ([#2245](https://github.com/alire-project/alire-index/pull/2245))
  - Every workflow gets an explicit `permissions: contents: read`, except
    `cleaner.yml`, which already declared `pull-requests: write` for its
    job.
  - Build steps no longer receive `GITHUB_TOKEN`. That token was only used
    by `gh-build-crate.sh` to label PRs ("unavailable", "missing
    dependencies"). On fork PRs, which is most submissions, the token is
    read-only, so labels only ever worked for PRs from the index repository
    itself.
  - Labeling is therefore disabled rather than moved to a separate job. The
    lines are kept commented out for the record. Without a token, the script
    logs the label it would have applied.
- [x] **[alire-index settings] Repository settings.** Set the default token
  permission to read, and disallow Actions from approving PRs. This is a
  settings change, not a PR. *Done 2026-10-17 in both alire-index and
  alire-index-checks.*
- [x] **[alire-index] Pin dependencies where it matters.** ([#2246](https://github.com/alire-project/alire-index/pull/2246))
  Following the rules in the Phase 0 roadmap:
  - pin `melusina-org/setup-macports`, the only third-party action;
  - pin `actions/stale` in `cleaner.yml`, the only workflow with a write
    token.

  Everything else keeps its `@vX` tags. Without that churn, Dependabot is not
  needed. The `gnat` Docker images keep their tags, since they are
  first-party and following new toolchains is intended. The manifest schema
  is still cloned from alire HEAD: that is about stability, not hardening.
  - Add `.github/WORKFLOWS.md` to alire-index, documenting the workflow rules
    from Phase 0: explicit permissions, untrusted values through `env:`, no
    token in crate builds, pinning, and mirrors in alire-index-checks. It is
    not named `README.md`, because GitHub would show it instead of the
    repository's main README. The workflows' existing "please read TESTING"
    headers now point to it too.
- [x] **[alire-index, alire-index-checks] Unique check names.**
  ([alire-index#2247](https://github.com/alire-project/alire-index/pull/2247),
  [alire-index-checks#26](https://github.com/alire-project/alire-index-checks/pull/26)) Prerequisites for requiring checks, since branch
  protection matches required checks by job name:
  - In alire-index:
    - give jobs unique names (`native / <os>`, `alr dev / <os>`,
      `distro / <tag>`, `macports / <os>`, and `Index` for Check Index);
    - document in `.github/WORKFLOWS.md` the naming rule, and why the
      `paths: index/**.toml` filters can stay. A required check that does
      not run blocks the PR rather than passing it. The PRs it skips change
      no manifest, so they fail `Release-only` and need an admin anyway.
      Every PR that can be auto-merged changes a manifest, so the filtered
      checks always run on it.
  - In alire-index-checks:
    - name mirror jobs `Mirror: <sibling job name>`;
    - name self-test jobs `Self-test: <script>`;
    - document the naming scheme in the README.
- [x] **[alire-index, alire-index-checks] One summary job per build
  workflow.**
  ([alire-index#2249](https://github.com/alire-project/alire-index/pull/2249),
  [alire-index-checks#27](https://github.com/alire-project/alire-index-checks/pull/27))
  - Requiring each matrix job by name would tie branch protection to runner
    labels, which change often. Each build workflow instead ends with an
    `all` job (`native / all`, `alr dev / all`, `distro / all`,
    `macports / all`, and `Mirror: native / all`). It succeeds only if the
    whole matrix did.
  - It uses `if: always()` and checks `needs.build.result` explicitly. Without
    that, a failing matrix would skip it, and a skipped required check counts
    as passed.
- [x] **[alire-index, alire-index-checks settings] Require checks to merge.**
  - alire-index: require the checks (`check / Release-only`,
    `check / Author`, `check / Crate Name`, `check / Schema`, `Index`) and
    the four build summary jobs. That is 9 fixed names, independent of the
    runners. *Done 2026-10-18 on the `stable-*` rule, so future
    stable branches inherit it. Each check is tied to the GitHub Actions
    app.*
    - Checks and builds keep their `paths:` filters, so PRs that change no
      manifest need an admin bypass. They fail `Release-only` anyway.
    - So do PRs that fail checks by design (Release-only, Crate Name). The
      reviewers who merge are admins.
    - `DIFF` stays informational.
  - alire-index-checks: currently unprotected. Require one approving review,
    every mirror and self-test check, and `Mirror: native / all` rather than
    the individual build mirror jobs. *Done 2026-10-18 as a `stable-*` rule, mirroring
    alire-index: 1 approval, 12 required checks, linear history, pushes
    restricted to admins.*
- [x] **[alire-index] Code owner for `index/index.toml`.**
  ([#2250](https://github.com/alire-project/alire-index/pull/2250)) Add
  `/index/index.toml @alire-project/alire-developers` to `.github/CODEOWNERS`.
  The `stable-*` rule already requires code-owner reviews, so changes to the
  index format version need a developer's approval. `Release-only` also
  flags such changes.

Phase 1: `alr` loader hardening
-------------------------------

- [x] **Fault isolation per manifest.** ([#2194](https://github.com/alire-project/alire/pull/2194))
  - A manifest that fails to load, for any exception, is skipped with a
    one-line warning, so the rest of the index still loads. This applies to
    both full and single-crate loads, in `Load_Manifest`.
  - `alr index --check` and `alr publish` (strict loading) still fail. They
    re-raise checked errors unchanged and wrap any other exception into a
    checked error naming the file.
  - An externals manifest registers its aliases only once it has fully
    loaded, so a skipped one leaves no orphan alias in the providers cache.
  - New test `index/skip-invalid-manifest`. It has valid crates, a crate
    with one invalid release, invalid TOML, and a `Constraint_Error`, and is
    run both with and without the providers cache.
  - 16 existing tests now check what users see, through the new
    `assert_invalid_manifest` helper: the original command warns and skips
    the manifest, and `alr index --check` reports the same detail as an
    error.
  - Side fixes in the same PR:
    - `macos-26` arm64 is dropped from the toolchain CI matrix. Since runner
      image 20261013, its SDK lists `arm64e.x1` in `libSystem.tbd`, which the
      GNAT linker rejects. alire-index's dev builds were moved off
      `macos-latest` for the same reason
      ([alire-index#2241](https://github.com/alire-project/alire-index/pull/2241)).
    - The `self_update` tests set `GH_TOKEN`, which alr reads, from CI's
      `GITHUB_TOKEN`. Their `curl` mock was bypassed on Windows by MSYS2, so
      their GitHub API calls were unauthenticated and rate-limited.
- [x] **No unexpected exceptions from loading, remaining sources.**
  ([#2204](https://github.com/alire-project/alire/pull/2204))
  - The missing local origin path in `Index_Release` raised
    `Constraint_Error`, shown by `alr index --check` as an unexpected error.
    It is now a checked error. `index/origin-filesystem-bad-path` checks that
    no unexpected error is reported.
  - Version numbers that overflow raise `Malformed_Input` once the
    semantic_versioning bump in [#2198](https://github.com/alire-project/alire/pull/2198) is merged. `Check_Index`
    already reports that as a malformed `index.toml` version.
  - Not needed:
    - the providers cache is written by `alr` from crates that loaded
      successfully, and is recreated if unreadable;
    - the remaining explicit `raise Program_Error` / `Constraint_Error` in
      the loading code guard unreachable states, and are isolated per
      manifest anyway.
- [x] **Manifest size limit.** ([#2205](https://github.com/alire-project/alire/pull/2205), [alire-index-checks#29](https://github.com/alire-project/alire-index-checks/pull/29))
  - `alr`: new setting `index.max_manifest_size`, 64 KiB by default (the
    largest community manifest is 6.8 KB, the median 0.9 KB). Larger release
    and externals manifests are rejected before parsing, with a checked
    error. Test `index/manifest-size-limit`.
  - Index CI: `check-changes.py` rejects added manifests over 16 KiB
    (`MAX_MANIFEST_SIZE`), so they go to manual review.
  - Two limits on purpose: the one in `alr` is built into released
    binaries, and a legitimate manifest over it would be skipped by every
    existing `alr`, so it is generous; the CI one keeps the index lean and
    can change in a single PR. Other registries only cap whole uploads
    (crates.io 10 MiB, PyPI 100 MB per file, `go.mod` 16 MiB) and give no
    guidance for manifests this small.
- [x] **TOML nesting depth limit.** ([alire-index-checks#30](https://github.com/alire-project/alire-index-checks/pull/30))
  - Measured with `alr index --check`: inline arrays, inline tables, dotted
    keys and table headers all exhaust the stack between 5,000 and 10,000
    levels (`STORAGE_ERROR` in the parser, or `PROGRAM_ERROR` from
    finalization while freeing the tree). That fits in the size limits.
  - Checked in the index CI rather than in `alr`, since required checks now
    gate merging. `check-changes.py` measures the depth of added manifests
    without recursion (dotted keys build deep tables that `tomllib` parses
    fine), and `common.load` turns `tomllib`'s own `RecursionError` (a few
    hundred levels) into a regular problem.
  - Limit: 128 levels of tables and arrays (`MAX_NESTING_DEPTH`). Real
    manifests have at most 4 levels plus 2 per nested `case(...)`, 7 so far;
    the rest leaves room for casing on many variables (e.g. environment
    ones).
  - `alr` itself remains exposed to deep manifests from other sources
    (private indexes, remote pins), which are out of the threat model. A
    depth limit in `ada-toml` is listed in Phase 5.
- [x] **[semantic_versioning] Strict parsing per the specification.**
  ([#28](https://github.com/alire-project/semantic_versioning/pull/28),
  [#29](https://github.com/alire-project/semantic_versioning/pull/29))
  - Strict `Parse` now enforces every rule of the specification except that
    all three numbers be present, which the new `Has_All_Points` reports
    separately. Leading zeros in `X.Y.Z` and in numeric pre-release
    identifiers are rejected. Pre-release and build identifiers must be
    non-empty and made of `[0-9A-Za-z-]`. Relaxed parsing is unchanged.
  - `Point` is widened to `0 .. 999_999_999_999_999_999`, the largest
    all-nines value within `Long_Long_Integer`. Larger numbers raise
    `Malformed_Input` instead of `Constraint_Error`. The specification sets
    no limit.
  - Default image options are restored to the output before grouped options
    ([#24](https://github.com/alire-project/semantic_versioning/pull/24)),
    which had changed them.
  - The arm64 macOS CI job moves to `macos-15`.
  - New tests: `large_numbers`, `leading_zeros`, `identifiers`,
    `all_points` and `default_output`.
- [x] **[alire-index] Index data ahead of strict versions.**
  - [#2263](https://github.com/alire-project/alire-index/pull/2263):
    `globe_3d-2025.04.18` renamed to `2025.4.18`, and seven
    `honki_tonks_zivilisationen` manifests fixed whose internal version had
    leading zeros.
  - [#2264](https://github.com/alire-project/alire-index/pull/2264): removed
    `uxstrings-0.3.0+alpha-20220727.toml`. Its version equals that of
    `uxstrings-0.3.0+alpha-20220226.toml` when build metadata is ignored, and
    it sorts after it, so it was never served.

  The whole community index then passes the strict `alr index --check` of
  the next item.
- [x] **[alire-index-checks] Version suffix length.**
  ([#28](https://github.com/alire-project/alire-index-checks/pull/28))
  `check-changes.py` rejects added manifests with more than 32 characters
  after `X.Y.Z` (`MAX_VERSION_SUFFIX_LENGTH`, the same limit as alr). This
  enforces the limit on the current index branch, before alr 3.0.
- [x] **Strict and unique index versions.**
  ([#2198](https://github.com/alire-project/alire/pull/2198))
  - A version that does not parse, in the file name or the contents, is a
    regular error naming it (e.g. "invalid version 2025.04.18 in file name:
    Number has leading zeros"), not an unexpected `Malformed_Input`.
  - **Duplicates**, for any index version. In `Load_From_Index_Internal`,
    each release manifest lists its sibling manifests and fails if any of
    them has the same version, comparing with build metadata ignored. The
    check is stateless and order-independent: every file in a duplicate group
    is skipped with a warning, or reported naming all of them under
    `index --check`.
  - **Name and contents agree**, for any index version. The version in the
    file name and in the manifest must be equal and have the same build
    metadata. Short forms such as `1.0` remain valid in the contents.
  - **Strict versions**, from index 1.5.0 on
    (`Features.Index.Strict_Release_Versions`). The version in the file name
    must pass strict `Parse` and `Has_All_Points`, and have at most 32
    characters after `X.Y.Z` (`Index.Max_Version_Suffix_Length`). The checks
    are in `Index.Error_In_Release_Version`. `X.Y.Z` is limited only by
    `Point`. With the stricter library, leading zeros are rejected in every
    manifest, index or local, whatever the index version.
  - **`provides` shadowing.** `Index.Add` registers aliases only for releases
    that were actually added, so a dropped release can no longer replace the
    reviewed one under a provided name.
  - Tests:
    - `index/duplicate-manifests`;
    - `index/version-name-mismatch`;
    - `index/provides-duplicate-release`, with two indexes of different
      priority;
    - Ada unit tests `alr_tests-release_versions` for the suffix limit and the
      delegation to the library.
  - The new testsuite helper `use_index_with_manifests` generalizes
    `use_single_manifest_index` to several manifests.
  - The submodule and the `alire.toml` pin of `semantic_versioning` move to
    the merge of #29. `BREAKING.md` records each change under alr 3.0.0 and
    index 1.5.0.
  - To do when the 1.5.0 index branch is created: an end-to-end test of
    strict versions against an index of that version.
- [x] **Path validation, origins.** ([#2202](https://github.com/alire-project/alire/pull/2202))
  - `Alire.VFS` now has one definition of a portable path,
    `Error_In_Portable_Path` (no backslashes, leading `/` or `:`, on every
    platform), which `Is_Portable` wraps. Portable paths may contain `..`,
    as pins to sibling folders do.
  - `Error_In_Contained_Path (Path, What, Within)` also requires the path not
    to go above the folder that `Within` is relative to. `subdir` must be
    contained.
  - `archive-name` must be a plain file name
    (`Origins.Error_In_Archive_Name`), as the archive is saved under it.
  - The schema gets matching patterns, except for containment, which a
    pattern cannot count.
  - Tests: `index/bad-origin-paths`, `schema-fail` cases, Ada unit tests.
- [x] **Path validation, release paths.** ([#2203](https://github.com/alire-project/alire/pull/2203))
  - `project-files`, `executables`, action and test directories, in every
    `case(...)` branch, must be contained within the deployed origin
    (`Within` is the crate's `subdir`). 17 community manifests use `..` to
    reach project files shared by the crates of a monorepo, which stays
    allowed.
  - Checked in `TOML_Index` when a release is indexed, so only index
    manifests are affected.
  - Tests: `index/bad-release-paths`, Ada unit tests.
- [x] **System package names.**
  ([#2197](https://github.com/alire-project/alire/pull/2197))
  - Validated when the index loads, in both external `origin` lists (every
    `case(...)` branch) and `system:` origins. The check is
    `Origins.Error_In_System_Package_Name`, with a precondition on
    `New_System` as a last check.
  - The rule is targeted, not a character allowlist. A crate owner can
    already make users install any repository package by naming it, so only
    names that would escape "a package from the system repositories" are
    rejected:
    - a first character that is not an ASCII letter or digit (options,
      `./` or `/` paths, apt `~`/`?` patterns, emerge `@` sets, MacPorts `+`
      variants);
    - whitespace or control characters;
    - `://` (URLs);
    - more than 256 characters.

    Everything else, e.g. `gtk+3`, `dev-libs/gmp`, `openssl@3`,
    `pkg[feature]`, `libhello~1`, is accepted. All 240 community-index names
    pass.
  - Not handled on purpose: relative local files such as `dir/x.deb`. The
    tools only take a name as a file if that file exists, and an attacker
    cannot plant one in the user's working directory without already running
    code. Matching package-file extensions was considered and dropped: it adds
    nothing real and can never be complete.
  - `--` is added before the name only for tools known to honour it: `dpkg`,
    `apt-cache`, `apt-get`, `pacman` and `rpm`. It is not used for `dnf`/`yum`
    (dnf5 unverified), `zypper`, `emerge` and other Gentoo tools, `brew` or
    `port`, where an unexpected `--` could break installation in ways CI
    cannot catch. A comment in `Alire.Origins.Deployers.System` records this
    policy.
  - Tests:
    - `index/bad-system-package-name`: bad names are skipped with a warning
      and rejected by `index --check`; unusual legal names load fine.
    - `externals/system-package-separator`: a mocked `dpkg`/`apt-cache`
      checks that `--` comes right before the name.
    - New testsuite helper `use_single_manifest_index`: one manifest per
      index, with settings prepared once and copied.
- [x] **pacman search escaping.** ([#2196](https://github.com/alire-project/alire/pull/2196)) Found while exploring the
  deployers. `pacman -Ss` takes a regex, and the name was inserted
  unescaped, so names like `gtk+3` were never detected on Arch and MSYS2.
  Now escaped with `Utils.Regex.Escape`, which also gains `{}$|`.
  - Tests: `externals/pacman-regex-name` (mocked `pacman`), and Ada unit
    tests for `Escape` in `tests_ada`.
- [x] **VCS URL arguments.** ([#2206](https://github.com/alire-project/alire/pull/2206))
  - URLs whose part after `vcs+` starts with `-` were already rejected (not
    recognised as VCS URLs). `ssh://-oProxyCommand=...`, git's `ext::`
    transport and URLs with whitespace loaded, stopped only by `git`/`hg`
    themselves (CVE-2017-1000116/7 fixes, `protocol.ext.allow`) or the
    trusted-host check.
  - `Origins.Error_In_VCS_URL`, called from `New_VCS` (so also for mirrors
    and URLs given on the command line), rejects whitespace and control
    characters, a repository that is empty or begins with `-`, transports
    other than http, https, ssh and git (local paths and `git@host:path`
    are fine), and a user or host beginning with `-`. The community index
    uses only https and http.
  - `--` before positional arguments in `git clone`, `ls-remote` and
    `remote add`, `hg clone` and `svn checkout`.
  - Tests: `index/bad-vcs-urls`, and `get/vcs-url-separator` with a mocked
    `git` checking the `--`.
- [x] **Index version gating for new syntax.** *Dropped by design (see the
  roadmap).*
- [x] **`printenv` safety.** ([#2207](https://github.com/alire-project/alire/pull/2207))
  - Values are quoted so each shell takes them literally, as alr sets them
    for its own subprocesses: single quotes for sh; for PowerShell, plain
    ASCII with other characters as `[char]` codes, since Windows PowerShell
    decodes scripts as cp1252, where bytes inside UTF-8 characters become
    the typographic quotes it also accepts (an injection, reproduced);
    `set "NAME=value"` for cmd.exe, which fails on values it cannot take
    literally (`"`, `%`, `!`, line breaks).
  - Variable names in index manifests must be `[A-Za-z_][A-Za-z0-9_]*`.
  - Found on the way: `ada-toml` rejected every character in U+0800 ..
    U+7FFF as invalid UTF-8; bumped to the upstream fix.
  - Tests: `printenv/quoting` (real `sh`), `printenv/quoting-powershell`
    (real `pwsh`, reading as UTF-8, cp1252 and cp850; skipped without it),
    `index/bad-env-var-name`, Ada unit tests.
- [x] **Text fields.** ([#2210](https://github.com/alire-project/alire/pull/2210))
  - Length limits per label, counted in characters (code points), as JSON
    Schema's `maxLength` does: `description` 72, `long-description` 16384,
    `notes` and `hint` 2048, `website` 512. The schema gets the same bounds.
  - No control characters (C0, DEL, C1), except line breaks and tabs in
    `long-description`, `notes` and `hint`, in every manifest. The schema
    gets matching patterns.
  - A `Description_String` predicate rewritten as a function call works
    around a GNAT 10/11 bug box.
  - The check uncovered that alr wrote non-ASCII text mangled into its own
    lockfiles (next item); a mangled lockfile is regenerated.
  - Tests: `index/bad-text-fields`, `schema-fail` cases, Ada unit tests.
- [x] **No mojibake: no input or output through `Ada.Text_IO`.**
  ([#2210](https://github.com/alire-project/alire/pull/2210), [#2211](https://github.com/alire-project/alire/pull/2211))
  - alr is compiled with `-gnatW8`, so `Ada.Text_IO` encodes again each
    byte of its UTF-8 strings, as if Latin-1 ("César" became "CÃ©sar", one
    more layer per lockfile rewrite, until C1 controls appeared).
  - TOML files (lockfile, index metadata, providers cache, manifests) are
    written raw (`TOML_Load.Write_File`); generated config files, captured
    command output and `.gpr` files use the raw
    `Utils.Text_Files.Write_Lines`/`Read_Lines`; console output uses
    `GNAT.IO`. Native line endings are kept.
  - `pragma Restrictions (No_Dependence => Ada.Text_IO)` in `alire.adc`
    makes the compiler reject any new use. Unused units (`Alr.Testing`,
    `src/alr/obsolete`) were removed.
  - Tests: `misc/non-ascii-lockfile` (also mangled lockfiles),
    `crate_config/non-ascii-values`, Ada unit tests for the raw lines.
- [x] **No symlinks in the index.** ([#2208](https://github.com/alire-project/alire/pull/2208), [den#27](https://github.com/mosteo/den/pull/27))
  - A symlinked manifest, crate or shelf folder is rejected (skipped with a
    warning, failing `index --check`), unless the new `index.allow_symlinks`
    setting is enabled; even then each link is reported with a warning.
    Links to folders are never entered.
  - Fixed in Den: walking from a symlinked folder failed with a predicate
    error, as `Ls` of a link returns the link itself.
  - Test: `index/symlinks` (Unix only).
- [x] **[alire-index-checks] No pins in new manifests.**
  ([alire-index-checks#31](https://github.com/alire-project/alire-index-checks/pull/31)) Pins are ignored in index manifests and may make sense in private
  indexes, so `alr` keeps accepting them. `check-changes.py` rejects any
  `pins` key in added manifests, even an empty `[[pins]]` table; the four
  existing `septum` releases with empty tables stay as they are.

Phase 2: community policy checks
--------------------------------

- [x] **[alire-index-checks] Origin kinds.** ([alire-index-checks#32](https://github.com/alire-project/alire-index-checks/pull/32))
  `check-changes.py` requires every origin URL of a new manifest, including
  those in `case(...)` tables, to be `http(s)` or `git`, optionally with a
  `git+`/`hg+` prefix (an allowlist, so local paths, `file:`, `ssh://`,
  `git@host:path` and `svn+` are all rejected). The whole community index
  complies. Mirrors are not concerned.
- [x] **VCS mirror hosts.** ([#2213](https://github.com/alire-project/alire/pull/2213)) `alr index --check`
  warns, without failing, about VCS mirrors on untrusted hosts (it only
  checked origins). New setting `origins.mirrors.untrusted` (`warn` by
  default, `reject`, `allow`), not used until `alr` deploys from mirrors.
  Test: `index/mirror-hosts`.

Phase 3: ownership
------------------

- [x] **Document ownership in `doc/policies.md`.** ([#2214](https://github.com/alire-project/alire/pull/2214))
  - "Maintainers": authority is the `maintainers-logins` of the latest
    release in the index (the external manifest for crates without
    releases), compared without regard to case; a submission's own list
    grants nothing to that submission, and changes take effect once merged.
  - "Transferring ownership": through a release with the updated list, or,
    for unreachable maintainers, abandoned crates or disputes, through an
    issue in the index repository, decided by the index maintainers.
  - Withdrawal through `available = false`, the only accepted modification
    of a published manifest, under "Release immutability".
  - `doc/publishing.md` no longer mentions "metadata modifications".
- Gating changes to `maintainers-logins` is part of the Phase 4
  eligibility script.

Phase 4: auto-merge bot
-----------------------

- [x] **[alire-index-checks] Eligibility script.**
  ([alire-index-checks#33](https://github.com/alire-project/alire-index-checks/pull/33))
  - `scripts/automerge.py` with `evaluate(author)`, used by the bot, and
    `check-automerge.py` for the command line. With `ALL_CRATES` it replays
    an index's history instead (75% of past releases would have been
    eligible compared with their predecessor).
  - Shared helpers moved to `common.py` (`latest`, `authority`,
    `is_withdrawal`, ...).
  - Tests: `tests/test-check-automerge.sh`, one case per rule.
- [x] **[alire-index-checks] Build outcome notice.**
  ([alire-index-checks#34](https://github.com/alire-project/alire-index-checks/pull/34)) `gh-build-crate.sh` reports `built`,
  `fetched` or `skipped (<reason>)` for each crate in one `alire-build`
  notice.
- [x] **[alire-index-checks] Bot.** ([alire-index-checks#35](https://github.com/alire-project/alire-index-checks/pull/35))
  - `scripts/automerge-bot.py`: decisions (skip, manual, pending, merge) in
    a pure `decide()`, from data fetched on demand; `dry-run` and `on`
    modes.
  - Robust to partial failures: the base branch is fetched again after each
    merge (or the run stops), merges count at once towards the rate limit,
    each PR's errors are isolated, and text from PRs is escaped in comments.
  - Tests: `tests/test_automerge_bot.py`, offline, with fake PR data and a
    fake GitHub client.
- [ ] **[alire-index] Workflow.** `.github/workflows/auto-merge.yml` and an
  "Auto-merge bot" section in `.github/WORKFLOWS.md`. Tested in dry-run on a
  fork.
- [ ] **[alire-index] Dry run.** Create the labels, set
  `AUTO_MERGE_ISSUE=2299` and `AUTO_MERGE=dry-run`, and compare the log with
  the manual merges for a few weeks.
- [ ] **[alire-index settings] Enable merging.** Create the GitHub App
  (contents, issues and pull requests write, checks read), allow it to bypass
  required pull requests and to push in the `stable-*` rule, set
  `AUTO_MERGE_APP_ID` and the `AUTO_MERGE_APP_KEY` secret, then
  `AUTO_MERGE=on`. Announce the change to the community.

Phase 5: defense in depth
-------------------------

- [x] **Monorepo vault naming.** ([#2215](https://github.com/alire-project/alire/pull/2215), [diskflags#1](https://github.com/mosteo/diskflags/pull/1))
  - Monorepo folders are named `<repository>_<URL hash>_<commit>`, with
    8 characters of each hash.
  - The full origin (URL and commit) is stored in the checkout as
    `alire/flags/monorepo_data.toml`. Diskflags gained flags with TOML data
    for this. A checkout whose stored origin is missing or different is
    deployed again, with a warning.
  - Tests: `monorepo/foreign-checkout` (a planted checkout),
    `monorepo/fork-same-commit` (two URLs, same commit, no eviction).
- [x] **Restrict `provides`.** *Dropped (see the roadmap).*
- [ ] **`version-output` externals.** ([#2218](https://github.com/alire-project/alire/pull/2218), in review)
  - New settings `externals.version_output.policy` (`allowlist` by default,
    `all`, `none`) and `externals.version_output.allowlist` (commands
    separated by `;`, where `*` matches one argument made of letters, digits
    and `._+-`, not starting with `-`).
  - The default allowlist has the commands of the community index:
    `curl`, `gnat`, `gprbuild`, `m4`, `make` and `odbc_config` with
    `--version`, and `pkg-config` with `--version` or `--modversion *`.
  - A command not allowed is not run, with a warning once per command, and
    its external is not detected.
  - Tests: `externals/version-output-policy`, Ada unit tests for the
    matching. The testsuite runs every command by default, as many fixtures
    use `echo`.
- [ ] **[alire-index] Incident runbook.** ([alire-index#2295](https://github.com/alire-project/alire-index/pull/2295), in review)
  `INCIDENTS.md`, linked from `REVIEWING.md`: disable auto-merge and block
  the account; withdraw a broken release, or delete a malicious one (its
  manifest disappears, and authority over the crate returns to the previous
  release); notify users of the steps they must take (refresh the index,
  `alr update`, delete deployed copies, and treat the machine as compromised
  if a malicious release was built).
- [x] **TOML nesting depth in `ada-toml`.** *Dropped: the community index CI
  already limits the depth
  ([alire-index-checks#30](https://github.com/alire-project/alire-index-checks/pull/30)).*
- [x] **Delayed or signed index distribution.** *Dropped (see the roadmap).*

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
