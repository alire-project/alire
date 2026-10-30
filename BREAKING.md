Log of breaking changes in index or alr. Future versions are as of today and
may change.

### alr 3.0.0 + index 1.5.0

- alr: removed `ALR_CONFIG` environment variable.
- alr: removed `alr config` command.
- alr: removed remote testing in `alr test` command.
- alr: added new behavior to `alr test` and a built-in test runner.
- manifest: added `[test]` section.
- manifest: array of licenses is no longer supported (SPDX expressions allow
  multiple licenses).
- alr: setting a builtin without `--builtin` will emit a new warning.
- alr: style checks are now disabled by default in all build profiles.
- alr: quoting follows shell rules for `editor.cmd` and `alr run -a`
- manifest: `licenses` now supports SPDX v3.0.0 syntax, with the exception that license and exception identifiers are still matched case-sensitively.
- manifest: `licenses` now uses v3.24.0 of the SPDX License List.
- manifest: new `mirror` property (array of alternative origins).
- index: origin `subdir` must be a relative path with forward slashes, without
  `:`, that stays within the repository, and `archive-name` a plain file name
  without `/`, `\` or `:` (applies to any index version).
- index: `project-files`, `executables`, and `directory` in actions and tests
  must be relative paths that stay within the deployed origin (they may go up
  from a `subdir` only as far as the repository root), with forward slashes
  except in actions (applies to any index version).
- index: manifests larger than 64 KiB are rejected (configurable with the
  `index.max_manifest_size` setting; applies to any index version).
- alr: `alr printenv` quotes values so that shells take them literally: single
  quotes for sh; for PowerShell, an ASCII-only expression with other
  characters given as codes (`'a' + [char]0x0151`); `set "NAME=value"` for
  cmd.exe, which fails on values containing `"`, `%`, `!` or line breaks.
- index: symbolic links in indexes are rejected, unless enabled with the
  `index.allow_symlinks` setting (links to folders are never entered).
- manifest: dependency version sets accept word operators (`and`, `or`, `not`)
  and `¬`.
- manifest: versions with leading zeros, in X.Y.Z or in numeric pre-release
  identifiers (e.g. `01.0.0`, `2025.04.18`, `1.0.0-01`), are rejected, as
  semantic versioning forbids them. This applies to all manifests, index or
  local, and to versions in dependencies and pins.
- index: release versions must be strict semantic versions (X.Y.Z, valid
  pre-release and build identifiers), with at most 32 characters after X.Y.Z
  (pre-release and build metadata, separators included).
- index: releases of a crate whose versions differ only in build metadata are
  duplicates, and all of them are rejected (applies to any index version).
- index: the version in a manifest file name and in its contents must also
  have the same build metadata (applies to any index version; short forms
  like `1.0` for `1.0.0` remain valid).
- manifest: text fields must not contain control characters, except line
  breaks and tabs in `long-description`, `notes` and external hints, and
  `long-description`, `notes` and `website` are limited to 16384, 2048 and 512
  characters.
- alr: version commands of externals detected from their output are only run
  when listed in the `externals.version_output.allowlist` setting (by default,
  those used in the community index), unless the
  `externals.version_output.policy` setting is changed to `all`.

### We are here

### alr 2.1.0 + index 1.4.0

- index: git remotes in origins are recognized even without `git+` prefix
- manifest: remote pins accept a new `subdir` field to specify a subdirectory

### alr 2.0.0 + index 1.3.0

- index:`binary` property required in binary origins.
- index: Paths in `[environment]` must be portable (using forward slashes).
- alr: removed `alr toolchain`'s `--install, --uninstall, --install-dir`.
- alr: deprecated (but still working) `ALR_CONFIG`, `alr config`.

### alr 1.2.2 + index 1.2.1

- alr: unable to load externals containing regex special characters in the system
package name (fixed in #1545).
- alr: paths in `[environment]` are not converted to the native platform convention.
