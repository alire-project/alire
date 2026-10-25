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
