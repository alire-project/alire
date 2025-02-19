Log of breaking changes in index or alr.

### alr 3.0.0 + index 1.4.0

- alr: removed `ALR_CONFIG` environment variable.
- alr: removed `alr config` command.

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
