# Upgrading

## Upgrading from 1.x to 2.x

There are no special preparations to be made in advance to upgrading. However,
please check out the following information.

### Refactored features

The following features have changed in version `2.0`, with the corresponding
replacements:

- "Alire configuration" is now renamed to "Alire settings", to distinguish it
  from a crate configuration. This entails a few changes:
  - `ALR_CONFIG` environment variable is now `ALIRE_SETTINGS_DIR`.
  - `alr config` is now `alr settings`.
  - `alr -c|--config` is now `alr -s|--settings`.

- Installation of toolchains for use outside of Alire control is no longer done
  through `alr toolchain`, but through `alr install`:
  - `alr toolchain --install|--uninstall|--install-dir` no longer exist.
  - `alr toolchain --select` remains as the way to select a default toolchain
    for use by Alire.
  - `alr install` will create a standard prefix structure, with binaries found
    at `<prefix>/bin`. The default prefix location is `<user home>/.alire`.

### Obsolete large folders

Changes in default storage locations mean that the following folders, if
existing, can be safely deleted to retrieve disk space:

- `<userdir>/.cache/alire`
- `<userdir>/.config/alire/cache`

On Windows, `<userdir>` stands for `%UserProfile%`, whereas for other OSes it
stands for `$HOME`.

Also, `alr 2.x` defaults to shared builds, meaning that local workspace
caches can also be removed at

- `<crate>/alire/cache/dependencies`

### New features in 2.x

Please find all user-facing changes at our [User Changes log
file](https://github.com/alire-project/alire/blob/master/doc/user-changes.md).

## Downgrading to an earlier version

This is not a recommended nor supported operation, as changes in storage of configuration and
other resources may make a downgrade prone to failure.

If you want to have a fallback to be able to downgrade, you have two safe
options:

- Use the newer version with its own separate configuration storage. You can override
  the default location by providing a new path in the `ALIRE_SETTINGS_DIR` environment
  variable.

- Keep a backup of your current configuration at the default location, which is
  `.config/alire` within your user's home. You can restore this folder in sync with
  its older `alr` version.
