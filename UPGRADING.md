## Upgrading from 1.x to 2.x

There are no special preparations to be made in advance to upgrading. However,
please check out the following information.

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
  the default location by providing a new path in the `ALR_CONFIG` environment
  variable.

- Keep a backup of your current configuration at the default location, which is
  `.config/alire` within your user's home. You can restore this folder in sync with
  its older `alr` version.