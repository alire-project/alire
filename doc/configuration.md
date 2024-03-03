# Configuration

`alr` provides a generic mechanism to `list`, `get`, `set` or
`unset` configuration options, either in a local or global context.

 Option names (keys) can use lowercase and uppercase alphanumeric characters
 from the Latin alphabet. Underscores and dashes can also be used except as
 the first or last character. Dot '.' is used to specify sub-categories, e.g.
 'user.name' or 'user.email'.

 Option values can be integers, floats, Booleans (true or false), or strings. The
 type detection is automatic, e.g. 10 is integer, 10.1 is float, true is
 Boolean. You can force a value to be a string by using double-quotes, e.g.
 "10.1" or "true". Extra type checking is used for built-in options (see below).

 Specific config options:

  - `--list` List configuration options
  - `--show-origin` Show origin of configuration values in `--list`
  - `--get` Print value of a configuration option
  - `--set` Set a configuration option
  - `--unset` Unset a configuration option
  - `--global` Set and Unset global configuration instead of the local one
  - `--builtins-doc` Print Markdown list of built-in configuration options

 Examples:

 - `alr config --global --set my_option option_value`

    Will set a configuration option with the key `my_option` and the string
    value `option_value` in the global configuration file.

 - `alr config --get my_option`

    Will print the value configuration option `my_option` if it is defined,
    otherwise the command fails.


## Custom configuration options

The `alr config` command allows you to set and get any combination of
configuration option `key` and `value`. You can use this feature to store your
own project related configuration, or implement tools that integrate in an
`Alire` context. However, be careful when naming custom configuration options
because `Alire` may use the same `key` in the future. We recommend using a
distinctive sub-category name, for instance: `my_project.my_config_option`.

## Built-in configuration options

The options used by `Alire` are pre-defined and documented. We call these
options `built-ins`.

A built-in option has a pre-defined type that is checked when setting or
loading a configuration file. For instance:

 - `alr config --global --set user.email "This is not an email address"`

will fail because the value tentatively assigned to `user.email` is not an
email address.

The built-ins also have a short description to document their type and usage.

## Built-ins list

You can get the list of options recognized by `alr` with `alr help config`,
including their default values and a short explanation of their effects.

## Relocating your configuration

By default, `alr` stores its global configuration at `<user
home>/.config/alire`. You can use any other location by setting in the
environment the variable `ALR_CONFIG=</absolute/path/to/config/folder>`, or by
using the global `-c` switch: `alr -c </path/to/config> <command>`.

Using a pristine default configuration can be useful to isolate the source of
errors by ensuring that a misconfiguration is not at play.

## Inspecting your configuration

These commands may help you in identifying Alire configuration and environment:
- `alr config --list` will show all configuration options in effect.
- `alr version` will print many relevant bits of information about the current
  `alr` environment.
- `alr --version` will just print the version number and exit.