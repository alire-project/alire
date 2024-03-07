# Settings

`alr` provides a generic mechanism to `list`, `get`, `set` or
`unset` settings options, either in a local or global context.

 Option names (keys) can use lowercase and uppercase alphanumeric characters
 from the Latin alphabet. Underscores and dashes can also be used except as
 the first or last character. Dot '.' is used to specify sub-categories, e.g.
 'user.name' or 'user.email'.

 Option values can be integers, floats, Booleans (true or false), or strings. The
 type detection is automatic, e.g. 10 is integer, 10.1 is float, true is
 Boolean. You can force a value to be a string by using double-quotes, e.g.
 "10.1" or "true". Extra type checking is used for built-in options (see below).

 Specific settings options:

  - `--list` List settings options
  - `--show-origin` Show origin of settings values in `--list`
  - `--get` Print value of a setting option
  - `--set` Set a setting option
  - `--unset` Unset a setting option
  - `--global `Set and Unset global settings instead of the local one
  - `--builtins-doc` Print Markdown list of built-in settings

 Examples:

 - `alr settings --global --set my_option option_value`

    Will set a setting option with the key `my_option` and the string
    value `option_value` in the global settings file.

 - `alr settings --get my_option`

    Will print the value setting option `my_option` if it is defined,
    otherwise the command fails.


## Custom settings options

The `alr settings` command allows you to set and get any combination of option
`key` and `value`. You can use this feature to store your own project related
settings, or implement tools that integrate in an `Alire` context. However, be
careful when naming custom settings options because `Alire` may use the same
`key` in the future. We recommend using a distinctive sub-category name, for
instance: `my_project.my_option`.

## Built-in settings options

The options used by `Alire` are pre-defined and documented. We call these
options `built-ins`.

A built-in option has a pre-defined type that is checked when setting or
loading. For instance:

 - `alr settings --global --set user.email "This is not an email address"`

will fail because the value tentatively assigned to `user.email` is not an
email address.

The built-ins also have a short description to document their type and usage.

## Built-ins list

You can get the list of options recognized by `alr` with `alr help settings`,
including their default values and a short explanation of their effects.

## Relocating your settings

By default, `alr` stores its global settings at `<user home>/.config/alire`.
You can use any other location by setting in the environment the variable
`ALR_CONFIG=</absolute/path/to/config/folder>`, or by using the global `-c`
switch: `alr -c </path/to/config> <command>`.

Using pristine default settings can be useful to isolate the source of errors
by ensuring that a misconfiguration is not at play.

## Inspecting your settings

These commands may help you in identifying Alire settings and environment:
- `alr settings --list` will show all settings options in effect.
- `alr version` will print many relevant bits of information about the current
  `alr` environment.
- `alr --version` will just print the version number and exit.
