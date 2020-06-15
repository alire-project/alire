# Configuration

`alr` provides a generic mechanism to `list`, `get`, `set` or
`unset` configuration options, either in a local or global context.

 Option names (keys) can use lowercase and uppercase alphanumeric characters
 from the Latin alphabet. Underscores and dashes can also be used except as
 first or last character. Dot '.' is used to specify sub-categories, e.g.
 'user.name' or 'user.email'.

 Option values can be integers, float, Boolean (true or false) or strings. The
 type detection is automatic, e.g. 10 is integer, 10.1 is float, true is
 Boolean. You can force a value to be set a string by using double-quotes, e.g.
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

The `alr config` command allows you to set an get any combination of
configuration option `key` and `value`. You can use this feature to store you
own project related configuration, or implement tools that integrate in an
`Alire` context. However, be careful when naming custom configuration options
as `Alire` may use the same `key` in the future. We recommend using a
distinctive sub-category name, for instance: `my_project.my_config_option`.

## Built-in configuration options

The options used by `Alire` are pre-defined and documented. We call these
options `built-ins`.

A built-in option have a pre-defined type that is checked when setting or
loading a configuration file. For instance:

 - `alr config --global --set user.email "This is not an email address"`

will fail because the value tentatively assigned to `user.email` is not an
email address.

The built-ins also have a short description to document their type and usage.

## Built-ins list

Here is the list of `Alire` built-in configuration options. You can also get
this from `alr` with `alr help config`.

