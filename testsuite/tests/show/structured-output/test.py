"""
Check the structured output of `alr show` for a complex crate
"""

import re
from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

# JSON

assert_eq(r"""{
  "authors": [
    "Bob",
    "Alice"
  ],
  "configuration": {
    "values": {
      "hello": {
        "Var1": true
      },
      "libhello": {
        "Var1": false
      }
    },
    "variables": {
      "Var1": {
        "type": "Boolean"
      },
      "Var2": {
        "default": "str",
        "type": "String"
      },
      "Var3": {
        "default": "A",
        "type": "Enum",
        "values": [
          "A",
          "B"
        ]
      },
      "Var4": {
        "default": 0,
        "type": "Integer"
      },
      "Var5": {
        "default": 0,
        "first": -1,
        "last": 1,
        "type": "Integer"
      },
      "Var6": {
        "default": 0.0,
        "first": -1.0,
        "last": 1.0,
        "type": "Real"
      },
      "Var7": {
        "default": 0.0,
        "type": "Real"
      }
    }
  },
  "depends-on": [
    {
      "libhello": "^1.0"
    }
  ],
  "description": "\"Hello, world!\" demonstration project",
  "licenses": "GPL-3.0-only OR MIT",
  "long-description": "This is an example of long description in a multi-line string.\n\nMarkdown formating `can` be used to have \"nice\" display on the website.\n",
  "maintainers": [
    "alejandro@mosteo.com",
    "bob@example.com"
  ],
  "maintainers-logins": [
    "mylogin"
  ],
  "name": "hello",
  "origin": {
    "url": "file:../../../crates/hello_1.0.1"
  },
  "tags": [
    "tag1",
    "other-tag"
  ],
  "version": "1.0.1",
  "website": "example.com"
}
""", run_alr("--format", "show", "hello").out)

# TOML

assert_match(re.escape(r'''authors = [
"Bob",
"Alice",
]
description = "\"Hello, world!\" demonstration project"
licenses = "GPL-3.0-only OR MIT"
long-description = "This is an example of long description in a multi-line string.\n\nMarkdown formating `can` be used to have \"nice\" display on the website.\n"
maintainers = [
"alejandro@mosteo.com",
"bob@example.com",
]
maintainers-logins = [
"mylogin",
]
name = "hello"
tags = [
"tag1",
"other-tag",
]
version = "1.0.1"
website = "example.com"
[configuration]
[configuration.values]
[configuration.values.hello]
Var1 = true
[configuration.values.libhello]
Var1 = false
[configuration.variables]
[configuration.variables.Var1]
type = "Boolean"
[configuration.variables.Var2]
default = "str"
type = "String"
[configuration.variables.Var3]
default = "A"
type = "Enum"
values = [
"A",
"B",
]
[configuration.variables.Var4]
default = 0
type = "Integer"
[configuration.variables.Var5]
default = 0
first = -1
last = 1
type = "Integer"
[configuration.variables.Var6]
default = 0.00000000000000E+00
first = -1.00000000000000E+00
last = 1.00000000000000E+00
type = "Real"
[configuration.variables.Var7]
default = 0.00000000000000E+00
type = "Real"
[origin]
url = "file:../../../crates/hello_1.0.1"
[[depends-on]]
libhello = "^1.0"'''), run_alr("--format=TOML", "show", "hello").out)

# YAML

assert_eq(r'''"authors":
  - "Bob"
  - "Alice"
"configuration":
  "values":
    "hello":
      "Var1": true
    "libhello":
      "Var1": false
  "variables":
    "Var1":
      "type": "Boolean"
    "Var2":
      "default": "str"
      "type": "String"
    "Var3":
      "default": "A"
      "type": "Enum"
      "values":
        - "A"
        - "B"
    "Var4":
      "default": 0
      "type": "Integer"
    "Var5":
      "default": 0
      "first": -1
      "last": 1
      "type": "Integer"
    "Var6":
      "default": 0.0
      "first": -1.0
      "last": 1.0
      "type": "Real"
    "Var7":
      "default": 0.0
      "type": "Real"
"depends-on":
  - "libhello": "^1.0"
"description": "\"Hello, world!\" demonstration project"
"licenses": "GPL-3.0-only OR MIT"
"long-description": "This is an example of long description in a multi-line string.\n\nMarkdown formating `can` be used to have \"nice\" display on the website.\n"
"maintainers":
  - "alejandro@mosteo.com"
  - "bob@example.com"
"maintainers-logins":
  - "mylogin"
"name": "hello"
"origin":
  "url": "file:../../../crates/hello_1.0.1"
"tags":
  - "tag1"
  - "other-tag"
"version": "1.0.1"
"website": "example.com"
''', run_alr("--format=YAML", "show", "hello").out)

print("SUCCESS")
