"""
Test invalid crate configuration variable definitions
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import content_of


def make_manifest(var_def):
    """ Make an hello_world manifest that include the variable definitions
    passed as argument.
    """
    dst = os.path.join("my_index", "index", "he", "hello_world",
                       "hello_world-0.1.0.toml")

    src = "manifest.toml"

    # Remove existing manifest, if any
    if os.path.exists(dst):
        os.remove(dst)

    with open(dst, 'w') as f:
        f.write(content_of(src))
        f.write(var_def)


def check_error(var_def, expected):

   make_manifest(var_def)

   p = run_alr('show', 'hello_world',
               complain_on_error=False, debug=False, quiet=True)
   assert_match('ERROR:.*' + expected + '\n', p.out)

def check_ok(var_def):

   make_manifest(var_def)

   p = run_alr('show', 'hello_world',
               complain_on_error=True, debug=False, quiet=True)


os.remove(os.path.join("my_index", "index", "he", "hello_world",
                       ".gitignore"))

check_error('var1=["plop"]', 'variable definition must be a table')
check_error('var1={}', "config_variables.var1:")
check_error('var1={}', "'type' missing")
check_error('var1={type=42}', "'type' must be string")
check_error('var1={type=""}', "Invalid configuration type '',"
                               " must be \(real, integer, string,"
                               " enum or boolean\)")
check_error('var1={type="test"}', "Invalid configuration type 'test'.*")
check_error('var1={type="String", plop="test"}', "forbidden extra entries: plop")

# String
check_ok('var1={type="String"}')
check_ok('var1={type="String", default="test"}')

check_error('var1={type="String", first="test"}', "forbidden extra entries: first")
check_error('var1={type="String", last="test"}', "forbidden extra entries: last")
check_error('var1={type="String", values="test"}', "forbidden extra entries: values")

expected = "invalid default value for String"
check_error('var1={type="String", default=42}', expected)
check_error('var1={type="String", default=42.0}', expected)
check_error('var1={type="String", default=false}', expected)
check_error('var1={type="String", default=["test"]}', expected)
check_error('var1={type="String", default={plop="test"}}', expected)

# Boolean
check_ok('var1={type="Boolean"}')
check_ok('var1={type="Boolean", default=true}')
check_ok('var1={type="Boolean", default=false}')

check_error('var1={type="Boolean", first=true}', "forbidden extra entries: first")
check_error('var1={type="Boolean", last=true}', "forbidden extra entries: last")
check_error('var1={type="Boolean", values=true}', "forbidden extra entries: values")

expected = "invalid default value for Boolean"
check_error('var1={type="Boolean", default=42}', expected)
check_error('var1={type="Boolean", default=42.0}', expected)
check_error('var1={type="Boolean", default="false"}', expected)
check_error('var1={type="Boolean", default=["test"]}', expected)
check_error('var1={type="Boolean", default={plop="test"}}', expected)

# Integer
check_ok('var1={type="Integer"}')
check_ok('var1={type="Integer", default=42}')
check_ok('var1={type="Integer", default=9223372036854775807}')
check_ok('var1={type="Integer", default=-9223372036854775808}')
check_ok('var1={type="Integer", first=0}')
check_ok('var1={type="Integer", last=10}')
check_ok('var1={type="Integer", first=0, last=10}')
check_ok('var1={type="Integer", first=0, last=10, default=5}')

check_error('var1={type="Integer", values=0}', "forbidden extra entries: values")

expected = "invalid default value for Integer range .* \.\. .*"
check_error('var1={type="Integer", first=0, default=-1}', expected)
check_error('var1={type="Integer", last=0, default=1}', expected)
check_error('var1={type="Integer", first=0, last=10, default=20}', expected)
check_error('var1={type="Integer", default="42"}', expected)
check_error('var1={type="Integer", default=42.0}', expected)
check_error('var1={type="Integer", default=false}', expected)
check_error('var1={type="Integer", default=["test"]}', expected)
check_error('var1={type="Integer", default={plop="test"}}', expected)

# Real
check_ok('var1={type="Real"}')
check_ok('var1={type="Real", default=42.0}')
check_ok('var1={type="Real", default=4.9406564584124654e-324}')
check_ok('var1={type="Real", default=1.7976931348623157e+308}')

check_ok('var1={type="Real", first=0.0}')
check_ok('var1={type="Real", last=10.0}')
check_ok('var1={type="Real", first=0.0, last=10.0}')
check_ok('var1={type="Real", first=0.0, last=10.0, default=5.0}')

expected = "'first' cannot be NaN"
check_error('var1={type="Real", first=nan}', expected)
check_error('var1={type="Real", first=+nan}', expected)
check_error('var1={type="Real", first=-nan}', expected)
expected = "'last' cannot be NaN"
check_error('var1={type="Real", last=nan}', expected)
check_error('var1={type="Real", last=+nan}', expected)
check_error('var1={type="Real", last=-nan}', expected)

check_error('var1={type="Real", values=0}', "forbidden extra entries: values")

expected = "invalid default value for Real range .* \.\. .*"
check_error('var1={type="Real", first=0.0, default=-1.0}', expected)
check_error('var1={type="Real", last=0.0, default=1.0}', expected)
check_error('var1={type="Real", first=0.0, last=10.0, default=20.0}', expected)
check_error('var1={type="Real", default=nan}', expected)
check_error('var1={type="Real", default=+nan}', expected)
check_error('var1={type="Real", default=-nan}', expected)
check_error('var1={type="Real", default=inf}', expected)
check_error('var1={type="Real", default=+inf}', expected)
check_error('var1={type="Real", default=-inf}', expected)
check_error('var1={type="Real", default=42}', expected)
check_error('var1={type="Real", default="42.0"}', expected)
check_error('var1={type="Real", default=false}', expected)
check_error('var1={type="Real", default=["test"]}', expected)
check_error('var1={type="Real", default={plop="test"}}', expected)

# Enum
check_ok('var1={type="Enum", values=["A"]}')
check_ok('var1={type="Enum", values=["A"], default="A"}')
check_ok('var1={type="Enum", values=["A", "B"]}')
check_ok('var1={type="Enum", values=["A", "B"], default="B"}')

check_error('var1={type="Enum", values=["A"], first="test"}', "forbidden extra entries: first")
check_error('var1={type="Enum", values=["A"], last="test"}', "forbidden extra entries: last")
check_error('var1={type="Enum", values=[]}', "'values' must be a not empty array of strings")
check_error('var1={type="Enum"}', "missing 'values' for enumeration type")

expected = "invalid default value for Enum \(A, B\)"
check_error('var1={type="Enum", values=["A", "B"], default="C"}', expected)
check_error('var1={type="Enum", values=["A", "B"], default=42}', expected)
check_error('var1={type="Enum", values=["A", "B"], default=42.0}', expected)
check_error('var1={type="Enum", values=["A", "B"], default=false}', expected)
check_error('var1={type="Enum", values=["A", "B"], default=["test"]}', expected)
check_error('var1={type="Enum", values=["A", "B"], default={plop="test"}}', expected)

print('SUCCESS')
