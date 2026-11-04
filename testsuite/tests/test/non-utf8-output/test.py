"""
A failing test whose output is not valid UTF-8 must still be reported, with
its output shown escaped (and said so), instead of crashing alr or passing on
the invalid bytes. Output that is valid UTF-8 must be left as is.
"""

import json

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match, assert_substring
from drivers.testing import write_test

init_local_crate("xxx", with_test=True)

# "ñ" as UTF-8 bytes, to not depend on the encoding of the test sources
N_TILDE = "Character'Val (16#C3#) & Character'Val (16#B1#)"

# Invalid bytes FF FE, a backslash, a valid "ñ" and an overlong NUL (C0 80),
# which a lax decoder would accept
write_test("bad_bytes",
           'GNAT.IO.Put_Line ("before:" & Character\'Val (16#FF#)'
           ' & Character\'Val (16#FE#) & " a\\b " & '
           f'{N_TILDE} & " " & Character\'Val (16#C0#)'
           ' & Character\'Val (16#80#) & ":after");\n'
           '   raise Program_Error;',
           prelude="with GNAT.IO;\n")

# Valid output with the same backslash and "ñ"
write_test("good_bytes",
           f'GNAT.IO.Put_Line ("before: a\\b " & {N_TILDE} & ":after");\n'
           '   raise Program_Error;',
           prelude="with GNAT.IO;\n")

BAD_ESCAPED = r"before:\xFF\xFE a\\b ñ \xC0\x80:after"
GOOD = r"before: a\b ñ:after"

# Console output: both failures reported, only the invalid output escaped

p = run_alr("test", quiet=False, complain_on_error=False)
assert p.status != 0, "The failing tests should fail the run"
assert_match(r".*\[ FAIL \] *\d+[smh]\d+ bad_bytes.*", p.out)
assert_match(r".*\[ FAIL \] *\d+[smh]\d+ good_bytes.*", p.out)
assert_substring("*** Test output (not valid UTF-8: invalid bytes shown as"
                 r" \xHH, backslashes as \\) ***", p.out)
assert_substring(BAD_ESCAPED, p.out)
assert_substring(GOOD, p.out)

# Structured output: the same escaped text, flagged as such

p = run_alr("--format=json", "test", complain_on_error=False)
data = json.loads(p.out)
tests = {t["name"]: t for t in data["tests"]}

assert BAD_ESCAPED in tests["bad_bytes"]["output"], data
assert tests["bad_bytes"]["output_escaped"] is True, data

assert GOOD in tests["good_bytes"]["output"], data
assert "output_escaped" not in tests["good_bytes"], data

print("SUCCESS")
