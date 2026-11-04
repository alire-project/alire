"""
A failing test whose output is not valid UTF-8 must still be reported, with
its output, instead of crashing alr.
"""

import json

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match
from drivers.testing import write_test

init_local_crate("xxx", with_test=True)

# Print the bytes FF FE, never valid in UTF-8, and fail

write_test("bad_bytes",
           'GNAT.IO.Put_Line ("before:" & Character\'Val (16#FF#)'
           ' & Character\'Val (16#FE#) & ":after");\n'
           '   raise Program_Error;',
           prelude="with GNAT.IO;\n")

# Console output: the failure is reported and the output shown byte for byte

p = run_alr("test", quiet=False, complain_on_error=False)
assert p.status != 0, "The failing test should fail the run"
assert_match(r".*\[ FAIL \] *\d+[smh]\d+ bad_bytes.*", p.out)
assert "*** Test output ***" in p.out, p.out
# As the output is not UTF-8, e3 gives it with non-ASCII bytes escaped
assert r"before:\xff\xfe:after" in p.out, p.out

# Structured output: the bytes are kept as Latin-1 characters in valid UTF-8

p = run_alr("--format=json", "test", complain_on_error=False)
data = json.loads(p.out)
failed = [t for t in data["tests"] if t["name"] == "bad_bytes"]
assert len(failed) == 1, data
assert failed[0]["status"] == "fail", data
assert "before:ÿþ:after" in failed[0]["output"], data

print("SUCCESS")
