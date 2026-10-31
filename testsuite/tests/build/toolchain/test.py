"""
Check `alr build --toolchain`, that reports the tools a build would use and
where they come from, without building.
"""

import json
import os

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match


def tools():
    """
    Return the structured report as a dict tool -> row. Rows give the crate of
    the release in use, so compilers are identified by their "gnat" prefix.
    """
    rows = json.loads(run_alr("--format=json", "build", "--toolchain").out)
    assert_eq(2, len(rows))
    result = {}
    for row in rows:
        tool = "gnat" if row["crate"].startswith("gnat") else row["crate"]
        assert tool not in result, f"Duplicated tool {tool} in {rows}"
        result[tool] = row
    assert_eq({"gnat", "gprbuild"}, set(result))
    return result


init_local_crate()

# Nothing configured: the fake externals in the index are detected

t = tools()
assert_eq({"crate": "gnat_external", "version": "3.3.3",
           "dependencies": "", "source": "environment"},
          t["gnat"])
assert_eq("gprbuild", t["gprbuild"]["crate"])
assert_eq("environment", t["gprbuild"]["source"])

# Plain output is the same table

assert_match(r"CRATE\s+VERSION\s+DEPENDENCIES\s+SOURCE\n"
             r"gnat_external\s+3\.3\.3\s+environment\n"
             r"gprbuild\s+.*environment\n",
             run_alr("build", "--toolchain").out)

# Tools selected globally

run_alr("toolchain", "--select", "gnat_native=8888", "gprbuild=8888")
t = tools()
assert_eq({"crate": "gnat_native", "version": "8888.0.0",
           "dependencies": "", "source": "global"},
          t["gnat"])
assert_eq({"crate": "gprbuild", "version": "8888.0.0",
           "dependencies": "", "source": "global"},
          t["gprbuild"])

# A tool selected locally

run_alr("toolchain", "--select", "--local", "gnat_native=7777")
t = tools()
assert_eq({"crate": "gnat_native", "version": "7777.0.0",
           "dependencies": "", "source": "local"},
          t["gnat"])
assert_eq("global", t["gprbuild"]["source"])

# An abstract dependency on "gnat" is fulfilled by a concrete compiler, the
# selected one in this case.

alr_with("gnat")
t = tools()
assert_eq({"crate": "gnat_native", "version": "7777.0.0",
           "dependencies": "gnat*", "source": "dependencies"},
          t["gnat"])
assert_eq("global", t["gprbuild"]["source"])
alr_with("gnat", delete=True, manual=False)

# A dependency overrides the selected compiler

alr_with("gnat_cross_1")
t = tools()
assert_eq({"crate": "gnat_cross_1", "version": "9999.0.0",
           "dependencies": "gnat_cross_1*", "source": "dependencies"},
          t["gnat"])
assert_eq("global", t["gprbuild"]["source"])

# With both an abstract and a concrete dependency, the solver keeps them
# separately as they are on different crates, but both are fulfilled by the
# same compiler and must be reported.

alr_with("gnat")
t = tools()
assert_eq({"crate": "gnat_cross_1", "version": "9999.0.0",
           "dependencies": "gnat*, gnat_cross_1*", "source": "dependencies"},
          t["gnat"])
assert_match(r".*gnat_cross_1\s+9999\.0\.0\s+gnat\*, gnat_cross_1\*\s+"
             r"dependencies\n",
             run_alr("build", "--toolchain").out)

# Other build switches or arguments are rejected

p = run_alr("build", "--toolchain", "--release", complain_on_error=False)
assert p.status != 0, "Should have failed"
assert_match(".*--toolchain cannot be combined.*", p.out)

p = run_alr("build", "--toolchain", "--", "-v", complain_on_error=False)
assert p.status != 0, "Should have failed"

# No build has happened

assert not os.path.exists("obj"), "Unexpected build"
assert not os.path.exists("bin"), "Unexpected build"

print('SUCCESS')
