"""
When an external compiler is explicitly configured, no other compiler will be
attempted to be added to the solution. This may be counterintuitive, so we emit
an special warning in this case.
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_match


init_local_crate()

# Select the external compiler
run_alr("toolchain", "--select", "gnat_external", "gprbuild")

# Add a different version to the solution
p = run_alr("with", "gnat^9999", force=True, quiet=False)

# Check that the warning is emitted
assert_match(".*The explicitly configured external compiler gnat_external=.*"
            "cannot satisfy dependency in solution gnat\^9999",
             p.out)


print("SUCCESS")
