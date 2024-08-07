"""
Check solving with a configured preferred compiler. When the selected compiler
fulfills the dependency, it is preferred over other available but not installed
compilers.
"""


from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import match_solution

# Select the default preferred compiler, which is the native packaged one
run_alr("toolchain", "--select")

# Verify expected compiler is selected
p = run_alr("toolchain")
assert "gnat_native   8888.0.0 Default" in p.out, \
    f"Unexpected compiler selected: {p.out}"

# Init a crate depending on gnat

init_local_crate("xxx")
alr_with("gnat*")

# Will appear in the solution as generic fulfilled by the preferred compiler
match_solution("gnat=8888.0.0 (gnat_native)", escape=True)

# Selecting another default will cause a corresponding change in the solution
run_alr("settings", "--set", "toolchain.use.gnat", "gnat_cross_2=1")
run_alr("update")
match_solution("gnat=1.0.0 (gnat_cross_2)", escape=True)

# Adding another incompatible compiler dependency should result in overriding
# the configured one
alr_with("gnat_cross_1")

# Both dependencies will appear in the solution, matching the same crate
match_solution("gnat=9999.0.0 \(gnat_cross_1\).*"
               "gnat_cross_1=9999.0.0")

print('SUCCESS')
