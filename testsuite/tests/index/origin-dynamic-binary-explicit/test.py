"""
Test that dynamic origins require a binary flag for new index versions
"""

from drivers.alr import run_alr

# Check basic loading in pre-1.3 versions
p = run_alr('show', 'hello_world')
assert "Origin: (case OS is others => binary archive" in p.out, \
    "Unexpected output: " + p.out

# Check failure to load for new index versions
with open("my_index/index/index.toml", "w") as f:
    f.write("version = '1.3'")

# Should fail to load
p = run_alr("index", "--check", complain_on_error=False)
assert "Dynamic origins must explicitly set the `binary=true` property" in p.out,\
    f"Unexpected output: {p.out}"

# Fix the origin
with open("my_index/index/he/hello_world/hello_world-0.1.0.toml", "a") as f:
    f.write("binary = true")

# Should succeed
p = run_alr('show', 'hello_world')
assert "Origin: (case OS is others => binary archive" in p.out, \
    "Unexpected output: " + p.out


print('SUCCESS')
