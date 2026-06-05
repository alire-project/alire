"""
Intentionally malformed manifests must be rejected by the manifest JSON
Schema (schemas/manifest-schema.yaml), each for its intended reason.
"""

from drivers.schema import catalog_validator, manifest_errors

validator = catalog_validator()

# A minimal valid release manifest, extended with one defect per case.
BASE = 'name = "crate"\nversion = "1.0.0"\ndescription = "d"\n'

# (label, manifest, substring expected somewhere in the error messages)
CASES = [
    ("unknown top-level key",
     BASE + 'bogus = 1\n',
     "Additional properties"),
    ("missing mandatory version",
     'name = "crate"\ndescription = "d"\n',
     "'version' is a required property"),
    ("crate name too short",
     'name = "ab"\nversion = "1.0.0"\ndescription = "d"\n',
     "does not match"),
    ("crate name with whitespace",
     'name = "bad name"\nversion = "1.0.0"\ndescription = "d"\n',
     "does not match"),
    ("invalid configuration variable type",
     BASE + '[configuration.variables]\nV = { type = "Floaty" }\n',
     "does not match"),
    ("undefined case() variable",
     BASE + '[available."case(planet)"]\nmars = true\n',
     "is not valid"),
    ("unknown build switch category",
     BASE + '[build-switches]\nrelease.frobnicate = "Yes"\n',
     "does not match"),
    ("invalid action type",
     BASE + '[[actions]]\ntype = "post-lunch"\ncommand = ["x"]\n',
     "is not valid"),
    ("malformed origin hash",
     BASE + '[origin]\nurl = "u"\nhashes = ["md5:abc"]\n',
     "does not match"),
    ("origin must be a table, not a string",
     BASE + 'origin = "https://example.org/a.tgz"\n',
     "is not of type 'object'"),
]

problems = []
for label, manifest, expected in CASES:
    errors = manifest_errors(validator, manifest)
    if not errors:
        problems.append(f"{label}: expected rejection, but it was accepted")
        continue
    messages = " | ".join(e.message for e in errors)
    if expected not in messages:
        problems.append(
            f"{label}: rejected, but no error contained {expected!r}; "
            f"got: {messages}")

for problem in problems:
    print(problem)

assert not problems, \
    f"{len(problems)} case(s) did not behave as intended"

print(f"Rejected {len(CASES)} malformed manifests as intended")
print("SUCCESS")
