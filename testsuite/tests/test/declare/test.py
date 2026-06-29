"""
Tests must be declared with `pragma Alire_Test`. Verify that:
  * only declared parameterless main procedures are run;
  * sources that cannot be a runnable main (packages, functions, ...) are
    ignored without any pragma;
  * `Auxiliary_File` excludes a main silently;
  * an undeclared main is a recoverable error (fails, unless --force);
  * an Alire_Test pragma on a non-main is a recoverable error;
  * `Auxiliary_File` combined with other keys is a recoverable error.
"""

import os

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_substring
from drivers.testing import (
    find_test,
    parse_json_result,
    write_test,
)

base = os.getcwd()


def write(path, text):
    with open(path, "w") as f:
        f.write(text)


def fresh_crate(name):
    """Init a test crate from `base` and drop the seeded test."""
    os.chdir(base)
    init_local_crate(name, with_test=True)
    os.remove(f"./tests/src/{name}_tests-assertions_enabled.adb")


# --- Only declared mains run; non-mains and auxiliaries are not tests --------

fresh_crate("basics")

# A declared, passing test (write_test prepends `pragma Alire_Test;`).
write_test("ok", "null;", crate="basics")

# A main explicitly excluded from testing.
write_test("helper_main", "null;", crate="basics", declare=False,
           prelude="pragma Alire_Test (Auxiliary_File);\n")

# A package body (+ spec): never a test, and needs no pragma.
write("./tests/src/basics_tests-pkg.ads",
      "package Basics_Tests.Pkg is\n   procedure Noop;\nend Basics_Tests.Pkg;\n")
write("./tests/src/basics_tests-pkg.adb",
      "package body Basics_Tests.Pkg is\n"
      "   procedure Noop is begin null; end Noop;\n"
      "end Basics_Tests.Pkg;\n")

# A library function body: never a runnable main, and needs no pragma.
write("./tests/src/basics_tests-fn.adb",
      "function Basics_Tests.Fn return Integer is\n"
      "begin\n   return 0;\nend Basics_Tests.Fn;\n")

p = run_alr("--format=json", "test")
data = parse_json_result(p)
assert_eq(1, data["summary"]["total"])
assert_eq(0, data["summary"]["failures"])
assert_eq("pass", find_test(data["tests"], "ok")["status"])
assert_eq([], [t["name"] for t in data["tests"] if t["name"] != "ok"])

# --- An undeclared main is a recoverable error ------------------------------

fresh_crate("undecl")
write_test("orphan", "null;", crate="undecl", declare=False)

p = run_alr("test", quiet=False, complain_on_error=False)
assert p.status != 0, "undeclared main should fail the run"
assert_substring("is not declared", p.out)

# --force downgrades it to a warning and skips the source.
p = run_alr("test", quiet=False, force=True)
assert_substring("is not declared", p.out)

# --- An Alire_Test pragma on a non-main is a recoverable error --------------

fresh_crate("nonmain")
write("./tests/src/nonmain_tests-bad.adb",
      "pragma Alire_Test;\n"
      "procedure Nonmain_Tests.Bad (X : Integer) is\n"
      "begin\n   null;\nend Nonmain_Tests.Bad;\n")

p = run_alr("test", quiet=False, complain_on_error=False)
assert p.status != 0, "declared non-main should fail the run"
assert_substring("not a parameterless main procedure", p.out)

# --- Auxiliary_File must stand alone ----------------------------------------

fresh_crate("auxbad")
write_test(
    "conflict", "null;", crate="auxbad", declare=False,
    prelude=('pragma Alire_Test (Auxiliary_File);\n'
             'pragma Alire_Test (Name, "x");\n'))

p = run_alr("test", quiet=False, complain_on_error=False)
assert p.status != 0, "Auxiliary_File with other keys should fail the run"
assert_substring("must not carry any other", p.out)


print("SUCCESS")
