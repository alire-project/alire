# Project Overview

This project is a command-line tool (`alr`) and a supporting library
(`libalire`) designed to manage dependencies in Ada and SPARK projects, and
related operations like initialization, building, and running tests.

This tool fulfills a similar role as `cargo` for Rust, `go` for Go, `opam` for
OCaml, for the Ada language and its subset SPARK.

The tool leverages an online repository of packages (crates) which is a regular
git repository at GitHub, named the "community index".

## Folder Structure

- `/src/alire`: Contains the source code for the library.
- `/src/alr`: Contains the source code for the command-line tool.
- `/src/templates`: Contains generated source files from templates.
- `/doc`: Contains documentation for the project, including index specification
  and user guides, used also for generating the website.
- `/doc/catalog-format-spec.md`: Contains the specification for the
  index format used by the project.
- `/doc/user-changes.md`: Contains notices about user-facing changes.
- `/testsuite`: Contains the test suite for the project, including unit tests
  and integration tests.
- `/testsuite/tests`: Contains end-to-end tests using the Python e3-testsuite
  framework.
- `/testsuite/tests_ada`: Contains unit tests written in Ada.
- `/BREAKING.md`: Contains information about breaking changes in the master
  branch wrt the latest stable release.
- `/RELEASING.md`: Contains instructions for releasing a new version of the
  project.
- `/RELEASING-post.md`: Contains instructions for post-release tasks.

## Coding Standards

- Use subprogram boxes before all top-level subprogram bodies.

- Use subprogram boxes before non-trivial (more than 3-4 body lines) nested
  subprogram bodies.

- Use one empty line before and after subprogram boxes, and before and after
  subprogram bodies.

- Use formal English for all error messages (e.g., avoid contractions).

- Do not have long blocks of uncommented code, even if it is trivial code. A
  comment should prefix such long blocks of trivial code explaining its
  purpose, with one blank line before and after.

- Comment all complex code, no matter if it is a single line. Comments that
  refer to a single statement should be immediately after the statement and
  without separation (no blank line in between).

## Implementation Practices

- The exception `Checked_Error` is used for errors that should *not* result in
  an exception backtrace being exposed to the user. This exception is caught by
  the top-level exception handler, resulting in a pretty-printed error message.
  `Checked_Error` is raised with `Alire.Raise_Checked_Error` so the error
  message can be as long as needed (using `raise` directly has an unspecified
  maximum error message length and hence is not recommended).

- Exceptions `Child_Failed`, `Command_Failed` and `Wrong_Command_Arguments`
  also result in pretty-printed error messages, but are used for specific
  purposes. They do not result in observable differences but should be used
  accordingly. These are never raised directly; rather they are triggered with
  subprograms `Reportaise_Command_Failed` and `Reportaise_Wrong_Arguments`.
  `Child_Failed` is raised by spawning subprograms when the forked process
  fails. Typically, there is no need to raise this exception directly.

- `Program_Error` is the preferred exception to be triggered for diagnosed
  abnormal situations that should not occur and will result in a bug box and
  exception backtrace (in verbose mode) being exposed to the user.

- Other Ada exceptions triggered by the Ada runtime that are not handled on a
  case-by-case basis are OK to be propagated but will also result in a bug box.
  Hence, erroneous but expected situations should be handled (e.g., a
  `Constraint_Error` when validating user inputs) and not propagated when it is
  OK to exit with code 0, or re-raised as a `Checked_Error` to terminate
  execution gracefully with non-zero exit code.

- Situations that are dubious and can be considered an error, but that we want
  to allow if the user is absolutely sure, can be reported with
  `Alire.Recoverable_User_Error`. This will result in a checked error or just a
  warning depending on whether `--force` flag is in effect.

- Use `Alire.Recoverable_Program_Error` for anomalous situations that can
  somehow be detected and recovered from, despite not being the user fault and
  indicating a bug in the program. A bug box will be printed, but the program
  will continue normally. This usage should be very rare.

- Use the `Alire.Unimplemented` exception for temporary missing functionality.
  In general, use of this exception should not make into the master branch.

## Pull-request Checklist

Before submitting a PR, ensure you have met as many as applicable of the tasks
described in `/.github/PULL_REQUEST_TEMPLATE.md`. Namely, at least:

- Include functional tests for new features or bug fixes. See
  `/testsuite/README.md` for further information.
- Include Ada unit tests for critical new subprograms, type definitions, etc.
  Each unit test is a main procedure in a file in `/testsuite/tests_ada`.
- Document user-facing changes in `/doc/user-changes.md`.
- Document breaking changes in `/BREAKING.md`.
- Document index format changes in `/doc/catalog-format-spec.md`.