# AI Usage Policy

Alire welcomes contributions from everyone, including contributions created
with the help of AI tools and assistants, as long as a human carefully reviews,
curates, and takes responsibility for them.

This page describes what we expect from contributors who use these tools.

## Scope

This policy applies to all repositories in the `alire-project` GitHub
organization, including the community index (`alire-index`) and the crates
submitted to it. The coding standards referenced below are those of the `alire`
repository; other repositories may have their own, which apply instead.

## A note on autonomous AI agents

This page is for human contributors who are using AI as an assistant. AI
agents operating directly on a repository follow additional rules, which 
for this repository are in `/AGENTS.md`.

## Using AI for code

It is OK to use AI to generate code, draft tests, fix bugs, or help you
navigate the codebase. Regardless of whether the code is written by you or by
an AI, it still has to meet the project's standards described in
`/CONTRIBUTING.md`:

- It must build without warnings, and the relevant tests must pass (see
  `/testsuite/README.md`).
- It must follow the Coding Standards and Implementation Practices.
- It must satisfy the Pull-request Checklist (tests, `doc/user-changes.md`,
  `BREAKING.md`, format specification and schema, as applicable).
- It must be compatible with the license of the repository it is submitted to
  (GPL-3.0 for `alire`). Do not submit AI output that reproduces code under an
  incompatible license.

AI contributions must meet the same bar as human contributions. At a minimum,
this means no unnecessary verbosity, boilerplate, unrelated changes, or
overwrought analysis, whether in code, comments, or PR descriptions.

Submitting the output of a prompt to maintainers without critically reviewing
it, commonly known as "AI slop", is not allowed.

## Interacting with maintainers should be human

When it comes to interacting with the project (PR descriptions, code review
replies, issue comments, and discussion threads) we expect to interact with
real humans, not with AI-generated replies.

Please do not:

- Paste a reviewer's comment back into an AI and post the raw output as your
  reply.
- Generate issue or PR descriptions wholesale from AI without reading and
  editing them yourself.
- Use AI to argue with maintainers on your behalf.

Maintainer bandwidth is limited, and the conversation around a change is where
most of the value of code review lives. If that conversation is between an AI
on one side and a human on the other, it stops being useful.

## Disclose when AI was used

If AI was used to generate a significant portion of an issue, PR, or the code
it contains, please say so in the submission. A short note in the PR
description is enough, for example: "The initial implementation was drafted
with Claude and then reviewed and edited by me."

For PRs mainly written by AI, start the description with a "Human Summary"
section, written by you in your own words, that explains the gist of the
change and can be understood on its own. The rest of the description may be
drafted by AI, but you must still read and edit it as described above.

## Co-authorship

Code is authored by, and is the responsibility of, the human contributor. Do
not add `Co-authored-by:` or similar trailers crediting AI tools to commits.
Instead, you may mention the tools used informally in the commit message or
the PR description.

## Quality over quantity

Modern AI tools make it easy to generate a large number of changes very
quickly. Please resist the temptation to open many pull requests at once, for
example by pointing an AI tool at the codebase and submitting whatever it
produces.

A stack of simultaneous, similar pull requests from one author takes much
longer for us to review than a single, well-tested change, and it is often a
sign that the work has not been read or tested by a human. We would much
rather receive one change that you understand and have verified than ten that
you have not.

A good rhythm is to open one pull request, work with us to get it reviewed and
merged, and only then open the next one.

Likewise, avoid large PRs with significant changes that arrive unannounced.
As `/CONTRIBUTING.md` advises, open an issue first, discuss the proposal and
design, and get the maintainers' agreement before doing the work. A large
unannounced change is hard to review: it may not fit the project's goals or
design, it is harder to understand and dissect, and it takes much longer to
review. AI tools make such changes very easy to produce, so this bears
repeating here.

## Bug and security reports

AI tools are good at finding potential problems, and also at reporting
problems that do not exist. Before reporting a problem found with the help of
AI:

- **Verify that it is real**, ideally with a reproducer such as a test or a
  sequence of `alr` commands. Do not report what you could not confirm.
- **Include a fix if you can**, following the rest of this policy.
- **Say what you could not verify.** If there is no reproducer, or a fix could
  not be built or tested, state it explicitly.
- **Report vulnerabilities privately**, through GitHub's private vulnerability
  reporting at https://github.com/alire-project/alire/security/advisories/new,
  never in public issues or PRs.

## You are responsible for what you submit

Before you open an issue or a PR, you should:

- **Understand the code.** Read what the AI produced. Be able to explain what
  each change does and why it is needed. Fix it where necessary.
- **Verify it works.** Build it, run the tests, and confirm the behavior you
  are claiming.
- **Edit the prose.** AI-generated descriptions can be long, repetitive, or
  inaccurate. Trim them and make sure they match what the code actually does.

You are the author of the contribution. The AI is not.

## On the asymmetry of AI-produced and human-produced content

The bottom line is that you are responsible for the content you contribute,
as if you had written it yourself. Since AI makes producing content so easy,
always keep your contributions to amounts that a human can review: this is
humans working with other humans.

## Enforcement

Maintainers decide how to handle each contribution. Depending on the case,
they may:

- Treat it like any other contribution.
- Review it at a lower priority or with extra scrutiny.
- Ask you to explain the code in more detail, to rework it, or to split it
  into smaller pieces.
- Close it without review. This is the expected outcome for AI slop, for PRs
  mainly written by AI without a Human Summary, for undisclosed AI content,
  and for large changes that were not discussed first.

Contributors who repeatedly ignore this policy may be blocked from
contributing to `alire-project` repositories.
