# AI Usage Policy

Alire welcomes contributions from everyone, including contributions created
with the help of AI tools and assistants, as long as a human carefully reviews,
curates, and takes responsibility for them.

This page describes what we expect from contributors who use these tools.

## A note on autonomous AI agents

This page is for human contributors who are using AI as an assistant. AI
agents operating directly on this repository (for example, via Claude Code,
Codex or Cursor agents) follow an additional set of rules, such as prefixing
PR titles with `[AI]`, which are in `/AGENTS.md`.

## Using AI for code

It is OK to use AI to generate code, draft tests, fix bugs, or help you
navigate the codebase, as long as it is done responsibly. Regardless of
whether the code is written by you or by an AI, it still has to meet the
project's standards described in `/CONTRIBUTING.md`:

- It must build without warnings, and the relevant tests must pass (see
  `/testsuite/README.md`).
- It must follow the Coding Standards and Implementation Practices.
- It must satisfy the Pull-request Checklist (tests, `doc/user-changes.md`,
  `BREAKING.md`, format specification and schema, as applicable).

AI contributions must meet the same bar as human contributions. This means, at
a minimum, no unnecessary verbosity, boilerplate, unrelated changes,
overwrought analysis.

The phenomenon commonly known as "AI slop" in which a prompt is given and
the result is uncritically dumped on maintainers is obvious not allowed and
will result in summary dismissal and possible banning in case of abuse.

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

For PRs mainly written by AI, the human responsible must write an opening
paragraph with the gist of the issue in your own words, that is self-contained
to understand the change, under the title "Human Summary". Fully AI-generated
PRs without a human summary will be closed without review.

Issues and pull requests that appear to be AI-generated but do not disclose it
may be closed without review. Contributors who repeatedly submit undisclosed AI
content, or who ignore this policy, may be blocked from contributing.

## Co-authorship

Code is the responsibility of and considered authored by the human contributor.
Do not use Co-authorship tags for AI-generated content in git commits. Informal
descriptions of the AI tools used are acceptable instead in commit
descriptions.

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
merged, and only then open the next one. Pull requests that are low-effort,
untested, or undisclosed AI output may be closed without a detailed review,
and authors who repeatedly submit them may be blocked from contributing.

Similarly, large PRs out of nowhere with significant changes should be avoided.
Open an issue first, discuss the proposal and design, get the maintainers
behind your idea, and then do the work and open the PR. Dumping a large change
on maintainers without previous warning will significantly hinder review, as it
may not fit with the project's goals or design, it will be harder to understand
and dissect, and will generally require more time to review. A PR that is
deemed too large or too complex may be closed without review, or be requested
to be split into smaller, more manageable pieces.

## You are responsible for what you submit

Before you open an issue or a PR, you should:

- **Understand the code.** Read what the AI produced. Be able to explain what
  each change does and why it is needed. Tweak the code where necessary.
- **Verify it works.** Build it, run the tests, and confirm the behavior you
  are claiming.
- **Edit the prose.** AI-generated descriptions can be long, repetitive, or
  inaccurate. Trim them and make sure they match what the code actually does.

You are the author of the contribution. The AI is not.

## On the asymmetry of AI-produced and human-produced content

The bottom line to keep in mind when contributing AI artifacts is that the
human contributor is responsible for the content, as if they had written it
themselves. Given the easiness with which AIs can produce content, try always
to restrain these contributions to humanly manageable amounts, for humans
working with other humans.
