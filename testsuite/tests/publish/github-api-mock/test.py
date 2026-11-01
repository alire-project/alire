"""
Test the use of the GitHub REST API by `alr publish` and `alr self-update`
without network, by mocking `curl`. Responses contain the kinds of values found
in real ones, used by alr or not, as they all go through the JSON parser.
This also ensures consistent JSON data no matter the library in use by alr.

See the publish/submit-request-cancel test for a test against the real API.

This test is a handful...
"""

import json
import os
import re

from drivers.alr import alr_settings_set, run_alr
from drivers.asserts import assert_eq, assert_match, assert_not_substring, \
    assert_substring
from drivers.helpers import MockCommand, on_windows

ROUTES   = os.path.abspath("routes.json")
LOG      = os.path.abspath("curl.log")
PULLS    = "/repos/alire-project/alire-index/pulls"
COMMENTS = "/repos/alire-project/alire-index/issues"
RUNS     = "/repos/alire-project/alire-index/actions/runs"
RELEASES = "/repos/alire-project/alire/releases"
GRAPHQL  = "/graphql"

# Replies according to the routes file, and logs requests for later checks
CURL = f"""\
import json, os, sys
from urllib.parse import parse_qs, urlsplit

args = sys.argv[1:]
url  = urlsplit(next(arg for arg in args if arg.startswith("https://")))
head = parse_qs(url.query).get("head_sha")
verb = args[args.index("-X") + 1] if "-X" in args else "GET"
data = args[args.index("-d") + 1] if "-d" in args else None

with open(r"{LOG}", "a", encoding="utf-8") as log:
    log.write(json.dumps(
        {{"verb": verb,
          "path": url.path,
          "data": os.fsencode(data).decode("utf-8") if data else None}})
        + "\\n")

# Downloads are not REST calls; behave as with --fail on a missing file
if "--output" in args:
    sys.exit(22)

key = verb + " " + url.path + ("?head_sha=" + head[0] if head else "")
with open(r"{ROUTES}", encoding="utf-8") as routes:
    status, body = json.load(routes).get(key, [404, {{"message": "Not Found"}}])

sys.stdout.buffer.write(
    (f"HTTP/2 {{status}} \\r\\nx-ratelimit-remaining: 59\\r\\n\\r\\n"
     + json.dumps(body, indent=2, ensure_ascii=False)
       .replace("<", chr(92) + "u003c")  # As real responses do
     + "\\n").encode("utf-8"))
"""


def pull(number, sha="ok", draft=False, state="open", label="mylogin:branch",
         title="Plain title", **extra):
    """
    A pull request as returned by the API, with fields not used by alr too
    """
    return {"id": 4000000000 + number,
            "number": number,
            "node_id": f"PR_{number}",
            "url": f"https://api.github.com{PULLS}/{number}",
            "state": state,
            "locked": False,
            "title": title,
            "body": "<Lines>\nwith \"quotes\", \\ / \t and ñ€\U0001F600",
            "assignee": None,
            "labels": [],
            "draft": draft,
            "score": 1.5,
            "head": {"label": label,
                     "sha": sha,
                     "repo": {"id": 7, "owner": {"login": "mylogin"}}},
            **extra}


def run_with(*conclusions):
    """
    The workflow runs for a commit, with one run per given conclusion, or with
    no conclusion at all for a value of `...`
    """
    return [200, {"total_count": len(conclusions),
                  "workflow_runs":
                      [{"id": i} if c is ... else {"id": i, "conclusion": c}
                       for i, c in enumerate(conclusions)]}]


def review(login, state):
    return {"id": 1, "user": {"login": login}, "state": state, "body": None}


# Each pull request, with its expected status in `alr publish --status`

OPEN = [(pull(1,  draft=True),         "Checks_Passed"),
        (pull(2),                      "Under_Review"),
        (pull(3,  sha="failed"),       "Checks_Failed"),
        (pull(4,  sha="timed_out"),    "Checks_Failed"),
        (pull(5,  sha="null"),         "Checks_Ongoing"),
        (pull(6,  sha="absent"),       "Checks_Ongoing"),
        (pull(7,  sha="none"),         "Checks_Ongoing"),
        (pull(8,  sha="unknown"),      "Checks_Ongoing"),
        (pull(9),                      "Changes_Requested"),  # By one of two
        (pull(10),                     "Under_Review"),       # Changes redone
        (pull(11),                     "Changes_Requested"),  # Lower case
        (pull(12, state="OPEN", label="mylogin:rama-ñ"), "Under_Review")]

# Reviews pile up, and only the last one of each reviewer counts
REVIEWS = {9:  [review("alice", "CHANGES_REQUESTED"),
                review("bob", "APPROVED")],
           10: [review("alice", "CHANGES_REQUESTED"),
                review("alice", "APPROVED")],
           11: [review("alice", "changes_requested")]}

routes = {
    # Other users' PRs must be filtered out here and not asked about
    f"GET {PULLS}": [200, [p for p, _ in OPEN]
                     + [pull(99, label="other:branch")]],
    f"GET {PULLS}/20": [200, pull(20, state="closed", merged=True)],
    f"GET {PULLS}/21": [200, pull(21, state="closed", merged=False)],
    f"GET {RUNS}?head_sha=ok":
        run_with("success", "skipped", "neutral"),
    f"GET {RUNS}?head_sha=failed":     run_with("success", "failure"),
    f"GET {RUNS}?head_sha=timed_out":  run_with("timed_out"),
    f"GET {RUNS}?head_sha=null":       run_with("success", None),
    f"GET {RUNS}?head_sha=absent":     run_with("success", ...),
    f"GET {RUNS}?head_sha=none":       run_with(),
    f"GET {RUNS}?head_sha=unknown":    run_with("success", "stale"),
    f"PATCH {PULLS}/1":                [200, pull(1, state="closed")],
    f"POST {COMMENTS}/1/comments":     [201, {"id": 5000000000}],
    f"POST {GRAPHQL}":                 [200, {"data": {"id": None}}],
    f"GET {RELEASES}/latest":          [200, {"tag_name": "v9.9.9"}],
    f"GET {RELEASES}/tags/nightly":
        [200, {"tag_name": "nightly", "assets": [{"name": "other.zip"}]}],
}
for p, _ in OPEN:
    routes[f"GET {PULLS}/{p['number']}"] = [200, {**p, "merged": False}]
    routes[f"GET {PULLS}/{p['number']}/reviews"] = \
        [200, REVIEWS.get(p["number"], [])]


def set_routes(**changes):
    """
    Write the current routes, plus any changes given as a route key with
    underscores in place of the initial space
    """
    with open(ROUTES, "w", encoding="utf-8") as f:
        json.dump(routes | {k.replace("_", " ", 1): v
                            for k, v in changes.items()}, f)


def requests():
    """
    Returns the requests logged since the last call
    """
    if not os.path.exists(LOG):
        return []
    with open(LOG, encoding="utf-8") as f:
        lines = f.readlines()
    os.remove(LOG)
    return [json.loads(line) for line in lines]


def sent(verb):
    """
    Returns the requests with the given verb among the ones since the last call
    """
    return [r for r in requests() if r["verb"] == verb]


alr_settings_set("user.github_login", "mylogin")
alr_settings_set("msys2.install_dir", os.path.abspath("does_not_exist"),
                 builtin=on_windows())
set_routes()

with MockCommand("curl", CURL, "cmd_dir"):

    # Every status, leaving out the PR by another user

    p = run_alr("publish", "--status")
    status = {int(fields[0]): fields[2]
              for fields in (re.sub(r"\x1b\[\d+m", "", line).split()
                             for line in p.out.splitlines())
              if len(fields) >= 3 and fields[0].isdigit()}
    assert_eq({p["number"]: expected for p, expected in OPEN}, status)
    assert_not_substring(f"{PULLS}/99/reviews",
                         "\n".join(r["path"] for r in requests()))

    # Text with non-ASCII characters in a response is shown as-is

    if not on_windows():
        assert_substring("mylogin:rama-ñ", p.out)

    # No PRs at all

    set_routes(**{f"GET_{PULLS}": [200, []]})
    assert_substring("No pending submissions found",
                     run_alr("publish", "--status").out)
    set_routes()

    # Cancelling, sending a reason that requires escaping, also with backslashes
    # next to quotes and at the end

    requests()
    REASON = 'say "hi" \\" / ' + ('' if on_windows() else 'ñ ') + '\\'
    p = run_alr("publish", "--cancel=1", f"--reason={REASON}", quiet=False)
    assert_substring("closed successfully", p.out)
    log = requests()

    patch = [r for r in log if r["verb"] == "PATCH"]
    assert_eq([f"{PULLS}/1"], [r["path"] for r in patch])
    assert_eq({"state": "closed"}, json.loads(patch[0]["data"]))

    comment = [r for r in log if r["verb"] == "POST"]
    assert_eq([f"{COMMENTS}/1/comments"], [r["path"] for r in comment])
    body = json.loads(comment[0]["data"])["body"]
    assert_match("Closed using `alr .*` with reason: ", body)
    assert body.endswith(REASON), body

    # A backslash next to a quote wouldn't reach curl intact on Windows, where
    # GNAT doesn't escape it when quoting the arguments of a spawned process

    assert_not_substring('\\"', comment[0]["data"])

    # Text that isn't valid UTF-8 can't prevent the request; this is possible
    # on Windows but we can only reproduce it here on other platforms

    if not on_windows():
        p = run_alr("publish", "--cancel=1",
                    "--reason=" + os.fsdecode(b"caf\xe9"), quiet=False)
        assert_substring("closed successfully", p.out)
        comment = [r for r in requests() if r["verb"] == "POST"]
        assert_substring("caf", json.loads(comment[0]["data"])["body"])

    # Cannot cancel what isn't open or doesn't exist

    for number, message in [(20, "is already closed"),
                            (21, "is already closed"),
                            (999,
                             "Could not retrieve pull request information")]:
        p = run_alr("publish", f"--cancel={number}", "--reason=x",
                    complain_on_error=False)
        assert_substring(message, p.out)
    assert_eq([], sent("PATCH"))

    # Requesting a review, which uses GraphQL, requires passing checks

    p = run_alr("publish", "--request-review=1", quiet=False)
    assert_substring("Review requested successfully", p.out)
    query = sent("POST")
    assert_eq([GRAPHQL], [r["path"] for r in query])
    assert_not_substring('\\"', query[0]["data"])
    assert_match(r'mutation \{ markPullRequestReadyForReview \(input: \{ '
                 r'clientMutationId: "alr-.*", pullRequestId: "PR_1" \}\)',
                 json.loads(query[0]["data"])["query"])

    p = run_alr("publish", "--request-review=2", complain_on_error=False)
    assert_substring("Reviews can only be requested", p.out)
    assert_eq([], sent("POST"))

    set_routes(**{f"POST_{GRAPHQL}": [200, {"errors": [{"message": "Nope"}],
                                            "data": None}]})
    p = run_alr("publish", "--request-review=1", complain_on_error=False)
    assert_substring("Error updating PR using GitHub GraphQL API", p.out)
    set_routes()

    # Self-update finds the latest release, and looks for the binary in a
    # release, here in the nightly as we are not running a stable version

    requests()
    p = run_alr("self-update", "--location=.", quiet=False,
                complain_on_error=False)
    assert_substring("--release=9.9.9", p.out)
    assert_substring("could not find artifact", p.out)
    assert_eq([f"{RELEASES}/latest", f"{RELEASES}/tags/nightly"],
              [r["path"] for r in requests()])

    ARCHIVE = p.out.split("could not find artifact '")[1].split("'")[0]
    set_routes(**{f"GET_{RELEASES}/tags/nightly":
                  [200, {"assets": [{"name": "other.zip"},
                                    {"name": ARCHIVE}]}]})
    p = run_alr("self-update", "--location=.", quiet=False,
                complain_on_error=False)
    assert_not_substring("could not find artifact", p.out)
    assert requests()[-1]["path"].endswith(f"/{ARCHIVE}"), p.out

    set_routes(**{f"GET_{RELEASES}/tags/nightly": [404, {"message": "Nope"}]})
    p = run_alr("self-update", "--location=.", complain_on_error=False)
    assert_substring("could not find release nightly", p.out)

print("SUCCESS")
