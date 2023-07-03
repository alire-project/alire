private package Alire.Publish.Submit is

   --  Steps for the assistant, not intended to be called directly. These steps
   --  are executed right after manifest creation in the order that follows.

   procedure Exists (Context : in out Data);
   --  Check if there's a PR already for this release. GitHub doesn't allow two
   --  PRs from the same user and from the same branch, so to avoid a failure
   --  late in the process, we check immediately that no such a PR exists
   --  already.

   --  To uniquely identify the PR, a branch `release/crate-version` is created
   --  in the user fork of the community index.

   procedure Fork (Context : in out Data);
   --  To be able to submit the PR, the user needs to have its own fork of the
   --  community index (as users cannot create branches in the community index
   --  itself). This step checks if the user already has its own fork, or
   --  creates it otherwise.

   --  To be able to create a fork in the user's account, we need a Personal
   --  Access Token (PAT) with `repo` permissions. The same token will allow us
   --  to later create a new branch, push to it, and open the PR. This PAT can
   --  be supplied via the GH_TOKEN env var (also used by the `gh` tool), or
   --  `alr` will ask for it when undefined. We don't store it in our config
   --  as this is a sensitive piece of info.

   procedure Clone (Context : in out Data);
   --  Once the fork is sure to exist, we clone it locally to
   --  <cache>/publish/community, unless a repo already exists at that location
   --  with a matching remote. We add a new `upstream` remote (or recreate
   --  it just to be sure) that points to the community index. We fetch from
   --  upstream, as the status of the user's clone is not important, but the
   --  upstream status is.

   procedure Push (Context : in out Data);
   --  Once in sync with upstream, we locally create the
   --  `release/crate-version` branch, copy the manifest to its intended
   --  location, commit it, and force-push to the user's fork, creating the
   --  same branch remotely. As we are using upstream to base the new branch,
   --  and we are force-pushing, there's no possibility of ending in an
   --  incongruous or conflicting state.

   --  At this point, the user could open their fork on GitHub and they would
   --  see the banner asking to create a PR against the community index from
   --  its fork, because there is a recent push.

   procedure Request_Pull (Context : in out Data);
   --  This step simply uses the REST API to open the pull request on the
   --  community index, using the user's fork branch as head, just as if the
   --  web interface were used. A default title and message using the release
   --  name and version are used.

end Alire.Publish.Submit;
