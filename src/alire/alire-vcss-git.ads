with AAA.Strings;
with Alire.Optional;
with Alire.Utils;

package Alire.VCSs.Git is

   Known_Transformable_Hosts : constant AAA.Strings.Vector;
   --  Known hosts that honor the git@ --> https:// transformation

   subtype Git_Commit is String (1 .. 40) with
     Dynamic_Predicate =>
       (for all Char of Git_Commit => Char in Utils.Hexadecimal_Character);

   function Is_Valid_Commit (S : String) return Boolean
   is (S'Length = Git_Commit'Length and then
         (for all Char of S => Char in Utils.Hexadecimal_Character));

   No_Commit : constant Git_Commit := (others => '0');
   --  This is actually returned by e.g. `git worktree`, even if it could be a
   --  real commit. I guess the chances are deemed too low.

   function Git_Dir return Any_Path;
   --  ".git" unless overridden by GIT_DIR

   function Short_Commit (Commit : Git_Commit) return String
   is (Commit (Commit'First .. Commit'First + 7));

   type VCS (<>) is new VCSs.VCS with private;

   function Handler return VCS;

   procedure Add_Remote (Repo : Directory_Path;
                         Name : String;
                         URL  : String);

   not overriding
   function Branch (This : VCS;
                    Path : Directory_Path)
                    return String;
   --  Return the name of the branch currently checked out in the repo at Path.
   --
   --  This may have the form "(HEAD detached at <hash>)".

   function Branches (Repo   : Directory_Path;
                      Local  : Boolean := True;
                      Remote : Boolean := True)
                      return AAA.Strings.Vector;
   --  List all known branches (without going on-line)

   type Branch_States is
     (No_Branch, -- The specified branch doesn't exist
      No_Remote, -- Local branch is not configured to track any remote
      Synced,    -- Local and remote branches are synchronished
      Ahead,     -- Local branch has commit(s) not present on remote branch
      Behind);   -- Not Ahead, and remote branch has commit(s) not on local

   not overriding
   function Branch_Remote (This   : VCS;
                           Path   : Directory_Path;
                           Branch : String;
                           Status : out Branch_States)
                           return String;
   --  Retrieve the name of the remote tracked by a specified branch (usually
   --  "origin"), and determine whether the local and remote branches are
   --  synchronised.
   --
   --  Returns "" when Status is No_Branch or No_Remote.

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Directory_Path;
                   Commit : String := "")
                   return Outcome;
   --  Make a shallow clone of the given URL, optionally specifying a commit.
   --  For more precise control, use the following Clone signature.

   function Clone_Branch (This   : VCS;
                          From   : URL;
                          Into   : Directory_Path;
                          Branch : String;
                          Commit : String := "";
                          Depth  : Natural := 0)
                          return Outcome;
   --  Specify a branch to check out after cloning. Branch may be "" for the
   --  default remote branch. For any Depth /= 0, apply --depth <Depth>. A
   --  commit may optionally be specified.

   type Output is new AAA.Strings.Vector with null record;

   procedure Discard_Output (This : Output) is null;
   --  Allows running a git command and ignoring its output

   function Command (Repo  : Directory_Path;
                     Args  : AAA.Strings.Vector;
                     Quiet : Boolean := False)
                     return Output;

   --  Run any command directly. "git" is implicit. "-q" appended when Quiet.
   --  Will raise on exit code /= 0

   function Commit_All (Repo : Directory_Path;
                        Msg  : String := "Automatic by alr")
                        return Outcome;
   --  Add and commit all changes in a given repo; commiter will be set to the
   --  user email stored in our config.

   not overriding
   function Dirty_Files (This              : VCS;
                         Repo              : Directory_Path;
                         Include_Untracked : Boolean := False)
                         return AAA.Strings.Set;
   --  Return the paths of any files with uncommitted changes.
   --
   --  Ignored files are not included. Untracked files are not included unless
   --  Include_Untracked is True.

   function Discard_Uncommitted (Repo : Directory_Path;
                                 Discard_Untracked : Boolean := False)
                                 return Outcome;
   --  Reset all uncommitted changes to tracked files, and optionally also
   --  untracked files.
   --
   --  Ignored files are not discarded.

   function Push (Repo   : Directory_Path;
                  Remote : String;
                  Force  : Boolean := False;
                  Create : Boolean := False;
                  Token  : String  := "") return Outcome;
   --  Push to the remote. If Create, use "-u <Remote> <current branch>". If an
   --  Auth Token is given, a temporary remote that includes the token will be
   --  created and removed for the push; in this case, there will be no change
   --  to the local branch unless Create is True, in which case any configured
   --  upstream will be unset.

   not overriding
   function Remote_Commit (This : VCS;
                           From : URL;
                           Ref  : String := "HEAD") return String;
   --  Returns the commit matching Ref by ls-remote. If none, returns "". If
   --  several match, Checked_Error.

   not overriding
   function Revision_Commit (This   : VCS;
                             Repo   : Directory_Path;
                             Rev    : String)
                             return String;
   --  Returns the commit for a revision (commit, tag, branch...), if the
   --  repository and revision exist. Returns "" otherwise.
   --
   --  Should be a no-op for a commit.

   not overriding
   function Is_Detached (This : VCS;
                         Path : Directory_Path) return Boolean;
   --  Says if the repo checked out at Path is in a detached HEAD state.

   not overriding
   function Is_Repository (This : VCS;
                           Path : Directory_Path) return Boolean;
   --  Check if a repo exists at Path

   not overriding
   function Commit_Remotes (This    : VCS;
                            Path    : Directory_Path;
                            Commit  : Git_Commit)
                            return AAA.Strings.Set;
   --  Retrieve the names of all remotes on which Commit can be found.
   --
   --  Only searches the current remote-tracking branches, so this information
   --  may be out of date without a preceeding 'git fetch'.

   not overriding
   function Repo_Remote (This    : VCS;
                         Path    : Directory_Path;
                         Checked : Boolean := True)
                         return String;
   --  Retrieve the name of the only remote configured for the repository
   --  (usually "origin"). If Checked, raise Checked_Error when zero or
   --  multiple remotes are configured. Otherwise, return "".

   not overriding
   function Remote_URL (This    : VCS;
                        Path    : Directory_Path;
                        Remote  : String := "origin")
                        return String;
   --  Returns the URL for the given remote, or "" if unset. Assumes both fetch
   --  and push remotes are the same.

   function Remotes (Repo : Directory_Path) return AAA.Strings.Set;
   --  Return all the remote names defined in the repository

   overriding
   function Update (This : VCS;
                    Repo : Directory_Path)
                    return Outcome;

   not overriding
   function Update (This   : VCS;
                    Repo   : Directory_Path;
                    Branch : String)
                    return Outcome;
   --  Update and track Branch, if given.
   --
   --  Does not discard uncommitted changes, so will fail if there are local
   --  changes which conflict with the update.
   --
   --  Raises Checked_Error when the repo has multiple remotes configured and
   --  Branch is not the same as the current HEAD.

   type States is (Dirty,     -- Uncommitted local changes
                   Clean);    -- Clean
   --  States we are interested in for publishing

   not overriding
   function Status (This : VCS;
                    Repo : Directory_Path)
                    return States;
   --  Return whether the repo contains uncommitted changes to tracked files.
   --
   --  Note that untracked files don't cause a Dirty result!

   not overriding
   function Fetch_URL (This   : VCS;
                       Repo   : Directory_Path;
                       Origin : String := "origin";
                       Public : Boolean := True)
                       return URL;
   --  Retrieve the "fetch" url of the given origin, or "" if no repo, no
   --  origin, or any other unforeseen circumstance. If Public, a git@github
   --  private URL is transformed into its equivalent https:// public URL.

   not overriding
   function Head_Commit (This : VCS;
                         Repo : Directory_Path)
                         return String;
   --  Obtain the currently checked out Rev in the repository

   not overriding
   function Get_Rel_Path_Inside_Repo (This : VCS;
                                      Dir  : Directory_Path)
                                      return Relative_Path;
   --  Return the relative path from the VCSs root to Dir. Will raise if Dir is
   --  not a real dir or not actually inside a git repo. This is a wrapper on
   --  git rev-parse --show-prefix

   function Root (This : VCS) return Optional.Absolute_Path;
   --  Return the repo absolute root path, if in a repo; otherwise Empty. This
   --  is a wrapper on git rev-parse --show-toplevel

   function Transform_To_Public (Remote : String) return URL;
   --  For a Known_Transformable_Host, return the https:// equivalent of a
   --  git@... address. Otherwise return Remote unmodified.

   type Worktree_Data is record
      Worktree : Unbounded_Absolute_Path := +""; --  When not a git repo
      Head     : Git_Commit := No_Commit;        -- When not on a commit
      Branch   : UString    := +"detached";      -- When on detached head
   end record;

   function Worktree (This : VCS;
                      Repo : Directory_Path)
                      return Worktree_Data;
   --  Retrieve bundled info on a working repo; wraps `git worktree`

private

   type VCS is new VCSs.VCS with null record;

   function Handler return VCS is (null record);

   use AAA.Strings;

   Known_Transformable_Hosts : constant AAA.Strings.Vector :=
                                 Empty_Vector
                                 & "github.com"
                                 & "gitlab.com";

end Alire.VCSs.Git;
