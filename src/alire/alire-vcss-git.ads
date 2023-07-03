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

   type VCS (<>) is new VCSs.VCS with private;

   function Handler return VCS;

   procedure Add_Remote (Repo : Directory_Path;
                         Name : String;
                         URL  : String);

   not overriding
   function Branch (This : VCS;
                    Path : Directory_Path)
                    return String;
   --  Returns the branch name of the repo checked out at Path.

   function Branches (Repo   : Directory_Path;
                      Local  : Boolean := True;
                      Remote : Boolean := True)
                      return AAA.Strings.Vector;
   --  List all known branches (without going on-line)

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Directory_Path)
                   return Outcome;

   not overriding
   function Clone (This   : VCS;
                   From   : URL;
                   Into   : Directory_Path;
                   Branch : String;
                   Depth  : Natural := 0)
                   return Outcome;
   --  Specify a branch to check out after cloning. Branch may be "" for the
   --  default remote branch. For any Depth /= 0, apply --depth <Depth>. A
   --  commit may be specified as From#Commit_Id

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

   function Push (Repo   : Directory_Path;
                  Remote : String;
                  Force  : Boolean := False;
                  Create : Boolean := False;
                  Token  : String  := "") return Outcome;
   --  Push to the remote. If Create, use "-u <Remote> <current branch>". If an
   --  Auth Token is given, a temporary remote that includes the token will be
   --  created and removed for the push; the local branch will be set to track
   --  the original remote afterwards.

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
   --  Returns the commit for a revision, if the repository and revision
   --  (commit, tag, branch...) exist. Should be a no-op for a commit.

   not overriding
   function Is_Detached (This : VCS;
                         Path : Directory_Path) return Boolean;
   --  Says if the repo checked out at Path is in a detached HEAD state.

   not overriding
   function Is_Repository (This : VCS;
                           Path : Directory_Path) return Boolean;
   --  Check if a repo exists at Path

   not overriding
   function Remote (This    : VCS;
                    Path    : Directory_Path;
                    Checked : Boolean := True)
                    return String;
   --  Retrieve current remote name (usually "origin"). If checked, raise
   --  Checked_Error when no remote configured. Otherwise, return "";

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

   type States is (Dirty,     -- Uncommitted local changes
                   No_Remote, -- Clean, no remote configured
                   Clean,     -- Clean, up to date with remote
                   Ahead);    -- Clean, ahead of remote (needs pull)
   --  States we are interested in for publishing

   not overriding
   function Status (This : VCS;
                    Repo : Directory_Path)
                    return States;
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
