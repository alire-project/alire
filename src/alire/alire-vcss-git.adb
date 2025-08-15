with Ada.Directories;
with Ada.Containers;

with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Errors;
with Alire.URI;
with Alire.Utils.Tools;
with Alire.Utils.User_Input.Query_Config;
with Alire.VFS;

with GNAT.Source_Info;

package body Alire.VCSs.Git is

   package User_Info renames Utils.User_Input.Query_Config;

   -------------
   -- Run_Git --
   -------------

   procedure Run_Git (Arguments : AAA.Strings.Vector) is
   begin
      --  Make sure git is installed
      Utils.Tools.Check_Tool (Utils.Tools.Git);

      OS_Lib.Subprocess.Checked_Spawn ("git", Arguments);
   end Run_Git;

   -------------------------
   -- Run_Git_And_Capture --
   -------------------------

   function Run_Git_And_Capture (Arguments : AAA.Strings.Vector)
                                 return AAA.Strings.Vector
   is
   begin
      --  Make sure git is installed
      Utils.Tools.Check_Tool (Utils.Tools.Git);

      return
        OS_Lib.Subprocess.Checked_Spawn_And_Capture
          ("git", Arguments, Err_To_Out => True);
   end Run_Git_And_Capture;

   -----------------------------------
   -- Unchecked_Run_Git_And_Capture --
   -----------------------------------

   procedure Unchecked_Run_Git_And_Capture (Arguments : AAA.Strings.Vector;
                                            Output    : out AAA.Strings.Vector;
                                            Code      : out Integer)
   is
   begin
      --  Make sure git is installed
      Utils.Tools.Check_Tool (Utils.Tools.Git);
      Code := OS_Lib.Subprocess.Unchecked_Spawn_And_Capture
        (Command             => "git",
         Arguments           => Arguments,
         Output              => Output,
         Err_To_Out          => True);
   end Unchecked_Run_Git_And_Capture;

   ----------------
   -- Add_Remote --
   ----------------

   procedure Add_Remote (Repo : Directory_Path;
                         Name : String;
                         URL  : String)
   is
      Guard  : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
   begin
      Run_Git (To_Vector ("remote") & "add" & Name & URL);
   end Add_Remote;

   ------------
   -- Branch --
   ------------

   function Branch (This : VCS;
                    Path : Directory_Path)
                    return String
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Output : constant AAA.Strings.Vector :=
        Run_Git_And_Capture (Empty_Vector & "branch");
   begin
      for Line of Output loop
         if Line'Length > 0 and then Line (Line'First) = '*' then
            return Tail (Line, ' ');
         end if;
      end loop;

      Raise_Checked_Error
        ("Unexpected output from 'git branch: "
         & Output.Flatten ("\n "));
   end Branch;

   --------------
   -- Branches --
   --------------

   function Branches (Repo   : Directory_Path;
                      Local  : Boolean := True;
                      Remote : Boolean := True)
                      return AAA.Strings.Vector
   is
      Guard  : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
      Output : constant AAA.Strings.Vector :=
                 Run_Git_And_Capture
                   (Empty_Vector
                    & "branch" & "--format=%(refname:short)"
                    & (if Local and then Remote then
                         To_Vector ("-a")
                      elsif Remote then
                         To_Vector ("-r")
                      else
                         Empty_Vector));
   begin
      return Output;
   end Branches;

   function Branch_Remote (This   : VCS;
                           Path   : Directory_Path;
                           Branch : String;
                           Status : out Branch_States)
                           return String
   is
      use Ada.Containers;
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Ref    : constant String := "refs/heads/" & Branch;
      Output : constant AAA.Strings.Vector :=
        Run_Git_And_Capture
          (Empty_Vector
           & "for-each-ref"
           --  We need the name of the remote and the name of the remote branch
           --  thereon which the local branch tracks (normally the same as the
           --  local branch, but not always).
           --  We use ":" as a separator, as it is not permitted in refs or
           --  remote names.
           & "--format=%(upstream:remotename):%(upstream)"
           & Ref);
   begin
      if Output.Length = 0 then
         Status := No_Branch;
         return "";
      elsif Output.Length > 1 then
         --  Git should prevent branch "a/b" coexisting with "a", so this
         --  case (where there are multiple matches of the form
         --  'refs/heads/<Branch>/<something>') should not be possible if
         --  Branch exists.
         Status := No_Branch;
         return "";
      elsif Output.First_Element = ":" then
         --  The branch exists, but doesn't track a remote (both
         --  "%(upstream:remotename)" and "%(upstream)" are the empty
         --  string, so only the separator is returned)
         Status := No_Remote;
         return "";
      elsif not Contains (Output.First_Element, ":") then
         --  The separator is missing for some reason
         raise Program_Error
           with "Git gave output inconsistent with specified format";
      else
         declare
            Remote_Branch_Ref : constant String :=
              Tail (Output.First_Element, ":");
            Commits_Ahead     : constant AAA.Strings.Vector :=
              Run_Git_And_Capture
                (Empty_Vector
                 & "rev-list"
                 & String'(Remote_Branch_Ref & ".." & Branch));
            Commits_Behind    : constant AAA.Strings.Vector :=
              Run_Git_And_Capture
                (Empty_Vector
                 & "rev-list"
                 & String'(Branch & ".." & Remote_Branch_Ref));
         begin
            Status :=
              (if Commits_Ahead.Length /= 0 then Ahead
               elsif Commits_Behind.Length /= 0 then Behind
               else Synced);
            return Head (Output.First_Element, ":");
         end;
      end if;
   end Branch_Remote;

   -----------
   -- Clone --
   -----------

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Directory_Path;
                   Commit : String := "")
                   return Outcome
   is (This.Clone_Branch
     (From, Into, Branch => "", Commit => Commit, Depth => 1));

   ------------------
   -- Clone_Branch --
   ------------------

   function Clone_Branch (This   : VCS;
                          From   : URL;
                          Into   : Directory_Path;
                          Branch : String;
                          Commit : String := "";
                          Depth  : Natural := 0)
                          return Outcome
   is
      pragma Unreferenced (This);
      Extra : constant Vector :=
                     (if Log_Level < Trace.Info or else not CLIC.TTY.Is_TTY
                      then Empty_Vector & "-q"
                      else Empty_Vector);
      Depth_Opts : constant Vector :=
                     (if Depth /= 0 and then Commit = ""
                      then Empty_Vector & "--depth" & Trim (Depth'Image)
                                        & "--no-single-branch" -- but all tips
                   else Empty_Vector);
      Branch_Opts : constant Vector :=
                      (if Branch /= ""
                       then Empty_Vector & "--branch" & Branch
                       else Empty_Vector);
   begin
      Trace.Detail ("Checking out [git]: " & From);

      Run_Git (Empty_Vector & "clone" & "--recursive" &
                 Extra & Branch_Opts & Depth_Opts & Repo_URL (From) & Into);

      if Commit /= "" then
         declare
            Guard : Directories.Guard (Directories.Enter (Into))
              with Unreferenced;
         begin
            --  Checkout a specific commit.
            --  "-q" needed to avoid the "detached HEAD" warning from git
            Run_Git (Empty_Vector & "checkout" & "-q" & Commit);

            --  Update the submodules, if any
            Run_Git (Empty_Vector & "submodule" & "update" &
                       "--init" & "--recursive" & Extra);
         end;
      end if;

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Clone_Branch;

   -------------
   -- Command --
   -------------

   function Command (Repo  : Directory_Path;
                     Args  : AAA.Strings.Vector;
                     Quiet : Boolean := False)
                     return Output
   is
      Guard : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
   begin
      return
        (Run_Git_And_Capture
           (Arguments =>
              (if Quiet then To_Vector ("-q") else Empty_Vector) & Args)
         with null record);
   end Command;

   ----------------
   -- Commit_All --
   ----------------

   function Commit_All (Repo : Directory_Path;
                        Msg  : String := "Automatic by alr") return Outcome
   is
      Guard : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
   begin
      Run_Git (Empty_Vector & "add" & ".");
      Run_Git (Empty_Vector
               & "-c"
               & String'("user.email=" & User_Info.User_Email)
               & "commit"
               & "-m" & Msg);
      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Commit_All;

   -------------------------
   -- Discard_Uncommitted --
   -------------------------

   function Discard_Uncommitted (Repo : Directory_Path;
                                 Discard_Untracked : Boolean := False)
                                 return Outcome
   is
      Guard : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
   begin
      Run_Git (Empty_Vector & "reset" & "-q" & "--hard" & "HEAD");
      if Discard_Untracked then
         Run_Git (Empty_Vector & "clean" & "-fd");
      end if;
      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Discard_Uncommitted;

   ----------
   -- Push --
   ----------

   function Push (Repo   : Directory_Path;
                  Remote : String;
                  Force  : Boolean := False;
                  Create : Boolean := False;
                  Token  : String  := "") return Outcome
   is
      Guard : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;

      Writname : constant String := "writable";

      Force_Flags : constant Vector :=
                      (if Force then To_Vector ("-f") else Empty_Vector);

      Create_Flags : constant Vector :=
                       (if Create
                        then To_Vector ("-u")
                             & (if Token /= ""
                                then Writname
                                else Remote)
                             & Handler.Branch (Repo)
                        else Empty_Vector);
   begin
      if Token = "" then
         Run_Git (Empty_Vector
                  & "push"
                  & Force_Flags
                  & Create_Flags);
      else
         --  Create a temporary remote with our credentials and use it to push
         declare
            Old : constant URL := Handler.Remote_URL (Repo, Remote);
            Writurl  : constant URL :=
                         Replace (Old, "//", "//"
                                  & User_Info.User_GitHub_Login
                                  & ":" & Token & "@");
         begin
            Run_Git (Empty_Vector
                     & "remote" & "add" & Writname & Writurl);
            Run_Git (Empty_Vector
                     & "push"
                     & Force_Flags
                     & (if Create
                        then Create_Flags
                        else To_Vector (Writname)));
            Run_Git (Empty_Vector
                     & "remote" & "remove" & Writname);
         end;
      end if;

      return Outcome_Success;
   exception
      when E : others =>
         --  Ensure token is not left behind even in case of push failure
         if Handler.Remote_URL (Repo, Writname) /= "" then
            Run_Git (Empty_Vector
                     & "remote" & "remove" & Writname);
         end if;

         return Alire.Errors.Get (E);
   end Push;

   ---------------------
   -- Revision_Commit --
   ---------------------

   function Revision_Commit (This   : VCS;
                             Repo   : Directory_Path;
                             Rev    : String)
                             return String
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
   begin
      declare
         Output : constant AAA.Strings.Vector :=
                 Run_Git_And_Capture
                   (Empty_Vector
                    & "log" & Rev
                    & "-n1" & "--oneline" & "--no-abbrev-commit");
      begin
         --  Check expected output
         if Output.Length in 1 then
            return Head (Output.First_Element, ' ');
         else
            return "";
         end if;
      end;
   exception
      when others =>
         --  git exits with code 128 for a non-existing Rev
         return "";
   end Revision_Commit;

   ---------------
   -- Fetch_URL --
   ---------------

   function Fetch_URL (This   : VCS;
                       Repo   : Directory_Path;
                       Origin : String := "origin";
                       Public : Boolean := True)
                       return URL
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
      Output : constant AAA.Strings.Vector :=
                 Run_Git_And_Capture (Empty_Vector & "config" & "--list");
   begin
      for Line of Output loop
         if Has_Prefix (Line, "remote." & Origin & ".url") then
            declare
               URL : constant Alire.URL := Tail (Line, '=');
            begin
               if Public then
                  return Transform_To_Public (URL);
               else
                  return URL;
               end if;
            end;
         end if;
      end loop;

      return "";
   end Fetch_URL;

   -----------------
   -- Is_Detached --
   -----------------

   function Is_Detached (This : VCS;
                         Path : Directory_Path) return Boolean
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Output : constant AAA.Strings.Vector :=
        Run_Git_And_Capture (Empty_Vector & "status");
   begin

      --  When a repo is in detached head state (e.g. after checking out a
      --  commit instead of a branch), 'git status' will have as the first line
      --  "HEAD detached at <commit>". Other changes come next.

      return not Output.Is_Empty
        and then Contains (Output.First_Element, "HEAD detached");
   end Is_Detached;

   -------------------
   -- Is_Repository --
   -------------------

   function Is_Repository (This : VCS;
                           Path : Directory_Path) return Boolean
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Unused : AAA.Strings.Vector;
   begin
      return
        OS_Lib.Subprocess.Unchecked_Spawn_And_Capture
          (Command             => "git",
           Arguments           => Empty_Vector & "status",
           Output              => Unused,
           Err_To_Out          => True) = 0;
   end Is_Repository;

   --------------------
   -- Commit_Remotes --
   --------------------

   function Commit_Remotes (This    : VCS;
                            Path    : Directory_Path;
                            Commit  : Git_Commit)
                            return AAA.Strings.Set is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Output : constant AAA.Strings.Vector := Run_Git_And_Capture
                 (Empty_Vector
                  & "for-each-ref"
                  & "--format=%(refname:rstrip=-3)"
                  & String'("--contains=" & Commit)
                  & "refs/remotes/");
      Result : AAA.Strings.Set := Empty_Set;
   begin
      for Line of Output loop
         --  We want to return just the remote name, without "refs/remotes/".
         Assert (Has_Prefix (Line, "refs/remotes/"), "Unexpected Git output");
         Result.Include (Tail (Line, "refs/remotes/"));
      end loop;
      return Result;
   end Commit_Remotes;

   -----------------
   -- Repo_Remote --
   -----------------

   function Repo_Remote (This    : VCS;
                         Path    : Directory_Path;
                         Checked : Boolean := True)
                         return String is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Output : constant AAA.Strings.Vector :=
                 Run_Git_And_Capture (Empty_Vector & "remote");
   begin
      if Output.Length in 1 then
         return Output.First_Element;
      else
         if Checked and then Output.Is_Empty then
            Raise_Checked_Error ("No remote is configured");
         elsif Checked then
            Raise_Checked_Error ("Multiple remotes are configured");
         else
            return "";
         end if;
      end if;
   end Repo_Remote;

   ----------------
   -- Remote_URL --
   ----------------

   not overriding
   function Remote_URL (This    : VCS;
                        Path    : Directory_Path;
                        Remote  : String := "origin")
                        return String
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Output : constant AAA.Strings.Vector :=
                 Run_Git_And_Capture (Empty_Vector & "remote" & "-v");
   begin
      for Line of Output loop
         declare
            Cols : constant Vector := Split (Line, ASCII.HT, Trim => True);
         begin
            if Cols (1) = Remote then
               return AAA.Strings.Split (Cols (2), ' ').First_Element;
            end if;
         end;
      end loop;

      return "";
   end Remote_URL;

   -------------
   -- Remotes --
   -------------

   function Remotes (Repo : Directory_Path) return AAA.Strings.Set is
      Guard  : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
      Output : constant AAA.Strings.Vector :=
                 Run_Git_And_Capture (To_Vector ("remote"));
   begin
      return Result : AAA.Strings.Set do
         for Line of Output loop
            Result.Include (Line);
         end loop;
      end return;
   end Remotes;

   -------------------
   -- Remote_Commit --
   -------------------

   not overriding
   function Remote_Commit (This : VCS;
                           From : URL;
                           Ref  : String := "HEAD") return String
   is
      Output : constant AAA.Strings.Vector :=
        Run_Git_And_Capture (Empty_Vector & "ls-remote" & Repo_URL (From));
   begin
      --  Sample output from git (space is tab):
      --  95818710c1a2bea0cbfa617a67972fe984761227        HEAD
      --  b0825ac9373ed587394cf5e7ecf51fd7caf9290a        refs/heads/feat/cache
      --  95818710c1a2bea0cbfa617a67972fe984761227        refs/heads/master
      --  a917c31c47a8bd0155c402f692b63bd77e53bae7        refs/pull/1/head
      --  22cb794ed99dfe6cbb0541af558ada1d2ed8fdbe        refs/tags/v0.1
      --  ae6fdd0711bb3ca2c1e2d1d18caf7a1b82a11f0a        refs/tags/v0.1^{}
      --  7376b76f23ab4421fbec31eb616d767edbec7343        refs/tags/v0.2

      --  Prepare Ref to make it less ambiguous

      if Ref in "HEAD" | "" then
         return This.Remote_Commit (From, ASCII.HT & "HEAD");
      elsif Ref (Ref'First) not in '/' | ASCII.HT then
         return This.Remote_Commit (From, '/' & Ref);
      end if;

      --  Once here is reached, the Ref is ready for comparison

      declare
         Not_Found : constant String (Git_Commit'Range) := (others => 'x');
         Result    : String := Not_Found;
      begin
         for Line of Output loop
            if Has_Suffix (Line, Ref) then
               if Result = Not_Found then
                  Result := Head (Line, ASCII.HT);
               else
                  Raise_Checked_Error ("Reference is ambiguous: "
                                       & TTY.Emph (Ref));
               end if;
            end if;
         end loop;

         if Result = Not_Found then
            return "";
         else
            return Git_Commit (Result); -- Contents check
         end if;
      end;
   end Remote_Commit;

   -----------------
   -- Dirty_Files --
   -----------------

   function Dirty_Files (This              : VCS;
                         Repo              : Directory_Path;
                         Include_Untracked : Boolean := False)
                         return AAA.Strings.Set
   is
      Guard  : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;

      Output : constant AAA.Strings.Vector :=
        Run_Git_And_Capture (Empty_Vector & "status" & "-z" & "--no-renames");

      Lines  : constant AAA.Strings.Vector :=
        (if Output.Length in 0 then Empty_Vector
         else Split (Output.First_Element, Character'Val (0)));
      --  'git status -z' splits "lines" on the null character, not newline.

      Paths  : AAA.Strings.Set := Empty_Set;
   begin
      if Output.Length not in 0 | 1 then
         --  'git status -z' splits on null character, so should only yield one
         --  line of output.
         Raise_Checked_Error
           ("Unexpected output from 'git status -z': "
            & Output.Flatten (New_Line & "  "));
      end if;

      for Line of Lines loop
         if Contains (Line, "GNAT-TEMP-") then
            --  Turns out the temporary file we use to capture the output of
            --  "git status" makes git to return a dirty tree. We filter these
            --  out then.
            null;
         elsif Line = "" then
            --  The output of 'git status -z' includes a trailing null byte,
            --  so the last element of Lines is an irrelevant empty string.
            null;
         elsif not Include_Untracked and then Has_Prefix (Line, "??") then
            --  Ignore untracked files if Include_Untracked is False.
            null;
         else
            --  'git status -z' yields lines of the form "XY <path>", where
            --  "X" and "Y" are single-character status codes.
            --  We therefore strip the first three characters of each line to
            --  get the path.
            Paths.Include (Line (Line'First + 3 .. Line'Last));
         end if;
      end loop;
      return Paths;
   end Dirty_Files;

   ------------
   -- Status --
   ------------

   function Status (This : VCS;
                    Repo : Directory_Path)
                    return States
   is
   begin
      if Dirty_Files (This, Repo, Include_Untracked => False).Length in 0 then
         return Clean;
      else
         return Dirty;
      end if;
   end Status;

   -------------------------
   -- Transform_To_Public --
   -------------------------

   function Transform_To_Public (Remote : String) return URL is
      Domain : constant String := URI.Host (Remote);
   begin
      if URI.URI_Kind (Remote) in URI.SCP_Style_Git and then
        Known_Transformable_Hosts.Contains (Domain)
      then
         return  Public : constant URL := URI.Make_VCS_Explicit
           ("https://" & Domain & "/" & Tail (Remote, ':'),
            URI.Git)
         do
            Trace.Warning ("Private git " & TTY.URL (Remote)
                           & " transformed to public " & TTY.URL (Public));
         end return;
      else
         return Remote;
      end if;
   end Transform_To_Public;

   ------------
   -- Update --
   ------------

   overriding
   function Update (This : VCS;
                    Repo : Directory_Path)
                    return Outcome
   is (This.Update (Repo, Branch => ""));

   ------------
   -- Update --
   ------------

   function Update (This   : VCS;
                    Repo   : Directory_Path;
                    Branch : String)
                    return Outcome
   is
      Guard : Directories.Guard (Directories.Enter (Repo))
        with Unreferenced;
      Extra : constant Vector :=
                (if Log_Level < Trace.Info
                 then Empty_Vector & "-q"
                 else Empty_Vector);
   begin

      --  Switch branch if changed

      if Branch /= "" and then This.Branch (Repo) /= Branch then

         Trace.Detail ("Detected branch change needed in git update at "
                       & TTY.URL (Repo) & "; switching from "
                       & TTY.Emph (This.Branch (Repo)) & " to "
                       & TTY.Emph (Branch));

         Run_Git (Empty_Vector & "fetch");
         --  In case there are new remote branches

         Run_Git (Empty_Vector
                  & "checkout"
                  & String'(This.Repo_Remote (Repo) & "/" & Branch)
                  & "-B"
                  & Branch
                  & Extra
                  & "--recurse-submodules");
         --  Force overwrite any previous local same branch. Since we just
         --  fetched, the checkout should be up to date and there's no need
         --  to additionally pull.

      else

         Run_Git (Empty_Vector & "pull" & Extra & "--recurse-submodules");
         --  Plain pull

      end if;

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Update;

   --------------
   -- Worktree --
   --------------

   function Worktree (This : VCS;
                      Repo : Directory_Path)
                      return Worktree_Data
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
      Output : AAA.Strings.Vector;
      Code   : Integer;

      --------------------
      -- Worktree_Error --
      --------------------

      function Worktree_Error (Output : AAA.Strings.Vector;
                               Error  : String) return String
      is (Error & " in `git worktree list`: " & Output.Flatten ("\n"));

   begin
      Code :=
        OS_Lib.Subprocess.Unchecked_Spawn_And_Capture
          (Command             => "git",
           Arguments           =>
             Empty_Vector
           & "worktree"
           & "list"
           & "--porcelain",
           Output              => Output,
           Err_To_Out          => True);

      return Data : Worktree_Data do
         if Code /= 0 then
            Trace.Debug ("git worktree list failed with code: "
                         & Trim (Code'Image));
            return;
         end if;

         Assert (Natural (Output.Length) >= 3,
                 Worktree_Error (Output, "Unexpected output length (lines)"));

         Assert (Head (Output (1), ' ') = "worktree",
                 Worktree_Error (Output, "Unexpected 1st line"));

         Assert (Head (Output (2), ' ') = "HEAD",
                 Worktree_Error (Output, "Unexpected 2nd line"));

         Assert (Head (Output (3), ' ') = "branch",
                 Worktree_Error (Output, "Unexpected 3rd line"));

         --  Git on windows returns an absolute but forward-slashed path.
         --  Depending on if it is a Windows git or a msys2 git it will [not]
         --  have also a drive letter. So we convert this path to a native one,
         --  as promised by the type in use.

         Data.Worktree :=
           +VFS.To_Native (Portable_Path (Tail (Output (1), ' ')));
         Data.Head     :=  Tail (Output (2), ' ');
         Data.Branch   := +Tail (Output (3), ' ');
      end return;
   end Worktree;

   -------------
   -- Git_Dir --
   -------------

   function Git_Dir return Any_Path
   is (OS_Lib.Getenv (Name    => "GIT_DIR",
                      Default => ".git"));

   -----------------
   -- Head_Commit --
   -----------------

   function Head_Commit (This : VCS;
                            Repo : Directory_Path)
                            return String
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
      Output : constant AAA.Strings.Vector :=
                 Run_Git_And_Capture
                   (Empty_Vector
                    & "log" & "-n1" & "--oneline" & "--no-abbrev-commit");
   begin
      return Head (Output.First_Element, ' ');
   end Head_Commit;

   ------------------------------
   -- Get_Rel_Path_Inside_Repo --
   ------------------------------

   function Get_Rel_Path_Inside_Repo (This : VCS;
                                      Dir  : Directory_Path)
                                      return Relative_Path
   is
      pragma Unreferenced (This);
      use Ada.Directories;
      use Alire.Directories.Operators;
      Guard  : Directories.Guard (Directories.Enter (Full_Name (Dir)))
        with Unreferenced;
   begin
      return "." /
        Run_Git_And_Capture
         (Empty_Vector & "rev-parse" & "--show-prefix").First_Element;
   end Get_Rel_Path_Inside_Repo;

   ----------
   -- Root --
   ----------

   function Root (This : VCS) return Optional.Absolute_Path is
      pragma Unreferenced (This);
      Code   : Integer;
      Output : AAA.Strings.Vector;
   begin
      Unchecked_Run_Git_And_Capture
        (Empty_Vector & "rev-parse" & "--show-toplevel",
         Output,
         Code);

      if Code /= 0 then
         Trace.Debug ("git rev-parse returned code: " & Code'Image
                      & " at " & GNAT.Source_Info.Source_Location);
         return Optional.Absolute_Paths.Empty;
      else
         return Optional.Absolute_Paths.Unit (Output.First_Element);
      end if;
   end Root;

end Alire.VCSs.Git;
