with Ada.Directories;

with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Errors;
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

   -----------
   -- Clone --
   -----------

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Directory_Path)
                   return Outcome
   is (This.Clone (From, Into, Branch => ""));

   -----------
   -- Clone --
   -----------

   not overriding
   function Clone (This   : VCS;
                   From   : URL;
                   Into   : Directory_Path;
                   Branch : String;
                   Depth  : Natural := 0)
                   return Outcome
   is
      pragma Unreferenced (This);
      Extra : constant Vector :=
                     (if Log_Level < Trace.Info or else not CLIC.TTY.Is_TTY
                      then Empty_Vector & "-q"
                      else Empty_Vector);
      Depth_Opts : constant Vector :=
                     (if Depth /= 0 and then Commit (From) = ""
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
                 Extra & Branch_Opts & Depth_Opts & Repo (From) & Into);

      if Commit (From) /= "" then
         declare
            Guard : Directories.Guard (Directories.Enter (Into))
              with Unreferenced;
         begin
            --  Checkout a specific commit.
            --  "-q" needed to avoid the "detached HEAD" warning from git
            Run_Git (Empty_Vector & "checkout" & "-q" & Commit (From));

            --  Update the submodules, if any
            Run_Git (Empty_Vector & "submodule" & "update" &
                       "--init" & "--recursive" & Extra);
         end;
      end if;

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Clone;

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
            Old : constant URL :=
                    Handler.Remote_URL (Repo, Handler.Remote (Repo));
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

   ------------
   -- Remote --
   ------------

   function Remote (This    : VCS;
                    Path    : Directory_Path;
                    Checked : Boolean := True)
                    return String is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Output : constant AAA.Strings.Vector :=
                 Run_Git_And_Capture (Empty_Vector & "remote");
   begin
      if Output.Is_Empty then
         if Checked then
            Raise_Checked_Error ("No remote is configured");
         else
            return "";
         end if;
      else
         return Output.First_Element;
      end if;
   end Remote;

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
                 Run_Git_And_Capture (Empty_Vector & "ls-remote" & From);
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

      if Ref = "HEAD" or else Ref = "" then
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

   ------------
   -- Status --
   ------------

   function Status (This : VCS;
                    Repo : Directory_Path)
                    return States
   is
      Guard  : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;

      --  Out_1 should be portable. Out_2 is used as last resort; I believe
      --  git is not localized so it should always work but since it relies on
      --  human output it might break at any time I guess. Worst case, we would
      --  report an 'Ahead' as 'Dirty'.

      Out_1 : constant AAA.Strings.Vector :=
        Run_Git_And_Capture (Empty_Vector & "status" & "--porcelain");

      Untracked_File : Natural := 0;
      Tracked_File   : Natural := 0;
   begin

      for Line of Out_1 loop
         if Contains (Line, "GNAT-TEMP-") then
            --  Turns out the temporary file we use to capture the output of
            --  "git status" makes git to return a dirty tree. We filter these
            --  out then.
            null;
         elsif Has_Prefix (Line, "??") then
            Untracked_File := Untracked_File + 1;
         else
            Tracked_File := Tracked_File + 1;
         end if;
      end loop;

      if Tracked_File /= 0 then
         --  There are added/modified tracked files
         return Dirty;
      else
         --  Retrieve revisions from remote branch tip up to our local HEAD. If
         --  not empty, we are locally ahead.
         declare
            Branch : constant String := This.Branch (Repo);
            Remote : constant String := This.Remote (Repo, Checked => False);
         begin
            if Remote = "" then
               return No_Remote;
            elsif (for all B of Branches (Repo, Local => False) =>
                     B /= Remote & "/" & Branch)
            then -- The branch doesn't even exist remotely
               return Ahead;
            elsif Run_Git_And_Capture
              (Empty_Vector
               & "rev-list"
               & String'(Remote & "/" & Branch
                 &  "..HEAD")).Is_Empty
            then
               return Clean;
            else
               --  At least one local commit not pushed to the remote
               return Ahead;
            end if;
         end;
      end if;
   end Status;

   -------------------------
   -- Transform_To_Public --
   -------------------------

   function Transform_To_Public (Remote : String) return URL is
      Domain : constant String := Head (Tail (Remote, '@'), ':');
   begin
      if Has_Prefix (Remote, "git@") and then
        Known_Transformable_Hosts.Contains (Domain)
      then
         return  Public : constant URL :=
           "https://" & Domain & "/" & Tail (Remote, ':')
           & (if Has_Suffix (Remote, ".git")
              then ""
              else ".git")
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
                  & String'(This.Remote (Repo) & "/" & Branch)
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
