with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Errors;
with Alire.Utils.Tools;
with Alire.Utils.TTY;

package body Alire.VCSs.Git is

   -------------
   -- Run_Git --
   -------------

   procedure Run_Git (Arguments : Utils.String_Vector) is
   begin
      --  Make sure git is installed
      Utils.Tools.Check_Tool (Utils.Tools.Git);

      OS_Lib.Subprocess.Checked_Spawn ("git", Arguments);
   end Run_Git;

   -------------------------
   -- Run_Git_And_Capture --
   -------------------------

   function Run_Git_And_Capture (Arguments : Utils.String_Vector)
                                 return Utils.String_Vector
   is
   begin
      --  Make sure git is installed
      Utils.Tools.Check_Tool (Utils.Tools.Git);

      return
        OS_Lib.Subprocess.Checked_Spawn_And_Capture
          ("git", Arguments, Err_To_Out => True);
   end Run_Git_And_Capture;

   ------------
   -- Branch --
   ------------

   function Branch (This : VCS;
                    Path : Directory_Path)
                    return String
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Output : constant Utils.String_Vector :=
        Run_Git_And_Capture (Empty_Vector & "branch");
   begin
      for Line of Output loop
         if Line'Length > 0 and then Line (Line'First) = '*' then
            return Utils.Tail (Line, ' ');
         end if;
      end loop;

      Raise_Checked_Error
        ("Unexpected output from 'git branch: "
         & Output.Flatten ("\n "));
   end Branch;

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
      Extra : constant String_Vector :=
                Empty_Vector
                & (if Log_Level < Trace.Info
                   then "-q"
                   else "--progress");
      Depth_Opts : constant String_Vector :=
                     (if Depth /= 0 and then Commit (From) = ""
                      then Empty_Vector & "--depth" & Utils.Trim (Depth'Image)
                                        & "--no-single-branch" -- but all tips
                   else Empty_Vector);
      Branch_Opts : constant String_Vector :=
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
         Output : constant Utils.String_Vector :=
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
      Output : constant Utils.String_Vector :=
                 Run_Git_And_Capture (Empty_Vector & "config" & "--list");
   begin
      for Line of Output loop
         if Starts_With (Line, "remote." & Origin & ".url") then
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
      Output : constant Utils.String_Vector :=
        Run_Git_And_Capture (Empty_Vector & "status");
   begin

      --  When a repo is in detached head state (e.g. after checking out a
      --  commit instead of a branch), 'git status' will have as the first line
      --  "HEAD detached at <commit>". Other changes come next.

      return not Output.Is_Empty
        and then Utils.Contains (Output.First_Element, "HEAD detached");
   end Is_Detached;

   function Is_Repository (This : VCS;
                           Path : Directory_Path) return Boolean
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Unused : Utils.String_Vector;
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
      Output : constant Utils.String_Vector :=
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

   -------------------
   -- Remote_Commit --
   -------------------

   not overriding
   function Remote_Commit (This : VCS;
                           From : URL;
                           Ref  : String := "HEAD") return String
   is
      Output : constant Utils.String_Vector :=
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
         Not_Found : constant Git_Commit := (others => 'x');
         Result    : Git_Commit := Not_Found;
      begin
         for Line of Output loop
            if Ends_With (Line, Ref) then
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
            return Result;
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

      Out_1 : constant Utils.String_Vector :=
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
         elsif Starts_With (Line, "??") then
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
            Remote : constant String := This.Remote (Repo, Checked => False);
         begin
            if Remote = "" then
               return No_Remote;
            elsif Run_Git_And_Capture
              (Empty_Vector
               & "rev-list"
               & String'(Remote & "/" & This.Branch (Repo)
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
      if Starts_With (Remote, "git@") and then
        Known_Transformable_Hosts.Contains (Domain)
      then
         return  Public : constant URL :=
           "https://" & Domain & "/" & Tail (Remote, ':')
           & (if Ends_With (Remote, ".git")
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
      Extra : constant String_Vector :=
                (if Log_Level < Trace.Info
                 then Empty_Vector & "-q"
                 else Empty_Vector & "--progress");
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

   -----------------
   -- Head_Commit --
   -----------------

   function Head_Commit (This : VCS;
                            Repo : Directory_Path)
                            return String
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Repo)) with Unreferenced;
      Output : constant Utils.String_Vector :=
                 Run_Git_And_Capture
                   (Empty_Vector
                    & "log" & "-n1" & "--oneline" & "--no-abbrev-commit");
   begin
      return Head (Output.First_Element, ' ');
   end Head_Commit;

end Alire.VCSs.Git;
