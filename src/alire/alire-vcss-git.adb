with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Errors;
with Alire.Utils;             use Alire.Utils;

package body Alire.VCSs.Git is

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
                 OS_Lib.Subprocess.Checked_Spawn_And_Capture
                   ("git",
                    Empty_Vector & "branch");
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
   is
      pragma Unreferenced (This);
      Extra : constant String_Vector :=
        Empty_Vector & (if Log_Level < Trace.Info
                        then "-q"
                        else "--progress");
   begin
      Trace.Detail ("Checking out [git]: " & From);

      OS_Lib.Subprocess.Checked_Spawn
        ("git",
         Empty_Vector & "clone" & Extra & Repo (From) & Into);

      if Commit (From) /= "" then
         declare
            Guard : Directories.Guard (Directories.Enter (Into))
              with Unreferenced;
         begin
            OS_Lib.Subprocess.Checked_Spawn
              ("git",
               Empty_Vector & "checkout" & Commit (From));
         end;
      end if;

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Clone;

   -----------------
   -- Is_Detached --
   -----------------

   function Is_Detached (This : VCS;
                         Path : Directory_Path) return Boolean
   is
      pragma Unreferenced (This);
      Guard  : Directories.Guard (Directories.Enter (Path)) with Unreferenced;
      Output : constant Utils.String_Vector :=
                 OS_Lib.Subprocess.Checked_Spawn_And_Capture
                   ("git",
                    Empty_Vector & "status");
   begin

      --  When a repo is in detached head state (e.g. after checking out a
      --  commit instead of a branch), 'git status' will have as the first line
      --  "HEAD detached at <commit>". Other changes come next.

      return not Output.Is_Empty
        and then Utils.Contains (Output.First_Element, "HEAD detached");
   end Is_Detached;

   ------------
   -- Update --
   ------------

   overriding
   function Update (This : VCS;
                    Repo : Directory_Path)
                    return Outcome
   is
      pragma Unreferenced (This);
      Guard : Directories.Guard (Directories.Enter (Repo))
        with Unreferenced;
      Extra : constant String_Vector :=
                (if Log_Level < Trace.Info
                 then Empty_Vector & "-q "
                 else Empty_Vector & "--progress");
   begin
      OS_Lib.Subprocess.Checked_Spawn ("git", Empty_Vector & "pull" & Extra);
      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Update;

end Alire.VCSs.Git;
