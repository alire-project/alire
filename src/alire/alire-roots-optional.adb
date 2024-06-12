with Ada.Directories;

with Alire.Directories;
with Alire.Errors;
with Alire.Manifest;
with Alire.Root;

with GNAT.OS_Lib;

with Semantic_Versioning;

package body Alire.Roots.Optional is

   Root_Not_Detected : constant Root :=
                         (Alire.Outcome_Failure
                            ("Could not detect an " & Paths.Crate_File_Name
                             & " manifest at current or parent locations",
                             Report => False) with
                          Data => (Status => Outside));

   ----------------
   -- Brokenness --
   ----------------

   function Brokenness (This : Root) return String
   is (+This.Data.Cause);

   -----------------
   -- Detect_Root --
   -----------------

   function Detect_Root (Path : Any_Path) return Optional.Root is
      Crate_File : constant Any_Path := Crate_File_Name;
   begin
      if not GNAT.OS_Lib.Is_Directory (Path) then
         Trace.Debug
           ("No root can be detected because given path is not a directory: "
            & Path);
         return Root_Not_Detected;
      end if;

      declare
         Change_Dir : Directories.Guard (Directories.Enter (Path))
           with Unreferenced;
         --  We need to enter the folder with the possible crate, so stored
         --  relative paths (e.g. in pins) make sense when loaded.
      begin
         if Path /= "" then
            if GNAT.OS_Lib.Is_Regular_File (Crate_File) then
               begin
                  return This : constant Root :=
                    Outcome_Success
                      (Roots.New_Root
                         (R    => Releases.From_Manifest (Crate_File,
                                                          Manifest.Local,
                                                          Strict => True),
                          Path => Directories.Current,
                          Env  => Alire.Root.Platform_Properties))
                  do
                     --  Crate loaded properly, we can return a valid root here
                     Trace.Debug ("Valid root found at " & Path);
                  end return;
               exception
                  when E : others =>
                     Trace.Debug ("Unloadable root found at " & Path);
                     Log_Exception (E);
                     return Outcome_Failure
                       (Errors.Get (E),
                        Broken,
                        Report => False);
               end;
            else
               Trace.Debug ("No root found at " & Path);
               return Root_Not_Detected;
            end if;
         else
            Trace.Debug
              ("No root can be detected because given path is empty");
            return Root_Not_Detected;
            --  This happens when detection of a workspace in parent
            --  folders has been already attempted by the caller, so it
            --  ends calling here with an empty path.
         end if;
      end;
   end Detect_Root;

   -----------------
   -- Search_Root --
   -----------------

   function Search_Root (From : Any_Path) return Optional.Root
   is (Detect_Root
       (Directories.Detect_Root_Path
          (Ada.Directories.Full_Name (From))));

   ---------------
   -- Is_Broken --
   ---------------

   function Is_Broken (This : Root) return Boolean
   is (This.Status = Broken);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Root) return Boolean
   is (This.Status = Valid);

   -------------
   -- Outside --
   -------------

   function Outside (This : Root) return Boolean
   is (This.Status = Outside);

   ------------
   -- Status --
   ------------

   function Status (This : Root) return States
   is (This.Data.Status);

   -----------
   -- Value --
   -----------

   function Value (This : in out Root) return Reference
   is
   begin
      This.Assert;
      --  The following Unrestricted_Access cannot fail as we just asserted
      --  the value is stored.
      return Ref : constant Reference :=
        Reference'(Ptr => This.Data.Value'Unrestricted_Access);
   end Value;

   ---------------------
   -- Outcome_Failure --
   ---------------------

   function Outcome_Failure (Message : String;
                             Status  : States;
                             Report  : Boolean)
                             return Root
   is (if Status = Outside then
          (Alire.Outcome_Failure (Message, Report)
                  with Data => (Status => Outside))
       elsif Status = Broken then
          (Alire.Outcome_Failure (Message, Report)
           with Data => (Status => Broken,
                         Cause  => +Message))
       else
          raise Program_Error with "precondition not fulfilled");

   ---------------------
   -- Outcome_Success --
   ---------------------

   function Outcome_Success (This : Roots.Root) return Optional.Root
   is (Alire.Outcome_Success with
         Data   =>
           (Status => Valid,
            Value  => This));

   --------------------------
   -- Updatable_Dependency --
   --------------------------

   function Updatable_Dependency (This : in out Root)
                                  return Dependencies.Dependency
   is (Dependencies.New_Dependency
       (This.Value.Release.Element.Name,
          Semantic_Versioning.Updatable
            (This.Value.Release.Element.Version)));

end Alire.Roots.Optional;
