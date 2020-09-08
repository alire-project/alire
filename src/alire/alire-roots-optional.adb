with Ada.Directories;

with Alire.Directories;
with Alire.Errors;
with Alire.Manifest;
with Alire.Root;

with GNAT.OS_Lib;

package body Alire.Roots.Optional is

   Root_Not_Detected : constant Root :=
                         (Alire.Outcome_Failure
                            ("Could not detect a session folder"
                             & " at current or parent locations",
                             Report => False) with
                          Status => Outside);

   -----------------
   -- Detect_Root --
   -----------------

   function Detect_Root (Path : Any_Path) return Optional.Root is
      use Directories.Operators;
      Crate_File : constant Any_Path := Path / Crate_File_Name;
   begin
      if Path /= "" then
         if GNAT.OS_Lib.Is_Regular_File (Crate_File) then
            begin
               return This : constant Root :=
                 Outcome_Success
                   (Roots.New_Root
                      (R    => Releases.From_Manifest (Crate_File,
                                                       Manifest.Local),
                       Path => Ada.Directories.Full_Name (Path),
                       Env  => Alire.Root.Platform_Properties))
               do
                  --  Crate loaded properly, we can return a valid root here
                  Trace.Debug ("Valid root found at " & Path);
               end return;
            exception
               when E : others =>
                  Trace.Debug ("Unloadable root found at " & Path);
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
         Trace.Debug ("No root can be detected because given path is empty");
         return Root_Not_Detected;
         --  This happens when detection of session folders in parent folders
         --  has been already attempted by the caller, so it ends calling here
         --  with an empty path.
      end if;
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
   is (This.Status);

   -----------
   -- Value --
   -----------

   function Value (This : aliased Root) return Reference
   is
   begin
      This.Assert;
      return Reference'(Ptr => This.Value'Access);
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
                  with Status => Outside)
       elsif Status = Broken then
          (Alire.Outcome_Failure (Message, Report)
                  with Status => Broken)
       else
          raise Program_Error with "precondition not fulfilled");

   ---------------------
   -- Outcome_Success --
   ---------------------

   function Outcome_Success (This : Roots.Root) return Optional.Root
   is (Alire.Outcome_Success with
       Status => Valid,
       Value  => This);

end Alire.Roots.Optional;
