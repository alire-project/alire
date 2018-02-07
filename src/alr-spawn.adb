with Alire.OS_Lib;

with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Project;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alr.Spawn is

   -------------------
   -- Warn_Outdated --
   -------------------

   procedure Warn_Outdated is
   begin
      Log ("Alr executable not found at canonical location.");
      Log ("This may happen if a self-compilation failed.");
      Log ("");

      Log ("You can try the following action to recover:");

      if Project.Current.Is_Empty then
         Log ("No project is known to this alr instance.");
         Log ("Please run 'alr update -o' outside any alr project.");
      else
         Log ("The last used project was: " & Project.Name);
         Log ("Please check its metadata file: " & Hardcoded.Alire_File (Project.Name));
      end if;
   end Warn_Outdated;

   ---------
   -- Alr --
   ---------

   procedure Alr (Cmd : Commands.Cmd_Names; Args : String := "") is
   begin
      if Is_Executable_File (Hardcoded.Alr_Exe_File) then
         Command (Hardcoded.Alr_Exe_File,
                  Commands.Image (Cmd) & " " &
                    Commands.Global_Switches & " " & Args);
      else
         Warn_Outdated;
         raise Command_Failed;
      end if;
   end Alr;

   -----------------
   -- Updated_Alr --
   -----------------

   procedure Updated_Alr_Without_Return is
   begin
      if Is_Executable_File (Hardcoded.Alr_Exe_File) then
         Log ("...");
         begin
            Spawn.Command (Hardcoded.Alr_Exe_File, OS_Lib.Current_Command_Line);
            Os_Lib.Bailout (0);
            raise Program_Error with "Unreachable"; -- Just to remove a warning on No_Return
         exception
            when others =>
               OS_Lib.Bailout (1);
               raise Program_Error with "Unreachable"; -- Just to remove a warning on No_Return
         end;
         -- NOTE: THIS IS THE END OF EXECUTION OF THE CALLING alr
      else
         Warn_Outdated;
         raise Command_Failed;
      end if;
   end Updated_Alr_Without_Return;

   -------------
   -- Command --
   -------------

   procedure Command (Cmd : String; Args : String := ""; Quiet : Boolean := Commands.Is_Quiet) is
   begin
      if Alire.OS_Lib.Spawn (Cmd,
                             (if Quiet then "-q " & Args else Args)) /= 0
      then
         raise Command_Failed;
      end if;
   end Command;

   --------------
   -- Gprbuild --
   --------------

   procedure Gprbuild (Project_File : String;
                       Session_File : String := "";
                       Output       : Gpr_Output := (if Commands.Is_Quiet then Quiet else Raw))
   is
      Selfbuild : constant String :=
                    (if Session_File /= ""
                     then "-XALR_SESSION=" & Session_File & " -XALR_SELFBUILD=True "
                     else "");
   begin
      Command ("gprbuild",
               Selfbuild &
               "-j0 -m -p -P " & Project_File,
               Quiet => (if Output = Quiet then True else False));
   end Gprbuild;

end Alr.Spawn;
