with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Project;
with Alr.Utils;

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
      Extra_Switches : constant String :=
                         (if Args = OS_Lib.Current_Command_Line
                          then ""
                          else Commands.Global_Switches);
   begin
      if Is_Executable_File (Hardcoded.Alr_Exe_File) then
         Command (Hardcoded.Alr_Exe_File,
                  Commands.Image (Cmd) & " " & Extra_Switches & " " & Args);
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
         Trace.Detail ("...");
         begin
            OS_Lib.Spawn_Raw (Hardcoded.Alr_Exe_File, OS_Lib.Current_Command_Line);
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

   procedure Command (Cmd                 : String;
                      Args                : String := "";
                      Understands_Verbose : Boolean := False;
                      Force_Quiet         : Boolean := False;
                      Summary             : String  := "") is
   begin
      if OS_Lib.Spawn (Cmd,
                       Args,
                       Understands_Verbose,
                       Force_Quiet,
                       Summary) /= 0
      then
         raise Command_Failed;
      end if;
   end Command;

   --------------
   -- Gprbuild --
   --------------

   procedure Gprbuild (Project_File        : String;
                       Session_File        : String := "")
   is
      Selfbuild : constant String :=
                    (if Session_File /= ""
                     then "-XALR_SESSION=" & Session_File & " -XALR_SELFBUILD=True "
                     else "");
      Summary   : constant String :=
                    (if Utils.Contains (Project_File, "alr_env") then
                       (if Session_File /= "" then "metadata compiled" else "alr updated")
                     else
                        "project compiled");
   begin
      Command ("gprbuild",
               Selfbuild &
               "-j0 -m -p -P " & Project_File,
               Understands_Verbose => True,
               Summary => Summary);
   end Gprbuild;

end Alr.Spawn;
