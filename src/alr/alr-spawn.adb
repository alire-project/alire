with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Root;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alr.Spawn is

   -------------------
   -- Warn_Outdated --
   -------------------

   procedure Warn_Outdated is
   begin
      Log ("Alr executable not found at expected location.");
      Log ("This may happen if a self-compilation failed.");
      Log ("");

      Log ("You can try the following action to recover:");

      if Root.Is_Empty then
         Log ("No project is known to this alr instance.");
         Log ("Please run 'alr update -o' outside any alr project.");
      else
         Log ("The last used project was: " & (+Root.Project));
         Log ("Please check its metadata file: " & Hardcoded.Working_Deps_File);
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
      if Is_Executable_File (Hardcoded.Alr_Rolling_Exec) then
         OS_Lib.Spawn_Raw (Hardcoded.Alr_Rolling_Exec,
                           Commands.Image (Cmd) & " " & Extra_Switches & " " & Args);
      else
         Warn_Outdated;
         raise Child_Failed;
      end if;
   end Alr;

   --------------------------------
   -- Session_Alr_Without_Return --
   --------------------------------

   procedure Session_Alr_Without_Return is
   begin
      if Is_Executable_File (Hardcoded.Alr_Session_Exec) then
         Trace.Detail (":::");
         begin
            Setenv (Hardcoded.Alr_Child_Flag, "TRUE");
            OS_Lib.Spawn_Raw (Hardcoded.Alr_Session_Exec, OS_Lib.Current_Command_Line);
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
         Trace.Debug ("Session executable not found");
         raise Child_Failed;
      end if;
   end Session_Alr_Without_Return;

   -----------------
   -- Updated_Alr --
   -----------------

   procedure Updated_Alr_Without_Return is
   begin
      if Is_Executable_File (Hardcoded.Alr_Rolling_Exec) then
         Trace.Detail ("...");
         begin
            Setenv (Hardcoded.Alr_Child_Flag, "TRUE");
            OS_Lib.Spawn_Raw (Hardcoded.Alr_Rolling_Exec, OS_Lib.Current_Command_Line);
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
         Trace.Debug ("Rolling executable not found");
         raise Child_Failed;
      end if;
   end Updated_Alr_Without_Return;

   -------------
   -- Command --
   -------------

   procedure Command (Cmd                 : String;
                      Args                : String := "";
                      Understands_Verbose : Boolean := False;
                      Force_Quiet         : Boolean := False) is
   begin
      if OS_Lib.Spawn (Cmd,
                       Args,
                       Understands_Verbose,
                       Force_Quiet) /= 0
      then
         raise Child_Failed;
      end if;
   end Command;

   --------------
   -- Gprbuild --
   --------------

   procedure Gprbuild (Project_File        : String;
                       Session_Build       : Boolean;
                       Session_Path        : String := "";
                       Extra_Args          : String := "")
   is
      Selfbuild : constant String :=
                    (if Session_Path /= ""
                     then "-XALR_SESSION=" & Session_Path & " -XALR_SELFBUILD=True "
                     else "") &
                    (if Session_Build
                     then "-XALR_BIN=" & Session_Path
                     else "-XALR_BIN=bin") & " ";
   begin
      Setenv (Hardcoded.Alr_Child_Flag, "TRUE");
      Command ("gprbuild",
               Selfbuild &
                 "-gnatwU -j0 -p " & -- Supress warnings on unused (may happen in prj_alr.ads)
                 Extra_Args & (if Extra_Args /= "" then " " else "") &
                 "-P " & Project_File,
               Understands_Verbose => True);
   end Gprbuild;

end Alr.Spawn;
