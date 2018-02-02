with Alire.OS_Lib;

with Alr.OS_Lib;
with Alr.Utils;

package body Alr.Commands.Compile is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Execute;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute is
      use Alire.OS_Lib;

      Guard : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
   begin
      Requires_Project;
      Requires_Buildfile;

      if Alire.OS_Lib.Spawn ("gprbuild", "-j0 -p -P" & Project.GPR_Alr_File) = 0 then
         if OS_Lib.File_Contains_Ignore_Case (Project.GPR_File, "Library_Name") then
            Log ("Compilation finished without errors");
         else
            declare
               Execs : constant Utils.String_Vector := Os_Lib.Locate_File_Under (".", Project.Name, 2);
               --  FIXME: extension in non-linux platforms!
            begin
               case Execs.Length is
               when 0 =>
                  Log ("No executable found after compilation (might be too deep)", Verbose);
               when others =>
                  for Exe of Execs loop
                     Log ("Executable found at " & Utils.Quote (Execs.First_Element));
                  end loop;
               end case;
            end;
         end if;
      else
         Log ("alr detected a compilation failure");
      end if;
   end Execute;

end Alr.Commands.Compile;
