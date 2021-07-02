with Ada.Directories;

with Alire.Directories;
with Alire.Paths;
with Alire.TTY;
with Alire.Utils;

with Alr.Spawn;
with Alr.Platform;

package body Alr.Commands.Clean is

   -----------------------
   -- Delete_Temp_Files --
   -----------------------

   procedure Delete_Temp_Files is

      ------------
      -- Delete --
      ------------

      procedure Delete (Path : String)
      is
      begin
         Trace.Detail ("Deleting " & Alire.TTY.URL (Path));
         Alire.Directories.Force_Delete (Path);
      end Delete;

      Targets : Alire.Utils.String_Set;

      ----------------
      -- Add_Target --
      ----------------

      procedure Add_Target (Item        : Ada.Directories.Directory_Entry_Type;
                            Unused_Stop : in out Boolean)
      is
         use Ada.Directories;
         use Alire.Utils;
         Name : constant String := Simple_Name (Item);
      begin
         if Starts_With (Name, "alr-") and then Ends_With (Name, ".tmp") then
            Targets.Include (Ada.Directories.Full_Name (Item));
         end if;
      end Add_Target;

      package TTY renames Alire.TTY;
   begin
      Alire.Directories.Traverse_Tree
        (Start   => ".",
         Doing   => Add_Target'Access,
         Recurse => True);

      for Target of Targets loop
         Delete (Target);
      end loop;

      if Targets.Is_Empty then
         Trace.Info ("No temporaries found.");
      elsif Targets.Length in 1 then
         Trace.Info ("Deleted " & TTY.Emph ("1") & " temporary.");
      else
         Trace.Info ("Deleted" & TTY.Emph (Targets.Length'Image)
                     & " temporaries.");
      end if;
   end Delete_Temp_Files;

   ----------------
   -- Find_Cache --
   ----------------
   --  Return the cache dir, or "" if not found
   function Find_Cache return String is
      use Ada.Directories;
      use Alire.Directories.Operators;
      Root : constant String := Alire.Directories.Detect_Root_Path;
   begin
      if Root /= "" then
         if Exists (Root
                    / Alire.Paths.Working_Folder_Inside_Root
                    / Alire.Paths.Cache_Folder_Inside_Working_Folder)
         then
            return
              Root
                / Alire.Paths.Working_Folder_Inside_Root
                / Alire.Paths.Cache_Folder_Inside_Working_Folder;
         end if;
      end if;

      return "";
   end Find_Cache;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command) is
      use Alire.Utils;
   begin

      if not (Cmd.Cache or else Cmd.Temp) then
         Cmd.Requires_Valid_Session;
         Cmd.Root.Export_Build_Environment;

         Trace.Detail ("Cleaning project and dependencies...");

         --  Clean all the project files
         for Gpr_File of Cmd.Root.Release.Project_Files
           (Platform.Properties, With_Path => True)
         loop

            Spawn.Command ("gprclean",
                           Empty_Vector &
                             "-r" &
                             "-P" & Gpr_File &
                             Scenario.As_Command_Line,
                           Understands_Verbose => True);
         end loop;

         return;
      end if;

      if Cmd.Cache then

         --  We do not want to use Cmd.Root here, as it will check for a valid
         --  root, in turn deploying any missing dependencies (which we want to
         --  delete). This might result in that running two `alr clean --cache`
         --  in a row would redownload everything, and delete it again. So we
         --  go lower level and use more basic parts of Alire.

         declare
            Cache_Dir : constant String := Find_Cache;
         begin
            if Cache_Dir /= "" then
               Trace.Detail ("Deleting working copy cache...");
               Alire.Directories.Force_Delete (Cache_Dir);
               Trace.Info ("Cache folder deleted.");
            else
               Trace.Info ("Cache folder not present.");
               --  This is expected if the crate has no dependencies
            end if;
         end;
      end if;

      if Cmd.Temp then
         Delete_Temp_Files;
      end if;

   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("no options:")
      .Append ("   gprclean -r will be called to clean up the"
               & " build environment.")
      .New_Line
      .Append ("--cache:")
      .Append ("   All downloaded dependencies will be deleted.")
      .New_Line
      .Append ("--temp:")
      .Append ("   All alr-???.tmp files in the subtree will be deleted."
               & " These files may remain when alr is interrupted via"
               & " Ctrl-C or other forceful means.")
     );

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Cache'Access,
                     Long_Switch => "--cache",
                     Help        => "Delete cache of releases");
      Define_Switch (Config,
                     Cmd.Temp'Access,
                     Long_Switch => "--temp",
                     Help        => "Delete dangling temporary files");
   end Setup_Switches;

end Alr.Commands.Clean;
