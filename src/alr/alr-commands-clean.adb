with Ada.Directories;

with Alire.Settings.Edit;
with Alire.Directories;
with Alire.Paths;
with Alire.Platforms.Current;
with Alire.Spawn;

package body Alr.Commands.Clean is

   -----------------------
   -- Delete_Temp_Files --
   -----------------------

   procedure Delete_Temp_Files is

      Freed : Ada.Directories.File_Size := 0;

      -----------------
      -- Freed_Image --
      -----------------

      function Freed_Image return String
      is ("freeing " & Alire.Directories.TTY_Image (Freed) & ".");

      ------------
      -- Delete --
      ------------

      procedure Delete (Path : Alire.Absolute_Path)
      is
         use type Ada.Directories.File_Size;
      begin
         Trace.Detail ("Deleting " & Alire.TTY.URL (Path));
         Freed := Freed + Alire.Directories.Tree_Size (Path);
         Alire.Directories.Force_Delete (Path);
      end Delete;

      Targets : AAA.Strings.Set;

      ----------------
      -- Add_Target --
      ----------------

      procedure Add_Target (Item        : Alire.Any_Path;
                            Unused_Stop : in out Boolean)
      is
         use Ada.Directories;
         use AAA.Strings;
         Name : constant String := Simple_Name (Item);
      begin
         if Has_Prefix (Name, "alr-") and then Has_Suffix (Name, ".tmp") then
            Targets.Include (Ada.Directories.Full_Name (Item));
         end if;
      end Add_Target;

   begin

      --  Current workspace

      Alire.Directories.Traverse_Tree
        (Start   => Alire.Directories.Current,
         Doing   => Add_Target'Access,
         Recurse => True);

      --  Configuration-wide cache, where interrupted binary downloads dwell...

      Alire.Directories.Traverse_Tree
        (Start   => Alire.Settings.Edit.Path,
         Doing   => Add_Target'Access,
         Recurse => True);

      for Target of Targets loop
         Delete (Target);
      end loop;

      if Targets.Is_Empty then
         Trace.Info ("No temporaries found.");
      elsif Targets.Length in 1 then
         Trace.Info ("Deleted " & TTY.Emph ("1") & " temporary, "
                     & Freed_Image);
      else
         Trace.Info ("Deleted" & TTY.Emph (Targets.Length'Image)
                     & " temporaries, " & Freed_Image);
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
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      use AAA.Strings;
   begin

      if not (Cmd.Cache or else Cmd.Temp) then
         Cmd.Requires_Workspace;
         Cmd.Root.Export_Build_Environment;

         Trace.Detail ("Cleaning project and dependencies...");

         --  Clean all the project files
         for Gpr_File of Cmd.Root.Release.Project_Files
           (Alire.Platforms.Current.Properties, With_Path => True)
         loop

            Alire.Spawn.Command ("gprclean",
                                 Empty_Vector &
                                   "-r" &
                                   "-P" & Gpr_File &
                                   Args,
                                 Understands_Verbose => True);
         end loop;

         return;
      end if;

      if Args.Count /= 0 then
         Reportaise_Wrong_Arguments (Cmd.Name & " doesn't take arguments");
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
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
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

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
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
