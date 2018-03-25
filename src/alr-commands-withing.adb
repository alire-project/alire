with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alire.Index;
with Alire.Projects;
with Alire.Utils;

with Alr.Hardcoded;
with Alr.OS_Lib;

package body Alr.Commands.Withing is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      if Num_Arguments /= 1 then
         Reportaise_Wrong_Arguments ("alr with requires exactly one project name");
      end if;

      if Alire.Projects.Descriptions.Contains (Alire.Project (Argument (1))) then
         Put_Line (With_Line (Alire.Project (Argument (1))));
      else
         Reportaise_Command_Failed ("Project [" & Argument (1) & "] is not in Alire catalog");
      end if;
   exception
      when Constraint_Error =>
         Reportaise_Command_Failed ("Could not locate package containing releases of " & Argument (1));
   end Execute;

   --------------------
   -- Locate_Package --
   --------------------

   function Locate_Package (Name : Alire.Project) return String is

      use Ada.Directories;
      use Alire.Utils;

      Nameimg : constant String  := +Name;

      Found   : Unbounded_String := Null_Unbounded_String;

      -------------
      -- Matches --
      -------------

      procedure Matches (File : Directory_Entry_Type; Stop : in out Boolean) is
         Filename : constant String := Simple_Name (File);
      begin
         if Kind (File) /= Ordinary_File then
            return;
         end if;

         if Contains (Filename, "-" & Nameimg & ".ads") then
            Stop  := True;
            Found := To_Unbounded_String (Nameimg);
         end if;
      end Matches;

      use OS_Lib.Paths;

   begin
      --  Dummy first attempt
      if Exists (Hardcoded.Alr_Index_Folder_Absolute / "alire-index-" & Nameimg & ".ads") then
         return "Alire.Index." & Utils.To_Mixed_Case (Nameimg);
      end if;

      --  Look for exact name in subfolders
      OS_Lib.Traverse_Folder (Hardcoded.Alr_Index_Folder_Absolute,
                              Matches'Access,
                              Recurse => True);

      if Found /= Null_Unbounded_String then
         return "Alire.Index." & Utils.To_Mixed_Case (To_String (Found));
      end if;

      --  No evident file contains the project, so we may need the full catalog
      if Alire.Index.Is_Currently_Indexed (Name) then
         return "Alire.Index." & Alire.Index.Get (Name).Package_Name;
      else
         Requires_Full_Index;
         raise Program_Error with ("Project not found despite index being complete: " & Nameimg);
      end if;
   end Locate_Package;

   ---------------
   -- With_Line --
   ---------------

   function With_Line (Name : Alire.Project) return String is
      ("with " & Locate_Package (Name) & ";");

end Alr.Commands.Withing;
