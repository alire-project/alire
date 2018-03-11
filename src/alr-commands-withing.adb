with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alire.Index;
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

      if Alire.Index.Exists (Argument (1)) then
         Put_Line (With_Line (Alire.Index.Value (Argument (1))));
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

   function Locate_Package (Name : Alire.Projects.Names) return String is

      use Ada.Directories;
      use Alire.Utils;

      Nameimg : constant String := Image (Name);

      Found : Unbounded_String := Null_Unbounded_String;
      --  The project-specific part of the filename

      Strict : Boolean := True;
      --  First pass we look for the exact name,
      --  second pass we look for a prefix

      ------------
      -- Verify --
      ------------

      function Verify (File : String) return Boolean is (True);
      --  FIXME: use libadaland or a tokenizer or whatever to ensure that
      --  a likely file indeed contains the declaration of a release for the
      --  given project

      -------------
      -- Matches --
      -------------

      procedure Matches (File : Directory_Entry_Type; Stop : in out Boolean) is
         Filename : constant String := Simple_Name (File);
      begin
         if Kind (File) /= Ordinary_File then
            return;
         end if;

         if Strict then
            if Contains (Filename, "-" & Nameimg & ".ads") and then Verify (Full_Name (File)) then
               Stop  := True;
               Found := To_Unbounded_String (Nameimg);
            end if;
         else
            if Contains (Filename, "alire-index-") and then Contains (Filename, ".ads") then
               declare
                  Project : constant String := Head (Tail (Tail (Filename, '-'), '-'), '.');
               begin
                  if Nameimg'Length > Project'Length and then
                    Nameimg (Nameimg'First .. Nameimg'First + Project'Length - 1) = Project and then
                    Verify (Full_Name (File))
                  then
                     --  It's a prefix, we'll take it (!)
                     Stop := True;
                     Found := To_Unbounded_String (Project);
                  end if;
               end;
            end if;
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

      --  Look for a matching prefix
      Strict := False;
      OS_Lib.Traverse_Folder (Hardcoded.Alr_Index_Folder_Absolute,
                              Matches'Access,
                              Recurse => True);

      if Found /= Null_Unbounded_String then
         return "Alire.Index." & Utils.To_Mixed_Case (To_String (Found));
      end if;

      raise Constraint_Error with "Couldn't find index file for " & Nameimg;
   end Locate_Package;

   ---------------
   -- With_Line --
   ---------------

   function With_Line (Name : Alire.Projects.Names) return String is
      ("with " & Locate_Package (Name) & ";");

end Alr.Commands.Withing;
