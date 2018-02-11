with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Alire.Index;
with Alire.Repositories.Apt;
with Alr.Hardcoded;

with GNAT.OS_Lib;

with Semantic_Versioning;

package body Alr.Native is

   procedure Autodetect (Force : Boolean := False) is null;

   ------------------
   -- Add_To_Index --
   ------------------

   procedure Add_To_Index is
      use Ada.Text_IO;

      package Semver renames Semantic_Versioning;

      File : File_Type;
   begin
      if GNAT.OS_Lib.Is_Regular_File (Hardcoded.Native_Package_List) then
         Trace.Debug ("Parsing " & Hardcoded.Native_Package_List & " for native packages...");

         Open (File, In_File, Hardcoded.Native_Package_List);

         while not End_Of_File (File) loop
            declare
               Line : constant String := Get_Line (File);

               -- Fields are: package project version

               First_Space  : constant Natural := Ada.Strings.Fixed.Index (Line, " ");
               Second_Space : constant Natural := Ada.Strings.Fixed.Index (Line, " ", First_Space + 1);
            begin
               if Second_Space > 0 then
                  -- Trace.Debug ("Parsing version " & Line (Second_Space + 1 .. Line'Last));
                  declare
                     Prj : constant String := Line (First_Space + 1 .. Second_Space - 1);
                     Pkg : constant String := Line (Line'First .. First_Space - 1);
                     Ver : constant String := Line (Second_Space + 1 .. Line'Last);

                     R : constant Alire.Index.Release :=
                           Alire.Index.Register (Prj,
                                                 Semver.Relaxed (Ver),
                                                 Alire.Repositories.Apt.Repo,
                                                 Pkg,
                                                 Native => True) with Unreferenced;
                  begin
                     null;
--                       Trace.Debug ("Native release registered: " & R.Milestone_Image &
--                                      " found in package " & Pkg);
                  end;
               else
                  Trace.Warning ("Bad line in native package list: " & Line);
               end if;
            end;
         end loop;

         Close (File);
      end if;
   end Add_To_Index;

end Alr.Native;
