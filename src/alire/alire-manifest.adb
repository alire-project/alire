with Ada.Text_IO; use Ada.Text_IO;

with Alire.Directories;
with Alire.Errors;
with Alire.Releases;
with Alire.TOML_Keys;

package body Alire.Manifest is

   Warning : constant String := "  # This line was added by `alr with`";

   ------------
   -- Append --
   ------------

   procedure Append (Name : Any_Path;
                     Deps : Dependencies.Containers.List) is
      Replacer : constant Directories.Replacer :=
                   Directories.New_Replacement (Name);
      File     : File_Type;
   begin
      if Deps.Is_Empty then
         return;
      end if;

      Open (File, Append_File, Replacer.Editable_Name);

      for Dep of Deps loop
         New_Line (File);
         Put_Line (File, "[[" & TOML_Keys.Depends_On & "]]" & Warning);
         Put_Line (File, Dep.Manifest_Image & Warning);
      end loop;

      Close (File);

      --  Attempt loading of the new file as a double check
      begin
         if Releases.From_Manifest (Replacer.Editable_Name).To_TOML.Is_Present
         then
            Trace.Debug ("Manifest updated successfully");
         end if;
      exception
         when E : others =>
            Trace.Debug ("Exception caused by alr-modified manifest:");
            Log_Exception (E);
            raise Program_Error
              with Errors.Set ("Addition of dependencies to manifest failed");
      end;

      Replacer.Replace; -- All went well, keep the changes
   exception
      when E : others =>
         Trace.Debug ("Exception attempting to append dependencies:");
         Alire.Log_Exception (E);

         if Is_Open (File) then
            Close (File);
         end if;

         raise; -- Let it be processed upwards, if necessary
   end Append;

end Alire.Manifest;
