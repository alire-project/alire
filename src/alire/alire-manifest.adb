with Ada.Text_IO; use Ada.Text_IO;

with Alire.Config;
with Alire.Directories;
with Alire.Errors;
with Alire.Releases;
with Alire.TOML_Keys;
with Alire.Utils.Text_Files;

with GNATCOLL.Email.Utils;

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
      if not Is_Valid (Replacer.Editable_Name, Local) then
         raise Program_Error
           with Errors.Set ("Addition of dependencies to manifest failed");
      end if;

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

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Name : Any_Path; Source : Sources) return Boolean is
   begin
      --  Check we are able to load the manifest file
      if Releases.From_Manifest (Name, Source).Version_Image /= "" then
         Trace.Debug ("Checked valid manifest at " & Name);
         return True;
      else
         raise Program_Error with "A release will always have a version";
         return False;
      end if;
   exception
      when E : others =>
         Trace.Debug ("Exception trying to load manifest:");
         Log_Exception (E);
         return False;
   end Is_Valid;

   ------------
   -- Remove --
   ------------

   procedure Remove (Name : Any_Path;
                     Deps : Dependencies.Containers.List)
   is

      ------------
      -- Remove --
      ------------

      procedure Remove (Dep   : Dependencies.Dependency;
                        Lines : in out Utils.String_Vector)
      --  Remove given Dep from Lines, or warn if impossible
      is
         Enter_Marker : constant String := "[[" & TOML_Keys.Depends_On & "]]";
         --  We must see a line like this before being able to remove a dep.

         Target       : constant String := (+Dep.Crate) & "=""";
         --  A line starting with Target is a candidate for deletion

         Armed        : Boolean := False;
         --  True when we are inside [[depends-on]]

         Found        : Boolean := False; -- True when the dependency was found

         use Utils;

         -------------------
         -- Remove_Target --
         -------------------

         procedure Remove_Target is
         begin
            for I in Lines.First_Index .. Lines.Last_Index loop
               declare
                  Line  : constant String := Replace (Lines (I), " ", "");
               begin

                  if Armed and then Starts_With (Line, Target) then
                     --  Remove the target dependency
                     Trace.Debug ("Dependency to remove found at manifest line"
                                  & I'Img);
                     Found := True;
                     Lines.Delete (I);
                     exit;

                  elsif Starts_With (Line, "[[") then
                     --  Detect a plain [[depends-on]] with optional comment
                     Armed :=
                       Line = Enter_Marker or else
                       Starts_With (Line, Enter_Marker & '#');

                  elsif Armed and then Line /= "" and then
                    Line (Line'First) /= '[' -- not a table or different array
                  then
                     --  We are seeing a different dependency in the same array
                     --  entry; we can still remove our target if later found
                     --  in this entry. This can happen if the user edited and
                     --  reused a previous [[depends-on]] added by `alr with`.
                     null;

                  elsif Line = "" or else Starts_With (Line, "#") then
                     --  We still can remove a dependency found in this context
                     null;

                  else
                     --  Any other sighting complicates matters and we won't
                     --  touch it.
                     Armed := False;
                  end if;
               end;
            end loop;

            if not Found then
               Raise_Checked_Error
                 ("Could not find dependency in manifest: " & Dep.TTY_Image);
               return;
            end if;
         end Remove_Target;

         -----------------------
         -- Remove_Empty_Deps --
         -----------------------

         procedure Remove_Empty_Deps is
            --  This might probably be done with multiline regular expressions

            Deletable : Natural := 0;
            --  Tracks how many empty lines we have seen since the last [[

            Can_Delete : Boolean := True;
            --  We can delete as long as we are only seeing empty lines
         begin

            --  Traverse lines backwards

            for I in reverse Lines.First_Index .. Lines.Last_Index loop
               declare
                  Line : constant String := Replace (Lines (I), " ", "");
               begin
                  if Can_Delete then
                     --  Look for empty lines or the opening [[depends-on]]
                     if Line = "" then
                        Deletable := Deletable + 1;

                     elsif
                       Line = Enter_Marker or else
                       Starts_With (Line, Enter_Marker & '#')
                     then
                        --  Now we can delete the empty [[depends-on]] plus any
                        --  following empty lines.
                        for J in 0 .. Deletable loop -- 0 for the current line
                           Trace.Debug ("Removing meaningless manifest line: "
                                        & Lines (I));
                           Lines.Delete (I);
                        end loop;

                        --  Restart, we can still delete previous entries
                        Deletable := 0;

                     else
                        --  We found something else, so do not remove entry
                        Can_Delete := False;
                        Deletable  := 0;
                     end if;

                  else
                     --  Look for a [[ that starts another array entry. We
                     --  cannot rely on simply [ for tables, these could be
                     --  nested array tables.
                     if Starts_With (Line, "[[") then
                        Can_Delete := True;
                        Deletable  := 0;
                        --  We will look in next iterations for a precedent
                        --  empty array entry.
                     end if;
                  end if;
               end;
            end loop;
         end Remove_Empty_Deps;

      begin

         --  First pass, remove a detected dependency in the proper location.
         --  Note that this only removes the dependency line, but not the
         --  enclosing [[depends-on]]. It is ok to have such an empty array
         --  entry. Empty array entries are cleaned up afterwards.

         Remove_Target;

         --  Second pass, remove empty [[depends-on]] array entries. This
         --  ensures that trivial add/remove of dependencies cannot grow
         --  the file indefinitely with empty [[]] entries.

         Remove_Empty_Deps;

      end Remove;

      Replacer : constant Directories.Replacer :=
                   Directories.New_Replacement (Name);
   begin
      if Deps.Is_Empty then
         return;
      end if;

      declare
         File : constant Utils.Text_Files.File :=
                  Utils.Text_Files.Load (Replacer.Editable_Name,
                                         Backup => False);
                                         -- Replacer takes care of backup
      begin
         for Dep of Deps loop
            Remove (Dep, File.Lines.all);
         end loop;
      end;

      --  Attempt loading of the new file as a double check. This should never
      --  fail because we won't touch anything that's clearly removable.
      if not Is_Valid (Replacer.Editable_Name, Local) then
         raise Program_Error
           with Errors.Set ("Removal of dependencies in manifest failed");
      end if;

      Replacer.Replace; -- All went well, keep the changes
   end Remove;

   ---------------------
   -- Replace_Private --
   ---------------------

   Private_Begin : constant String :=
                     "# ALIRE PRIVATE CONTENTS BEGIN."
                     & " DO NOT EDIT BEYOND THIS LINE.";
   Private_End   : constant String :=
                     "# ALIRE PRIVATE CONTENTS END.";

   procedure Replace_Private (Name : Any_Path; Data : TOML.TOML_Value) is
      use Utils;
      File : Utils.Text_Files.File := Utils.Text_Files.Load (Name);
      Priv : constant TOML.TOML_Value := TOML.Create_Table;
   begin
      Outer : for I in File.Lines.First_Index .. File.Lines.Last_Index loop
         if File.Lines.Element (I) = Private_Begin or else
           Starts_With (Replace (File.Lines.all (I), " ", ""),
                        "[" & TOML_Keys.Privat & "]")
         then
            Trace.Debug ("Found private part in manifest, replacing it");

            for J in I .. File.Lines.Last_Index loop
               if File.Lines.Element (I) /= Private_End then
                  File.Lines.Delete (I);
               else
                  File.Lines.Delete (I);
                  exit Outer;
               end if;
            end loop;
         end if;
      end loop Outer;

      File.Lines.Append ("");

      --  Append the new TOML-formatted text. TOML-Ada uses LF to break lines.

      File.Lines.Append (Private_Begin);
      File.Lines.Append ("");

      if Config.Get (Config.Builtin_Keys.Private_Encode, True) then
         --  Manually dump the base64-encoded data
         File.Lines.Append (String'("[" & TOML_Keys.Privat & "]"));
         File.Lines.Append ("data="""""""); -- multiline string, """
         declare
            use GNATCOLL.Email.Utils;
            Encoded : UString;
         begin
            Base64_Encode (Str           => TOML.Dump_As_String (Data),
                           Charset       => "",
                           Result        => Encoded);
            File.Lines.Append (+Encoded);
         end;
         File.Lines.Append (""""""""); -- end multiline string, """
         File.Lines.Append ("");
      else
         Priv.Set (TOML_Keys.Privat, Data);
         File.Lines.Append
           (String_Vector'(Split (TOML.Dump_As_String (Priv), ASCII.LF)));
      end if;

      File.Lines.Append (Private_End);

   end Replace_Private;

end Alire.Manifest;
