with Ada.Directories;

with Alire.Directories;
with Alire.Origins;
with Alire.VFS;

with TOML;

package body Alire.User_Pins is

   package Keys is
      Commit  : constant String := "commit";
      Path    : constant String := "path";
      URL     : constant String := "url";
      Version : constant String := "version";
   end Keys;

   ------------
   -- Deploy --
   ------------

   procedure Deploy (This   : in out Pin;
                     Under  : Any_Path;
                     Online : Boolean)
   is
   begin
      null;
   end Deploy;

   -------------------
   -- Relative_Path --
   -------------------

   function Relative_Path (This : Pin) return Any_Path
   is (Directories.Find_Relative_Path
       (Parent => Directories.Current,
        Child  => +This.Path));

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (This : TOML_Adapters.Key_Queue) return Pin is

      ----------------
      -- From_Table --
      ----------------

      function From_Table (This : TOML_Adapters.Key_Queue) return Pin is
         use TOML;
      begin
         if This.Contains (Keys.Version) then
            return Pin'
              (Kind    => To_Version,
               Version => Semantic_Versioning.Parse
                 (This.Checked_Pop (Keys.Version, TOML_String).As_String));

         elsif This.Contains (Keys.Path) then
            return Result : Pin :=
              (Kind => To_Path,
               Path => +This.Checked_Pop (Keys.Path, TOML_String).As_String)
            do
               This.Report_Extra_Keys;

               if not GNAT.OS_Lib.Is_Directory (+Result.Path) then
                  This.Checked_Error ("Pin path is not a valid directory: "
                                      & (+Result.Path));
               end if;

               --  Check that the path was stored in portable format.

               if not Check_Absolute_Path (+Result.Path) and then
                 not VFS.Is_Portable (+Result.Path)
               then
                  This.Recoverable_Error
                    ("Pin relative paths must use forward slashes "
                     & "to be portable");
               end if;

               --  Make the path absolute

               Result.Path :=
                 +Ada.Directories.Full_Name
                 (VFS.To_Native (Portable_Path (+Result.Path)));
            end return;

         elsif This.Contains (Keys.URL) then
            return Result : Pin :=
              (Kind       => To_Git,
               URL        => +This.Checked_Pop (Keys.URL,
                                                TOML_String).As_String,
               Commit     => <>,
               Local_Path => <>)
            do
               if This.Contains (Keys.Commit) then
                  Result.Commit :=
                    +This.Checked_Pop (Keys.Commit, TOML_String).As_String;
                  This.Assert (+Result.Commit in Origins.Git_Commit,
                               "invalid commit: " & (+Result.Commit));
               end if;

               This.Report_Extra_Keys;
            end return;

         else
            Trace.Error ("Unexpected key in pin, got:");
            This.Print;
            Raise_Checked_Error ("invalid pin description");
         end if;
      end From_Table;

   begin
      case This.Unwrap.Kind is
         when TOML.TOML_String =>
            return Pin'
              (Kind     => To_Version,
               Version  => Semantic_Versioning.Parse (This.Unwrap.As_String));

         when TOML.TOML_Table =>
            return Result : constant Pin := From_Table (This) do
               This.Report_Extra_Keys;
            end return;

         when others =>
            Raise_Checked_Error
              ("improper format for pin, string or table expected but got a "
               & This.Unwrap.Kind'Image);

      end case;
   exception
      when E : Semantic_Versioning.Malformed_Input =>
         Log_Exception (E);
         Raise_Checked_Error ("Malformed semantic version in pin");
   end From_TOML;

end Alire.User_Pins;
