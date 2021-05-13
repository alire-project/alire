with Alire.Origins;

with TOML;

package body Alire.User_Pins is

   package Keys is
      Commit : constant String := "commit";
      Path   : constant String := "path";
      URL    : constant String := "url";
   end Keys;

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
         if This.Contains (Keys.Path) then
            return Result : constant Pin :=
              (Kind => To_Path,
               Path => +This.Checked_Pop (Keys.Path, TOML_String).As_String)
            do
               if not GNAT.OS_Lib.Is_Directory (+Result.Path) then
                  Raise_Checked_Error ("Pin path is not a valid directory: "
                                       & (+Result.Path));
               end if;
               This.Report_Extra_Keys;
            end return;

         elsif This.Contains (Keys.URL) then
            return Result : Pin :=
              (Kind   => To_Git,
               URL    => +This.Checked_Pop (Keys.URL, TOML_String).As_String,
               Commit => <>)
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
            Trace.Error ("Expecting a path or url pin, but got:");
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
            return From_Table (This);

         when others =>
            Raise_Checked_Error
              ("improper format for pin, string or table expected but got a "
               & This.Unwrap.Kind'Image);

      end case;

   end From_TOML;

end Alire.User_Pins;
