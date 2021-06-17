with Ada.Directories;

with Alire.Directories;
with Alire.Origins;
with Alire.Roots.Optional;
with Alire.Utils.TTY;
with Alire.Utils.User_Input;
with Alire.VCSs.Git;
with Alire.VFS;

with GNAT.OS_Lib;

package body Alire.User_Pins is

   package TTY renames Alire.Utils.TTY;

   package Keys is
      Branch   : constant String := "branch";
      Commit   : constant String := "commit";
      Internal : constant String := "lockfiled";
      Path     : constant String := "path";
      URL      : constant String := "url";
      Version  : constant String := "version";
   end Keys;

   ---------------
   -- Is_Broken --
   ---------------

   function Is_Broken (This : Pin) return Boolean
   is (not Ada.Directories.Exists (Path (This))
       or else Ada.Directories.Kind (Path (This))
               not in Ada.Directories.Directory);

   ------------
   -- Deploy --
   ------------

   procedure Deploy (This   : in out Pin;
                     Crate  : Crate_Name;
                     Under  : Any_Path;
                     Online : Boolean)
   is
      use Ada.Strings.Unbounded;
      use Directories.Operators;

      Folder : constant String :=
                 (+Crate)
               & (if This.Is_Remote and then This.Commit /= ""
                  then "_" & Origins.Short_Commit (+This.Commit)
                  else "");

      Destination : constant String :=
                      Ada.Directories.Full_Name (Under / Folder);

      --------------
      -- Checkout --
      --------------

      procedure Checkout (Branch : String := ""; Commit : String := "")
        with Pre => not (Branch /= "" and then Commit /= "");
      --  Pass only a commit or a branch. If none, default remote head.

      procedure Checkout (Branch : String := "";
                          Commit : String := "")
      is
         package Adirs renames Ada.Directories;
         Temp : Directories.Temp_File;
      begin

         --  Check out the branch or commit

         if not
           VCSs.Git.Handler.Clone
             (From   => URL (This) & (if Commit /= ""
                                      then "#" & Commit
                                      else ""),
              Into   => Temp.Filename,
              Branch => Branch, -- May be empty for default branch
              Depth  => 1).Success
         then
            Raise_Checked_Error
              ("Checkout of repository at " & TTY.URL (URL (This))
               & " failed, re-run with -vv -d for details");
         end if;

         --  Successful checkout

         if not Adirs.Exists (Adirs.Containing_Directory (Destination)) then
            Adirs.Create_Path (Adirs.Containing_Directory (Destination));
         end if;
         Adirs.Rename (Temp.Filename, Destination);
         Temp.Keep;
      end Checkout;

      ------------
      -- Update --
      ------------

      procedure Update is
      begin
         Trace.Detail ("Checking out pin " & TTY.Name (Crate) & " at "
                       & TTY.URL (Destination));

         --  If the fetch URL has been changed, fall back to checkout

         if VCSs.Git.Handler.Fetch_URL
           (Repo   => Destination,
            Public => False) /= This.URL
         then
            Put_Info ("Switching pin " & TTY.Name (Crate) & " to origin at "
                      & TTY.URL (+This.URL));
            Ada.Directories.Delete_Tree (Destination);
            Checkout; -- Pending branch tracking implementation
            return;
         end if;

         --  Finally update

         if not VCSs.Git.Handler.Update (Destination).Success then
            Raise_Checked_Error
              ("Update of repository at " & TTY.URL (Destination)
               & " failed, re-run with -vv -d for details");
         end if;
      end Update;

   begin

      --  Check when to do nothing

      if not This.Is_Remote then
         return;
      end if;

      This.Local_Path := +Destination;

      --  Don't check out an already existing commit pin, or a non-update
      --  branch pin

      if Ada.Directories.Exists (Destination)
        and then
          (This.Commit /= "" or else not Online)
      then
         Trace.Debug ("Skipping deployment of already existing pin at "
                      & TTY.URL (Destination));
         return;
      end if;

      --  Check out a fixed commit, a branch, or update a branch are the three
      --  remaining possibilities.

      if This.Commit /= "" then
         Checkout (Commit => +This.Commit);

      elsif Ada.Directories.Exists (Destination) then
         Update;

      else
         Checkout;

      end if;

      --  At this point, we have the sources at Destination. Last checks ensue.

      declare
         Root : constant Roots.Optional.Root :=
                  Roots.Optional.Detect_Root (Destination);
      begin

         --  Check crate name mismatch

         if Root.Is_Valid and then
           Crate /= Root.Value.Release.Name
         then
            Raise_Checked_Error
              ("Requested and retrieved crates do not match: "
               & TTY.Name (Crate) & " /= "
               & TTY.Name (Root.Value.Release.Name));
         end if;

         --  Warn if raw project

         if not Root.Is_Valid then
            Put_Warning
              ("Pin for " & TTY.Name (Crate) & " does not contain an Alire "
               & "manifest. It will be used as a raw GNAT project.");
         end if;

      end;

   end Deploy;

   -------------------------
   -- TTY_URL_With_Commit --
   -------------------------

   function TTY_URL_With_Commit (This : Pin) return String
   is (TTY.URL (URL (This))
       & (if Commit (This).Has_Element
         then "#" & TTY.Emph (Commit (This).Element.Ptr.all)
         else ""));

   ----------
   -- Path --
   ----------

   function Path (This : Pin) return Absolute_Path
   is
      --  Having this as an expression function causes CE2021 to return a
      --  corrupted string some times.
   begin
      case This.Kind is
         when To_Path =>
            return +This.Path;
         when To_Git  =>
            if +This.Local_Path /= "" then
               return +This.Local_Path;
            else
               raise Program_Error with "Undeployed pin";
            end if;
         when others  =>
            raise Program_Error with "invalid pin kind";
      end case;
   end Path;

   -------------------
   -- Relative_Path --
   -------------------

   function Relative_Path (This : Pin; Color : Boolean := True) return String
   is
      Portable : constant String :=
                   VFS.Attempt_Portable
                     (Directories.Find_Relative_Path_To (Path (This)));
   begin
      if Color then
         return TTY.URL (Portable);
      else
         return Portable;
      end if;
   end Relative_Path;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (This : TOML_Adapters.Key_Queue) return Pin is

      ----------------
      -- From_Table --
      ----------------

      function From_Table (This : TOML_Adapters.Key_Queue) return Pin is
         use TOML;

         -------------------
         -- From_Lockfile --
         -------------------
         --  Special case loader for pins not described by the user, but stored
         --  by us in the lockfile. These already have a path for the pin.
         function From_Lockfile return Pin is
         begin
            This.Assert
              (This.Checked_Pop (Keys.Internal, TOML_Boolean).As_Boolean,
               "Boolean expected");

            if This.Contains (Keys.URL) then
               return Result : Pin := (Kind => To_Git, others => <>) do
                  Result.URL :=
                    +This.Checked_Pop (Keys.URL,
                                       TOML_String).As_String;

                  Result.Local_Path :=
                    +Utils.User_Input.To_Absolute_From_Portable
                    (This.Checked_Pop (Keys.Path, TOML_String).As_String);

                  if This.Contains (Keys.Commit) then
                     Result.Commit :=
                       +This.Checked_Pop (Keys.Commit, TOML_String).As_String;
                  end if;
               end return;
            else
               return Result : Pin := (Kind => To_Path, others => <>) do
                  Result.Path :=
                    +Utils.User_Input.To_Absolute_From_Portable
                    (This.Checked_Pop (Keys.Path, TOML_String).As_String);
               end return;
            end if;
         end From_Lockfile;

      begin
         if This.Contains (Keys.Internal) then
            return Result : constant Pin := From_Lockfile do
               This.Report_Extra_Keys;
            end return;

         elsif This.Contains (Keys.Version) then
            return Pin'
              (Kind    => To_Version,
               Version => Semantic_Versioning.Parse
                 (This.Checked_Pop (Keys.Version, TOML_String).As_String));

         elsif This.Contains (Keys.Path) then
            return Result : Pin :=
              (Kind => To_Path,
               Path => <>)
            do
               declare
                  User_Path : constant String :=
                                This.Checked_Pop (Keys.Path,
                                                  TOML_String).As_String;
               begin
                  This.Report_Extra_Keys;

                  --  Check that the path was stored in portable format or as
                  --  absolute path.

                  if not Check_Absolute_Path (User_Path) and then
                    not VFS.Is_Portable (User_Path)
                  then
                     This.Recoverable_Error
                       ("Pin relative paths must use forward slashes "
                        & "to be portable: " & Utils.TTY.URL (User_Path));
                  end if;

                  --  Make the path absolute if not already, and store it

                  Result.Path :=
                    +Utils.User_Input.To_Absolute_From_Portable
                    (User_Path                  => User_Path,
                     Error_When_Relative_Native =>
                       "Pin relative paths must use forward slashes " &
                       " to be portable");

                  if not GNAT.OS_Lib.Is_Directory (+Result.Path) then
                     This.Checked_Error ("Pin path is not a valid directory: "
                                         & (+Result.Path));
                  end if;
               end;
            end return;

         elsif This.Contains (Keys.URL) then
            return Result : Pin :=
              (Kind       => To_Git,
               URL        => +This.Checked_Pop (Keys.URL,
                                                TOML_String).As_String,
               Branch     => <>,
               Commit     => <>,
               Local_Path => <>)
            do
               if This.Contains (Keys.Branch)
                 and then This.Contains (Keys.Commit)
               then
                  This.Checked_Error
                    ("cannot specify both a branch and a commit");
               end if;

               --  TEST: simultaneous branch/commit

               if This.Contains (Keys.Commit) then
                  Result.Commit :=
                    +This.Checked_Pop (Keys.Commit, TOML_String).As_String;
                  This.Assert (+Result.Commit in Origins.Git_Commit,
                               "invalid commit: " & (+Result.Commit));
               elsif This.Contains (Keys.Branch) then
                     Result.Branch :=
                       +This.Checked_Pop (Keys.Branch, TOML_String).As_String;
                     This.Assert (+Result.Branch /= "",
                                  "branch cannot be the empty string");
               end if;

               --  TEST: empty branch value

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

   -------------
   -- To_TOML --
   -------------

   function To_TOML (This : Pin) return TOML.TOML_Value is
      use TOML;
      Table : constant TOML_Value := Create_Table;
   begin

      --  Pins going into the lockfile require all the information; we must
      --  also notify the loader not to report unexpected keys

      if This.Is_Remote then
         Table.Set (Keys.URL,
                    Create_String
                      (URL (This)));

         if Commit (This).Has_Element then
            Table.Set (Keys.Commit,
                       Create_String (Commit (This).Element.Ptr.all));
         end if;
      end if;

      Table.Set (Keys.Path,
                 Create_String (VFS.Attempt_Portable (Path (This))));

      Table.Set (Keys.Internal, Create_Boolean (True));

      return Table;
   end To_TOML;

end Alire.User_Pins;
