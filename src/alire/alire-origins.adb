with Alire.TOML_Adapters;
with Alire.TOML_Keys;

package body Alire.Origins is

   function Ends_With (S : String; Suffix : String) return Boolean is
     (S'Length >= Suffix'Length
      and then S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix);
   --  Return whether the S string ends with the given Suffix sub-string

   function URL_Basename (URL : Alire.URL) return String;
   --  Try to get a basename for the given URL. Return an empty string on
   --  failure.

   function Archive_Format (Name : String) return Source_Archive_Format;
   --  Guess the format of a source archive from its file name

   ------------------
   -- URL_Basename --
   ------------------

   function URL_Basename (URL : Alire.URL) return String is
      Separator : Positive;
      --  Index of the first URL separator we can find ('#' or '?') in URL

      Last_Slash : Natural;
      --  Index of the last slash character in URL before the first URL
      --  separator.
   begin
      Last_Slash := 0;
      Separator := URL'Last + 1;
      for I in URL'Range loop
         case URL (I) is
            when '?' | '#' =>
               Separator := I;
               exit;

            when '/' =>
               Last_Slash := I;

            when others =>
               null;
         end case;
      end loop;

      return URL (Last_Slash + 1 .. Separator);
   end URL_Basename;

   --------------------
   -- Archive_Format --
   --------------------

   function Archive_Format (Name : String) return Source_Archive_Format is
   begin
      if Ends_With (Name, ".zip") then
         return Zip_Archive;

      elsif Ends_With (Name, ".tar")
        or else Ends_With (Name, ".tar.gz")
        or else Ends_With (Name, ".tgz")
        or else Ends_With (Name, ".tar.bz2")
        or else Ends_With (Name, ".tbz2")
        or else Ends_With (Name, ".tar.xz")
      then
         return Tarball;

      else
         return Unknown;
      end if;
   end Archive_Format;

   ------------------------
   -- New_Source_Archive --
   ------------------------

   function New_Source_Archive
     (URL : Alire.URL; Name : String := "") return Origin
   is
      Archive_Name : constant String :=
        (if Name'Length = 0 then URL_Basename (URL) else Name);
      Format       : Source_Archive_Format;
   begin
      if Archive_Name'Length = 0 then
         raise Unknown_Source_Archive_Name_Error with
           "Unable to determine archive name: please specify one";
      end if;

      Format := Archive_Format (Archive_Name);
      if Format not in Known_Source_Archive_Format then
         raise Unknown_Source_Archive_Format_Error with
           "Unable to determine archive format from file extension";
      end if;

      return (Data => (Source_Archive, +URL, +Archive_Name, Format));
   end New_Source_Archive;

   -------------
   -- To_TOML --
   -------------

   overriding function To_TOML (This : Origin) return TOML.TOML_Value is
      use TOML_Adapters;
      Table : constant TOML.TOML_Value := TOML.Create_Table;

      function Prefix (Kind : VCS_Kinds) return String is
        (case Kind is
            when Git => "git",
            when Hg  => "hg",
            when SVN => "svn");

   begin
      case This.Kind is
         when Filesystem =>
            Table.Set (TOML_Keys.Origin, +("path:" & This.Path));
         when VCS_Kinds =>
            Table.Set (TOML_Keys.Origin, +(Prefix (This.Kind) & "+" & This.URL & "@" & This.Commit));
         when Native =>
            raise Program_Error with "native packages do not need to be exported";
         when Source_Archive =>
            Table.Set (TOML_Keys.Origin, +This.Archive_URL);
            if This.Archive_Name /= "" then
               Table.Set (TOML_Keys.Archive_Name, +This.Archive_Name);
            end if;
      end case;

      return Table;
   end To_TOML;

end Alire.Origins;
