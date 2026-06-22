with Ada.Directories;

with AAA.Strings;

with Alire.Features;
with Alire.Loading;
with Alire.Origins.Deployers.System;
with Alire.Platforms.Current;
with Alire.Root;
with Alire.URI;
with Alire.Utils.TTY;
with Alire.VFS;

with Semantic_Versioning;

package body Alire.Origins is

   ----------
   -- Keys --
   ----------

   package Keys is -- TOML keys for serialization

      Archive_Name : constant String := "archive-name";
      Binary       : constant String := "binary";
      Commit       : constant String := "commit";
      Hashes       : constant String := "hashes";
      Origin       : constant String := "origin";
      Subdir       : constant String := "subdir";
      URL          : constant String := "url";

   end Keys;

   function URL_Basename (URL : Alire.URL) return String;
   --  Try to get a basename for the given URL. Return an empty string on
   --  failure.

   -------------
   -- As_Data --
   -------------

   function As_Data (This : Conditional_Archive) return Archive_Data'Class
   is
      --  Resolve the value that applies currently
      Evaluated : constant Conditional_Archive :=
                    This.Evaluate (Alire.Root.Platform_Properties);
   begin
      if Evaluated.Is_Empty then
         Raise_Checked_Error
           ("Binary archive is unavailable on current platform");
      else
         return Conditional_Archives.Tree (Evaluated).Value;
      end if;
   end As_Data;

   ------------------
   -- New_External --
   ------------------

   function New_External (Description : String) return Origin is
     (Data => (External,
               Description => +Description,
               Hashes      => <>));

   --------------------
   -- New_Filesystem --
   --------------------

   function New_Filesystem (Path : String) return Origin is
     (Data => (Filesystem,
               Path   => +Ada.Directories.Full_Name (Path),
               Hashes => <>));

   -------------
   -- New_Git --
   -------------

   function New_Git (URL    : Alire.URL;
                     Commit : Git_Commit;
                     Subdir : Relative_Path := "")
                     return Origin is
     (Data => (Git,
               Repo_URL => +URL,
               Commit   => +Commit,
               Hashes   => <>,
               Subdir   => +Subdir));

   ------------
   -- New_Hg --
   ------------

   function New_Hg (URL    : Alire.URL;
                    Commit : Hg_Commit;
                    Subdir : Relative_Path := "")
                    return Origin is
     (Data => (Hg,
               Repo_URL => +URL,
               Commit   => +Commit,
               Hashes   => <>,
               Subdir   => +Subdir));

   -------------
   -- New_SVN --
   -------------

   function New_SVN (URL    : Alire.URL;
                     Commit : String;
                     Subdir : Relative_Path := "") return Origin is
     (Data => (SVN,
               Repo_URL => +URL,
               Commit   => +Commit,
               Hashes   => <>,
               Subdir   => +Subdir));

   ----------------
   -- New_System --
   ----------------

   function New_System (System_Package_Name : String) return Origin is
     (Data => (System, Package_Name => +System_Package_Name, Hashes => <>));

   ----------
   -- Kind --
   ----------

   function Kind (This : Origin) return Kinds is (This.Data.Kind);

   ------------
   -- Subdir --
   ------------

   function Subdir (This : Origin) return Relative_Path
   is (+This.Data.Subdir);

   ---------
   -- URL --
   ---------

   function URL    (This : Origin) return Alire.URL is
     (Alire.URL (+This.Data.Repo_URL));

   ------------------
   -- Explicit_URL --
   ------------------

   function Explicit_URL (This : Origin) return Alire.URL
   is (URI.Make_VCS_Explicit
         (This.URL,
          (case This.Kind is
             when Git => URI.Git,
             when Hg => URI.Hg,
             when SVN => URI.SVN,
             when others => raise Program_Error with "unreachable case")));

   ------------
   -- Commit --
   ------------

   function Commit (This : Origin) return String is
     (+This.Data.Commit);

   -------------------------
   -- TTY_URL_With_Commit --
   -------------------------

   function TTY_URL_With_Commit (This : Origin) return String is
     (Utils.TTY.URL (This.URL) & "#" & TTY.Emph (This.Commit));

   ----------
   -- Path --
   ----------

   function Path (This : Origin) return String is (+This.Data.Path);

   -----------------
   -- Archive_URL --
   -----------------

   function Archive_URL (This : Origin) return Alire.URL is
     (if This.Kind in Source_Archive
      then +This.Data.Src_Archive.URL
      else +This.Data.Bin_Archive.As_Data.URL);

   ------------------
   -- Archive_Name --
   ------------------

   function Archive_Name (This : Origin) return String is
     (if This.Kind in Source_Archive
      then +This.Data.Src_Archive.Name
      else +This.Data.Bin_Archive.As_Data.Name);

   --------------------
   -- Archive_Format --
   --------------------

   function Archive_Format (This : Origin) return Known_Source_Archive_Format
   is (if This.Kind in Source_Archive
       then This.Data.Src_Archive.Format
       else This.Data.Bin_Archive.As_Data.Format);

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (This : Origin) return String is
     (+This.Data.Package_Name);

   -------------
   -- Get_URL --
   -------------

   function Get_URL (This : Origin) return Alire.URL
   is (case This.Kind is
          when Filesystem     => This.Path,
          when Source_Archive => This.Archive_URL,
          when VCS_Kinds      => This.Explicit_URL,
          when others         => raise Checked_Error with "Origin has no URL");

   --------------
   -- Add_Hash --
   --------------

   procedure Add_Hash (This : in out Origin;
                       Hash :        Hashes.Any_Hash) is
   begin
      case This.Kind is
         when Filesystem =>
            This.Data.Hashes.Append (Hash);
         when Binary_Archive =>
            --  This case should not happen, as publishing assistant doesn't
            --  work for conditional binary origins.
            raise Program_Error with Errors.Set
              ("Unintended use of Alire.Origins.Add_Hash");
         when Source_Archive =>
            This.Data.Src_Archive.Hashes.Append (Hash);
         when others =>
            Raise_Checked_Error ("Cannot add hash to origin kind "
                                 & This.Kind'Image);
      end case;
   end Add_Hash;

   ----------------
   -- Get_Hashes --
   ----------------

   function Get_Hashes (This : Origin) return Hash_Vectors.Vector
   is (case This.Kind is
          when Filesystem     => This.Data.Hashes,
          when Binary_Archive =>
            (if This.Is_Available (Platforms.Current.Properties)
             then This.Data.Bin_Archive.As_Data.Hashes
             else Hash_Vectors.Empty_Vector),
          when Source_Archive => This.Data.Src_Archive.Hashes,
          when others         => Hash_Vectors.Empty_Vector);

   ----------------
   -- Unique_Ids --
   ----------------

   function Unique_Ids (This : Origin) return AAA.Strings.Vector is
      Result : AAA.Strings.Vector;
   begin
      for Hash of Hash_Vectors.Vector'(This.Get_Hashes) loop
         Result.Append (String (Hash));
      end loop;
      return Result;
   end Unique_Ids;

   ----------------
   -- Add_Hashes --
   ----------------
   --  Load hash information into the given origin
   function Add_Hashes (This   : in out Hash_Vectors.Vector;
                        Parent : TOML_Adapters.Key_Queue) return Outcome is
      Val : TOML.TOML_Value;
   begin
      if Parent.Pop (Keys.Hashes, Val) then
         if Val.Kind /= TOML.TOML_Array then
            return Parent.Failure
              (Keys.Hashes & " must be an array of hash values");
         end if;

         for I in 1 .. Val.Length loop
            if Val.Item (I).Kind /= TOML.TOML_String then
               return Parent.Failure
                 ("hash must be a 'kind:digest' formatted string");
            end if;

            declare
               Hash : constant String := Val.Item (I).As_String;
            begin
               if not Hashes.Is_Well_Formed (Hash) then
                  return Parent.Failure
                    ("malformed or unknown hash: " & Hash);
               end if;

               This.Append (Hashes.Any_Hash (Hash));
            end;
         end loop;
      else
         return Parent.Failure
           ("missing mandatory " & Keys.Hashes & " field");
      end if;

      return Outcome_Success;
   end Add_Hashes;

   ------------------
   -- URL_Basename --
   ------------------

   function URL_Basename (URL : Alire.URL) return String is
      Separator : Positive := URL'Last + 1;
      --  Index of the first URL separator we can find ('#' or '?') in URL, or
      --  URL'Last + 1 if we haven't found any.

      Last_Slash : Natural := URL'First - 1;
      --  Index of the last slash character in URL before the first URL
      --  separator or URL'First - 1 if we haven't found any.
   begin
      for I in URL'Range loop
         case URL (I) is
            when '?' | '#' =>
               Separator := I;
               exit;

            when '/' | '\' =>
               Last_Slash := I;

            when others =>
               null;
         end case;
      end loop;

      return URL (Last_Slash + 1 .. Separator - 1);
   end URL_Basename;

   --------------------
   -- Archive_Format --
   --------------------

   function Archive_Format (Name : String) return Source_Archive_Format is
      use AAA.Strings;
   begin
      if Has_Suffix (Name, ".zip") then
         return Zip_Archive;

      elsif Has_Suffix (Name, ".tar")
        or else Has_Suffix (Name, ".tar.gz")
        or else Has_Suffix (Name, ".tgz")
        or else Has_Suffix (Name, ".tar.bz2")
        or else Has_Suffix (Name, ".tbz2")
        or else Has_Suffix (Name, ".tar.xz")
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
     (URL  : Alire.URL;
      Name : String := "") return Origin
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

      --  We add the "file:" to have a proper URI and simplify things for
      --  Windows absolute paths with drive letter.
      return (Data =>
                (Source_Archive,
                 Src_Archive =>
                   (URL    =>
                      +(if URI.URI_Kind (URL) in URI.Local_Other
                        then URI.To_URL
                          (Ada.Directories.Full_Name (URI.Local_Path (URL)))
                        else URL),
                    Name   => +Archive_Name,
                    Format => Format,
                    Binary => False,
                    Hashes => <>)));
   end New_Source_Archive;

   -------------
   -- New_VCS --
   -------------

   function New_VCS (URL    : Alire.URL;
                     Commit : String;
                     Subdir : Relative_Path := "") return Origin is
      URL_Kind : constant URI.URI_Kinds := URI.URI_Kind (URL);
      VCS_URL : constant String := VCSs.Repo_URL (URL);

   begin
      case URL_Kind is
         when URI.Git_URIs =>
            if not VCSs.Git.Is_Valid_Commit (Commit) then
               Raise_Checked_Error
                 ("invalid git commit id, " &
                    "40 digits hexadecimal expected");
            end if;
            return New_Git (VCS_URL, Commit, Subdir);
         when URI.Hg_URIs =>
            if not VCSs.Hg.Is_Valid_Commit (Commit) then
               Raise_Checked_Error
                 ("invalid mercurial commit id, " &
                    "40 digits hexadecimal expected");
            end if;
            return New_Hg (VCS_URL, Commit, Subdir);
         when URI.SVN_URIs =>
            return New_SVN (VCS_URL, Commit, Subdir);
         when URI.HTTP_Other | URI.SSH_Other =>
            Raise_Checked_Error ("ambiguous VCS URL: " & URL);
         when others =>
            Raise_Checked_Error ("unknown VCS URL: " & URL);
      end case;
   end New_VCS;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional_Archives.Tree
   is
      use TOML;
      Archive : TOML_Value;
      Table   : constant TOML_Adapters.Key_Queue :=
                  From.Descend (From.Checked_Pop (Keys.Origin, TOML_Table),
                                Context => "data");
   begin
      --  Optional filename checks:
      if Table.Pop (Keys.Archive_Name, Archive) then
         if Archive.Kind /= TOML.TOML_String then
            Table.Checked_Error ("archive name must be a string");
         end if;
      end if;

      declare
         Archive_Origin : Origin :=
                            New_Source_Archive
                              (URL  => Table.Checked_Pop
                                 (Keys.URL, TOML_String).As_String,
                               Name => (if Archive.Is_Present
                                        then Archive.As_String
                                        else ""));
      begin
         Add_Hashes (Archive_Origin.Data.Src_Archive.Hashes, Table).Assert;

         if Table.Unwrap.Has (Keys.Binary) then
            Archive_Origin.Data.Src_Archive.Binary :=
              Table.Checked_Pop (Keys.Binary, TOML_Boolean).As_Boolean;
         end if;

         Table.Report_Extra_Keys;

         --  Wrap as a conditional tree
         return Conditional_Archives.New_Leaf
           (Archive_Origin.Data.Src_Archive);
      end;
   exception
      when Unknown_Source_Archive_Name_Error =>
         Table.Checked_Error
           ("unable to determine archive name from URL: "
            & "please specify one with '"
            & Keys.Archive_Name & "'");
   end From_TOML;

   ----------------------
   -- Binary_From_TOML --
   ----------------------
   --  This wrapper is used to make sure that a conditional archive is
   --  explicitly marked as binary.
   function Binary_From_TOML (From : TOML_Adapters.Key_Queue)
                              return Conditional_Archives.Tree
   is
      use TOML;
      use type Semantic_Versioning.Version;
      Table   : constant TOML_Adapters.Key_Queue :=
                  From.Descend (From.Unwrap.Get (Keys.Origin),
                                Context => "binary_archive_data");
   begin
      if From.Metadata.Kind not in Loading.None
      --  When loading from an `alire.toml` manifest outside of an index,
      --  we won't have version metadata, but this is okay as it either is a
      --  user manifest (which does not contain an origin) or it is a manifest
      --  generated by ourselves for internal use (which always contains the
      --  binary property when needed).
        and then
          From.Metadata.Version >= Features.Index.Explicit_Binary_Origin
        and then
          (not Table.Unwrap.Has (Keys.Binary) or else
           not Table.Unwrap.Get (Keys.Binary).As_Boolean)
      then
         Raise_Checked_Error
           ("Dynamic origins must explicitly set the `binary=true` property"
            & " from index version "
            & TTY.Bold (Features.Index.Explicit_Binary_Origin.Image)
            & " onwards.");
      end if;

      return From_TOML (From);
   end Binary_From_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return Archive_Data
   is (Archive_Data
       (Conditional_Archive'
          (Conditional_Archives.Tree'(From_TOML (From)) with null record)
        .As_Data));

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (This : in out Origin;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is

      use TOML;
      use all type URI.URI_Kinds;
      Table   : constant TOML_Adapters.Key_Queue :=
                 From.Descend (From.Checked_Pop (Keys.Origin, TOML_Table),
                               Context => Keys.Origin);

      -----------------
      -- Mark_Binary --
      -----------------

      procedure Mark_Binary (Data : in out Archive_Data) is
      begin
         Data.Binary := True;
      end Mark_Binary;

      -------------------------
      -- Load_Source_Archive --
      -------------------------

      procedure Load_Source_Archive (This  : in out Origin;
                                     Table : TOML_Adapters.Key_Queue;
                                     URL   : String) is
      begin
         --  Reinsert the URL so we can reuse the dynamic archive loader:
         Table.Unwrap.Set (Keys.URL, Create_String (URL));

         --  And load
         This := (Data => (Kind        => Source_Archive,
                           Src_Archive => From_TOML
                             (Table.Descend
                                (Keys.Origin,
                                 Table.Unwrap,
                                 Context => "source archive")),
                           Hashes      => <>));
      end Load_Source_Archive;

   begin
      --  Check if we are seeing a conditional binary origin, or a regular
      --  static one. If the former, divert to the dynamic loader; else
      --  continue loading normally.

      if Table.Contains_Expression
        or else
          (Table.Unwrap.Has (Keys.Binary) and then
           Table.Unwrap.Get (Keys.Binary).As_Boolean)
      then
         This := (Data => Origin_Data'
                    (Kind         => Binary_Archive,
                     Bin_Archive  => (Binary_Loader.Load
                       (From    => Table.Descend
                            (Keys.Origin,
                             Table.Unwrap,
                             Context => "binary archive"),
                        Loader  => Binary_From_TOML'Access,
                        Resolve => True,
                        Strict  => False) with null record)));

         --  Mark these as explicitly binary, because they're in a case, even
         --  if the maintainer omitted the binary field. This saves some noise
         --  in the manifest files.

         This.Data.Bin_Archive.Visit_All (Mark_Binary'Access);

         return Outcome_Success;
      end if;

      --  Regular static loading of other origin kinds

      declare
         URL      : constant String :=
                     Table.Checked_Pop (Keys.URL, TOML_String).As_String;
         URL_Kind : constant URI.URI_Kinds := URI.URI_Kind (URL);
         Hashed   : constant Boolean := Table.Unwrap.Has (Keys.Hashes);
      begin
         case URL_Kind is
         when External                     =>
            This := New_External (URI.Path (URL));

         when URI.Local_Other              =>
            if URI.Local_Path (URL) = "" then
               From.Checked_Error ("empty path given in local origin");
            end if;
            This := New_Filesystem (URI.Local_Path (URL));

         when URI.VCS_URIs                 =>
            if URL_Kind in URI.Probably_Git and then Hashed then
               --  To resolve the ambiguity of Probably_Git, assume a source
               --  archive if the "hashes" field is present.
               Load_Source_Archive (This, Table, URL);
            else
               --  In all other cases, treat this as a git repo.
               declare
                  Commit : constant String := Table.Checked_Pop
                  (Keys.Commit, TOML_String).As_String;
                  Subdir : constant String :=
                           (if Table.Contains (Keys.Subdir)
                              then Table.Checked_Pop
                              (Keys.Subdir, TOML_String).As_String
                              else "");
               begin
                  This := New_VCS
                  (URL,
                     Commit => Commit,
                     Subdir => VFS.To_Native (Portable_Path (Subdir)));
               end;
            end if;

         when URI.HTTP_Other               =>
            Load_Source_Archive (This, Table, URL);

         when SSH_Other                    =>
            From.Checked_Error ("Pure 'ssh://' URLs are not valid crate "
                                & "origins. You may want git+" & URL
                                & " instead.");

         when System                       =>
            This := New_System (URI.Path (URL));

         when Unknown                      =>
            From.Checked_Error ("unsupported scheme in URL: " & URL);
         end case;

         --  Check hashes existence appropriateness

         case This.Kind is
         when Filesystem =>
            if Hashed then
               Add_Hashes (This.Data.Hashes, Table).Assert;
            end if;
            --  Hashes are mandatory only for source archives. This is checked
            --  on deployment, since at this moment we do not have the proper
            --  absolute patch

         when Binary_Archive =>
            --  Should not happen, as we have loaded this particular case above
            raise Program_Error with
               Errors.Set ("This case should be unreachable");

         when Source_Archive =>
            --  Hashes already loaded by the archive data loader
            Assert (not This.Data.Src_Archive.Hashes.Is_Empty,
                    Or_Else => "source archive hashes missing");

         when others =>
            if Hashed then
               return Table.Failure
                 ("hashes cannot be provided for origins of kind "
                  & AAA.Strings.To_Mixed_Case (This.Kind'Img));
            end if;
         end case;

         return Table.Report_Extra_Keys;
      end;
   end From_TOML;

   -----------
   -- Image --
   -----------

   function Image (This : Origin) return String is
     ((case This.Kind is
          when VCS_Kinds      =>
             "commit " & S (This.Data.Commit)
       & " from " & S (This.Data.Repo_URL),
          when Archive_Kinds  =>
         (if This.Kind in Source_Archive then
             Source_Image (This.Data.Src_Archive)
          elsif This.Data.Bin_Archive.Is_Value then
             Binary_Image (This.Data.Bin_Archive.As_Data)
          elsif This.Data.Bin_Archive.Is_Empty then
             "(unavailable on current platform)"
          else
             This.Data.Bin_Archive.Image_One_Line),
          when System         =>
             "system package from platform software manager: "
       & This.Package_Name,
          when Filesystem     =>
             "path " & S (This.Data.Path),
          when External       =>
             "external " & S (This.Data.Description))
      & (if This.Get_Hashes.Is_Empty
         then ""
         elsif This.Get_Hashes.Last_Index = 1
         then " with hash " & This.Image_Of_Hashes
         else " with hashes " & This.Image_Of_Hashes)
     );

   ---------------------
   -- Image_Of_Hashes --
   ---------------------

   function Image_Of_Hashes (This : Origin) return String is

      --  Recursively concatenate all hashes:
      function Reduce (I : Natural := This.Get_Hashes.Last_Index)
                       return String is
        (if I = 0 then ""
         elsif I > 1 then Reduce (I => I - 1) & ", "
                          & String (This.Get_Hashes.Element (I))
         else String (This.Get_Hashes.Element (I)));

   begin
      return Reduce;
   end Image_Of_Hashes;

   ------------------
   -- Short_Commit --
   ------------------

   function Short_Commit (Commit : String) return String
   is (if Commit'Length < 8
       then Commit
       else Commit (Commit'First .. Commit'First + 7));

   ---------------------
   -- Short_Unique_Id --
   ---------------------

   function Short_Unique_Id (This : Origin) return String
   is (Short_Commit
         (if This.Kind in Source_Archive | Binary_Archive
          then AAA.Strings.Tail (String (This.Get_Hashes.First_Element), ':')
          else This.Commit));

   -------------
   -- To_TOML --
   -------------

   overriding function To_TOML (This : Origin) return TOML.TOML_Value is
      use TOML_Adapters;
      Table : TOML.TOML_Value := TOML.Create_Table;
   begin
      case This.Kind is
         when Filesystem =>
            Table.Set (Keys.URL, +(URI.To_URL (This.Path)));

         when VCS_Kinds =>
            Table.Set (Keys.URL, +This.Explicit_URL);
            Table.Set (Keys.Commit, +This.Commit);
            if This.Subdir /= "" then
               Table.Set (Keys.Subdir,
                          +String (VFS.To_Portable (This.Subdir)));
            end if;

         when External =>
            Table.Set (Keys.URL,
                       +(Prefix_External & (+This.Data.Description)));

         when Binary_Archive =>
            Table := TOML_Adapters.Merge_Tables
              (Table,
               This.Data.Bin_Archive.As_Data.To_TOML);

         when Source_Archive =>
            Table := TOML_Adapters.Merge_Tables
              (Table,
               This.Data.Src_Archive.To_TOML);

         when System =>
            Table.Set (Keys.URL,
                       +(Prefix_System & This.Package_Name));
      end case;

      if not This.Get_Hashes.Is_Empty then
         declare
            Hashes : constant TOML.TOML_Value := TOML.Create_Array;
         begin
            for Hash of This.Get_Hashes loop
               Hashes.Append (+String (Hash));
            end loop;

            Table.Set (Keys.Hashes, Hashes);
         end;
      end if;

      return Table;
   end To_TOML;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Archive_Data) return TOML.TOML_Value is
      use TOML;
      Table : constant TOML.TOML_Value := TOML.Create_Table;
   begin
      Table.Set (Keys.URL, Create_String (This.URL));

      if This.Name /= "" and then
        This.Name /= URL_Basename (+This.URL)
      then
         Table.Set (Keys.Archive_Name, Create_String (This.Name));
      end if;

      if This.Binary then
         Table.Set (Keys.Binary, Create_Boolean (This.Binary));
      end if;

      return Table;
   end To_TOML;

   --------------
   -- Whenever --
   --------------

   function Whenever (This : Origin; Env : Properties.Vector) return Origin is
   begin
      if This.Kind = Binary_Archive then
         return Result : Origin := This do
            Result.Data.Bin_Archive := This.Data.Bin_Archive.Evaluate (Env);
         end return;
      else
         return This;
      end if;
   end Whenever;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available (This : Origin; Env : Properties.Vector)
                             return Boolean
   is (This.Kind /= Binary_Archive
       or else
       not This.Data.Bin_Archive.Evaluate (Env).Is_Empty);

   ------------------
   -- Is_Installed --
   ------------------

   function Already_Installed (This : Origin) return Boolean
                               renames Deployers.System.Already_Installed;

end Alire.Origins;
