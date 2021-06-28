with Alire.URI;
with Alire.VCSs.Git;

package body Alire.Origins is

   ----------
   -- Keys --
   ----------

   package Keys is -- TOML keys for serialization

      Archive_Name : constant String := "archive-name";
      Commit       : constant String := "commit";
      Hashes       : constant String := "hashes";
      Origin       : constant String := "origin";
      URL          : constant String := "url";

   end Keys;

   function URL_Basename (URL : Alire.URL) return String;
   --  Try to get a basename for the given URL. Return an empty string on
   --  failure.

   --------------
   -- Add_Hash --
   --------------

   procedure Add_Hash (This : in out Origin;
                       Hash :        Hashes.Any_Hash) is
   begin
      This.Data.Hashes.Append (Hash);
   end Add_Hash;

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
      use Utils;
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

      return (Data => (Source_Archive,
                       Hashes         => <>,
                       Archive_URL    => +URL,
                       Archive_Name   => +Archive_Name,
                       Archive_Format => Format));
   end New_Source_Archive;

   -----------------
   -- From_String --
   -----------------

   function From_String (Image : String) return Origin is
      Scheme  : constant URI.Schemes := URI.Scheme (Image);
   begin
      case Scheme is
         when URI.File_Schemes =>
            return New_Filesystem (URI.Local_Path (Image));
         when URI.HTTP =>
            return New_Source_Archive (Image);
         when others =>
            Raise_Checked_Error ("Unsupported URL scheme: " & Image);
      end case;
   end From_String;

   -------------
   -- New_VCS --
   -------------

   function New_VCS (URL : Alire.URL; Commit : String) return Origin is
      use all type URI.Schemes;
      Scheme      : constant URI.Schemes := URI.Scheme (URL);
      Transformed : constant Alire.URL := VCSs.Git.Transform_To_Public (URL);
      VCS_URL : constant String :=
                  (if Utils.Contains (URL, "file:") then
                      Utils.Tail (URL, ':') -- Remove file: that confuses git
                   elsif Utils.Starts_With (URL, "git@") and then
                      Transformed /= URL -- known and transformable
                   then
                      Transformed
                   elsif Scheme in URI.VCS_Schemes then
                      Utils.Tail (URL, '+') -- remove prefix vcs+
                   elsif Scheme in URI.HTTP then -- A plain URL... check VCS
                     (if Utils.Ends_With (Utils.To_Lower_Case (URL), ".git")
                      then URL
                      elsif VCSs.Git.Known_Transformable_Hosts.Contains
                        (URI.Authority (URL))
                      then URL & ".git"
                      else raise Checked_Error with
                        "ambiguous VCS URL: " & URL)
                   else
                      raise Checked_Error with "unknown VCS URL: " & URL);

   begin
      case Scheme is
         when Pure_Git =>
            if Transformed /= URL then
               return New_VCS (Transformed, Commit);
            else
               Raise_Checked_Error
                 ("Attempting to use a private git@ URL with an unknown host: "
                  & Utils.TTY.URL (URL));
            end if;
         when Git | HTTP =>
            if Commit'Length /= Git_Commit'Length then
               Raise_Checked_Error
                 ("invalid git commit id, " &
                    "40 digits hexadecimal expected");
            end if;
            return New_Git (VCS_URL, Commit);
         when Hg =>
            if Commit'Length /= Hg_Commit'Length then
               Raise_Checked_Error
                 ("invalid mercurial commit id, " &
                    "40 digits hexadecimal expected");
            end if;
            return New_Hg (VCS_URL, Commit);
         when SVN =>
            return New_SVN (VCS_URL, Commit);
         when others =>
            Raise_Checked_Error ("Expected a VCS origin but got scheme: "
                                 & Scheme'Image);
      end case;
   end New_VCS;

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (This : in out Origin;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is

      ----------------
      -- Add_Hashes --
      ----------------

      function Add_Hashes (Parent : TOML_Adapters.Key_Queue) return Outcome is
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

                  This.Add_Hash (Hashes.Any_Hash (Hash));
               end;
            end loop;
         else
            return Parent.Failure
              ("missing mandatory " & Keys.Hashes & " field");
         end if;

         return Outcome_Success;
      end Add_Hashes;

      use TOML;
      use all type URI.Schemes;
      Archive : TOML_Value;
      Table   : constant TOML_Adapters.Key_Queue :=
                 From.Descend (From.Checked_Pop (Keys.Origin, TOML_Table),
                              Context => Keys.Origin);
      URL     : constant String :=
                 Table.Checked_Pop (Keys.URL, TOML_String).As_String;
      Scheme  : constant URI.Schemes := URI.Scheme (URL);
      Hashed  : constant Boolean := Table.Unwrap.Has (Keys.Hashes);
   begin
      case Scheme is
         when External =>
            This := New_External (URI.Path (URL));

         when URI.File_Schemes =>
            if URI.Local_Path (URL) = "" then
               From.Checked_Error ("empty path given in local origin");
            end if;
            This := New_Filesystem (URI.Local_Path (URL));

         when URI.VCS_Schemes  => null;
            declare
               Commit : constant String := Table.Checked_Pop
                 (Keys.Commit, TOML_String).As_String;
            begin
               This := New_VCS (URL, Commit);
            end;

         when HTTP             =>
            --  Optional filename checks:
            if Table.Pop (Keys.Archive_Name, Archive) then
               if Archive.Kind /= TOML.TOML_String then
                  return Table.Failure ("archive name must be a string");
               end if;
            end if;

            begin
               This := New_Source_Archive
                 (URL  => URL,
                  Name => (if Archive.Is_Present
                           then Archive.As_String
                           else ""));
            exception
               when Unknown_Source_Archive_Name_Error =>
                  return Table.Failure
                    ("unable to determine archive name from URL: "
                     & "please specify one with '"
                     & Keys.Archive_Name & "'");
            end;

         when System =>
            This := New_System (URI.Path (URL));

         when Unknown          =>
            From.Checked_Error ("unsupported scheme in URL: " & URL);
      end case;

      --  Check hashes existence appropriateness

      case This.Kind is
         when Filesystem =>
            if Hashed then
               return Add_Hashes (Table);
            end if;
            --  Hashes are mandatory only for source archives. This is checked
            --  on deployment, since at this moment we do not have the proper
            --  absolute patch

         when Source_Archive =>
            return Add_Hashes (Table); -- mandatory

         when others =>
            if Hashed then
               return Table.Failure
                 ("hashes cannot be provided for origins of kind "
                  & Utils.To_Mixed_Case (This.Kind'Img));
            end if;
      end case;

      return Table.Report_Extra_Keys;
   end From_TOML;

   ---------------------
   -- Image_Of_Hashes --
   ---------------------

   function Image_Of_Hashes (This : Origin) return String is

      --  Recursively concatenate all hashes:
      function Reduce (I : Natural := This.Data.Hashes.Last_Index)
                       return String is
        (if I = 0 then ""
         elsif I > 1 then Reduce (I => I - 1) & ", "
                          & String (This.Data.Hashes.Element (I))
         else String (This.Data.Hashes.Element (I)));

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
         (if This.Kind = Source_Archive
          then Utils.Tail (String (This.Data.Hashes.First_Element), ':')
          else This.Commit));

   -------------
   -- To_TOML --
   -------------

   overriding function To_TOML (This : Origin) return TOML.TOML_Value is
      use TOML_Adapters;
      Table : constant TOML.TOML_Value := TOML.Create_Table;
   begin
      case This.Kind is
         when Filesystem =>
            Table.Set (Keys.URL, +("file:" & This.Path));

         when VCS_Kinds =>
            Table.Set (Keys.URL,
                       +(Prefixes (This.Kind).all
                       & (if URI.Scheme (This.URL) in URI.None
                           --  not needed for remote repos, but for testing
                           --  ones used locally:
                          then "file:"
                          else "")
                       & This.URL));
            Table.Set (Keys.Commit, +This.Commit);

         when External =>
            Table.Set (Keys.URL,
                       +(Prefixes (This.Kind).all & (+This.Data.Description)));

         when Source_Archive =>
            Table.Set (Keys.URL, +This.Archive_URL);
            if This.Archive_Name /= "" and then
              This.Archive_Name /= URL_Basename (This.Archive_URL)
            then
               Table.Set (Keys.Archive_Name, +This.Archive_Name);
            end if;

         when System =>
            Table.Set (Keys.URL,
                       +(Prefixes (This.Kind).all & This.Package_Name));
      end case;

      if not This.Data.Hashes.Is_Empty then
         declare
            Hashes : constant TOML.TOML_Value := TOML.Create_Array;
         begin
            for Hash of This.Data.Hashes loop
               Hashes.Append (+String (Hash));
            end loop;

            Table.Set (Keys.Hashes, Hashes);
         end;
      end if;

      return Table;
   end To_TOML;

end Alire.Origins;
