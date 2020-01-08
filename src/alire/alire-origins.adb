with Alire.Errors;
with Alire.TOML_Expressions;
with Alire.TOML_Keys;
with Alire.Utils;

package body Alire.Origins is

   function Ends_With (S : String; Suffix : String) return Boolean is
     (S'Length >= Suffix'Length
      and then S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix);
   --  Return whether the S string ends with the given Suffix sub-string

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

            when '/' =>
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

   --  Loads a statically defined origin from its textual description.

   function From_String
     (This   : out Origin;
      From   : String;
      Parent : TOML_Adapters.Key_Queue := TOML_Adapters.Empty_Queue;
      Hashed : Boolean := True)
      return Outcome
   is

      ----------------
      -- Add_Hashes --
      ----------------

      function Add_Hashes (Mandatory : Boolean) return Outcome is
         Val : TOML.TOML_Value;
      begin
         if Parent.Pop (TOML_Keys.Origin_Hashes, Val) then
            if Val.Kind /= TOML.TOML_Array then
               return Parent.Failure
                 (TOML_Keys.Origin_Hashes
                  & " must be an array of hash values");
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
         elsif Hashed and then Mandatory then
            return Parent.Failure
              ("missing mandatory " & TOML_Keys.Origin_Hashes & " field");
         end if;

         return Outcome_Success;
      end Add_Hashes;

      use Utils;
      Commit : constant String := Tail (From, '@');
      URL    : constant String := Tail (Head (From, '@'), '+');
      Pkg    : constant String := Tail (From, ':');
      Path   : constant String :=
                 From (From'First + Prefixes (Filesystem)'Length ..
                         From'Last);
   begin
      --  Check easy ones first (unique prefixes):
      for Kind in Prefixes'Range loop
         if Prefixes (Kind) /= null and then
           Utils.Starts_With (From, Prefixes (Kind).all)
         then
            case Kind is
               when Git            => This := New_Git (URL, Commit);
               when Hg             => This := New_Hg (URL, Commit);
               when SVN            => This := New_SVN (URL, Commit);

               when Filesystem     =>
                  if Path = "" then
                     return Parent.Failure
                       ("empty path given in local origin");
                  else
                     This := New_Filesystem (Path);
                  end if;
               when Native         =>
                  This := New_Native ((others => Packaged_As (Pkg)));
               when External | Source_Archive =>
                  raise Program_Error with "can't happen";
            end case;

            if Hashed then
               --  Add optional (for now) hashes:
               return Add_Hashes (Mandatory => False);
            else
               return Outcome_Success;
            end if;
         end if;
      end loop;

      --  By elimination it must be a source archive (or erroneous):
      if not (Starts_With (From, "http://") or else
              Starts_With (From, "https://"))
      then
         return Parent.Failure ("unknown origin: " & From);
      else
         declare
            Archive : TOML.TOML_Value;
         begin
            --  Optional filename checks:
            if Parent.Pop (TOML_Keys.Archive_Name, Archive) then
               if Archive.Kind /= TOML.TOML_String then
                  return Parent.Failure ("archive name must be a string");
               end if;
            end if;

            begin
               This := New_Source_Archive
                 (URL  => From,
                  Name => (if Archive.Is_Present
                           then Archive.As_String
                           else ""));
            exception
               when Unknown_Source_Archive_Name_Error =>
                  return Parent.Failure
                    ("unable to determine archive name from URL: "
                     & "please specify one with '"
                     & TOML_Keys.Archive_Name & "'");
            end;

            return Add_Hashes (Mandatory => True);
         end;
      end if;
   end From_String;

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (This : in out Origin;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is

      -------------------------
      -- Package_From_String --
      -------------------------

      --  Extract, with error checking, the part after "native:", to create
      --  a platform-specific package origin.

      function Package_From_String (Val : TOML.TOML_Value;
                                    Pkg : out Package_Names) return Outcome is
      begin
         --  A missing entry defaults to unavailable:
         if Val.Is_Null then
            Pkg := Unavailable;
            return Outcome_Success;

         --  Otherwise, it must be a "native:blah" strings:
         elsif Val.Kind /= TOML.TOML_String then
            return From.Failure ("expected ""native:name"" string for origin");
         end if;

         declare
            Str : constant String := Val.As_String;
         begin
            if Str = "" then
               Pkg := Unavailable;
            elsif not Utils.Starts_With (Str, Prefix_Native) then
               return From.Failure ("native origin string must start with """
                                    & Prefix_Native
                                    & """ but found: " & Str);
            else
               Pkg := Packaged_As (Utils.Tail (Str, ':'));
            end if;

            return Outcome_Success;
         end;
      end Package_From_String;

      ---------------
      -- From_Case --
      ---------------

      function From_Case (Case_From : TOML.TOML_Value) return Outcome is
         package Distros is new TOML_Expressions.Enum_Cases
           (Platforms.Distributions);
      begin
         if Case_From.Keys'Length /= 1 or else
         +Case_From.Keys (1) /= "case(distribution)"
         then
            return From.Failure ("origins can only be distribution-specific");
         end if;

         --  Get an array of values that will be turned into origins:
         declare
            Distro_Origins : constant Distros.TOML_Array :=
                               Distros.Load_Cases
                                 (TOML_Adapters.From
                                    (Case_From.Get (Case_From.Keys (1)),
                                     From.Message ("case")));
         begin
            --  Load each origin
            This := New_Native ((others => Unavailable));

            for Distro in Distro_Origins'Range loop
               declare
                  Result : constant Outcome :=
                             Package_From_String
                               (Distro_Origins (Distro),
                                This.Data.Packages (Distro));
               begin
                  if not Result.Success then
                     return Result;
                  end if;
               end;
            end loop;

            return Outcome_Success;
         end;
      exception
         when E : Checked_Error =>
            return Errors.Get (E);
      end From_Case;

      Value : TOML.TOML_Value;
   begin
      if not From.Pop (TOML_Keys.Origin, Value) then
         return From.Failure ("mandatory origin missing");

      elsif Value.Kind = TOML.TOML_Table then
         --  A table: a case origin.
         return From_Case (Value);

      elsif Value.Kind = TOML.TOML_String then
         --  Plain string: regular origin
         return From_String (This,
                             Value.As_String,
                             From);
      else
         return From.Failure ("expected string description or case table");
      end if;
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

   ---------------------
   -- Short_Unique_Id --
   ---------------------

   function Short_Unique_Id (This : Origin) return String is
      Hash : constant String :=
               (if This.Kind = Source_Archive
                then Utils.Tail (String (This.Data.Hashes.First_Element), ':')
                else This.Commit);
   begin
      if Hash'Length < 8 then
         return Hash;
      else
         return Hash (Hash'First .. Hash'First + 7);
      end if;
   end Short_Unique_Id;

   -------------
   -- To_TOML --
   -------------

   overriding function To_TOML (This : Origin) return TOML.TOML_Value is
      use TOML_Adapters;
      Table : constant TOML.TOML_Value := TOML.Create_Table;
   begin
      case This.Kind is
         when Filesystem =>
            Table.Set (TOML_Keys.Origin, +("file://" & This.Path));

         when VCS_Kinds =>
            Table.Set (TOML_Keys.Origin, +(Prefixes (This.Kind).all &
                         This.URL & "@" & This.Commit));
         when External | Native =>
            raise Program_Error
              with "external or native packages do not need to be exported";

         when Source_Archive =>
            Table.Set (TOML_Keys.Origin,       +This.Archive_URL);
            if This.Archive_Name /= "" then
               Table.Set (TOML_Keys.Archive_Name, +This.Archive_Name);
            end if;
      end case;

      if not This.Data.Hashes.Is_Empty then
         declare
            Hashes : constant TOML.TOML_Value := TOML.Create_Array;
         begin
            for Hash of This.Data.Hashes loop
               Hashes.Append (+String (Hash));
            end loop;

            Table.Set (TOML_Keys.Origin_Hashes, Hashes);
         end;
      end if;

      return Table;
   end To_TOML;

end Alire.Origins;
