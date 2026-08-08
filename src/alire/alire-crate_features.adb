with Ada.Strings.Fixed;

package body Alire.Crate_Features is

   use all type TOML.Any_Value_Kind;

   -------------------------
   -- Selection_From_TOML --
   -------------------------

   function Selection_From_TOML
     (From : TOML_Adapters.Key_Queue) return Selection
   is
      Requested : AAA.Strings.Set;
      Defaults  : Boolean := True;
   begin
      if From.Contains ("features") then
         Requested := AAA.Strings.To_Set
           (TOML_Adapters.To_Vector
              (From.Checked_Pop ("features", TOML.TOML_Array)));
      end if;
      if From.Contains ("default_features") then
         Defaults := From.Checked_Pop
           ("default_features", TOML.TOML_Boolean).As_Boolean;
      end if;
      From.Report_Extra_Keys;
      return (Requested, Defaults);
   end Selection_From_TOML;

   function To_TOML (This : Selection) return TOML.TOML_Value is
   begin
      return Table : constant TOML.TOML_Value := TOML.Create_Table do
         Table.Set
           ("features", TOML_Adapters.To_TOML (This.Requested.To_Vector));
         Table.Set ("default_features",
                    TOML.Create_Boolean (This.Default_Features));
      end return;
   end To_TOML;

   procedure Configure (This : Selection) is
   begin
      Current_Selection := This;
      Explicit_Selection := True;
   end Configure;

   procedure Adopt (This : Selection) is
   begin
      if not Explicit_Selection then
         Current_Selection := This;
      end if;
   end Adopt;

   function Current return Selection is (Current_Selection);

   function Canonical (Name : String) return String
   is (AAA.Strings.To_Lower_Case (Name));

   function Valid_Name (Name : String) return Boolean
   is (Name /= ""
       and then
         (for all Ch of Name =>
            Ch in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_'));

   function Dependency_Part (Reference : String) return String is
      Slash : constant Natural := Ada.Strings.Fixed.Index (Reference, "/");
      Last  : Natural := (if Slash = 0 then Reference'Last else Slash - 1);
   begin
      if Last >= Reference'First and then Reference (Last) = '?' then
         Last := Last - 1;
      end if;
      return Reference (Reference'First .. Last);
   end Dependency_Part;

   ----------------
   -- From_TOML --
   ----------------

   function From_TOML
     (From : TOML_Adapters.Key_Queue) return Definitions
   is
      Result : Definitions;
   begin
      loop
         declare
            Value : TOML.TOML_Value;
            Name  : constant String := From.Pop (Value);
         begin
            exit when Name = "";

            if not Valid_Name (Name) then
               From.Checked_Error ("invalid feature name: " & Name);
            end if;
            From.Assert
              (Value.Kind = TOML.TOML_Array,
               "feature '" & Name & "' must be an array of strings");

            declare
               Items : AAA.Strings.Set;
            begin
               for Item of TOML_Adapters.To_Vector (Value) loop
                  if Item = "" then
                     From.Checked_Error
                       ("feature '" & Name
                        & "' contains an empty reference");
                  end if;
                  Items.Include (Canonical (Item));
               end loop;
               Result.Map.Insert (Canonical (Name), Items);
            end;
         end;
      end loop;

      return Result;
   end From_TOML;

   --------------
   -- To_TOML --
   --------------

   overriding
   function To_TOML (This : Definitions) return TOML.TOML_Value is
      use Definition_Maps;
   begin
      return Table : constant TOML.TOML_Value := TOML.Create_Table do
         for Position in This.Map.Iterate loop
            Table.Set
              (Key (Position),
               TOML_Adapters.To_TOML (Element (Position).To_Vector));
         end loop;
      end return;
   end To_TOML;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Definitions) return Boolean
   is (This.Map.Is_Empty);

   --------------
   -- Contains --
   --------------

   function Contains (This : Definitions; Name : String) return Boolean
   is (This.Map.Contains (Canonical (Name)));

   -------------
   -- Members --
   -------------

   function Members
     (This : Definitions; Name : String) return AAA.Strings.Set
   is (This.Map (Canonical (Name)));

   -----------
   -- Names --
   -----------

   function Names (This : Definitions) return AAA.Strings.Set is
      use Definition_Maps;
   begin
      return Result : AAA.Strings.Set do
         for Position in This.Map.Iterate loop
            Result.Include (Key (Position));
         end loop;
      end return;
   end Names;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Name : String) return String
   is (AAA.Strings.To_Mixed_Case
         (AAA.Strings.Replace (Canonical (Name), "-", "_")));

   --------------
   -- Validate --
   --------------

   procedure Validate (This         : Definitions;
                       Dependencies : AAA.Strings.Set;
                       Optional     : AAA.Strings.Set)
   is
      use Ada.Strings.Fixed;
   begin
      declare
         Identifiers : AAA.Strings.Set;
      begin
         for Feature of This.Names loop
            if Identifiers.Contains (Identifier (Feature)) then
               Raise_Checked_Error
                 ("feature names collide after identifier normalization: "
                  & Feature);
            end if;
            Identifiers.Include (Identifier (Feature));
         end loop;
      end;

      for Feature of This.Names loop
         for Reference of This.Members (Feature) loop
            declare
               Slash : constant Natural := Index (Reference, "/");
            begin
               if AAA.Strings.Has_Prefix (Reference, "dep:") then
                  declare
                     Name : constant String := Reference
                       (Reference'First + 4 .. Reference'Last);
                  begin
                     if not Optional.Contains (Name) then
                        Raise_Checked_Error
                          ("feature '" & Feature & "' activates '" & Name
                           & "', which is not an optional dependency");
                     end if;
                  end;
               elsif Slash /= 0 then
                  declare
                     Name : constant String := Dependency_Part (Reference);
                     Weak : constant Boolean :=
                       Slash > Reference'First
                       and then Reference (Slash - 1) = '?';
                     Forwarded : constant String := Reference
                       (Slash + 1 .. Reference'Last);
                  begin
                     if Slash = Reference'First
                       or else Slash = Reference'Last
                       or else Name = "" or else Forwarded = ""
                     then
                        Raise_Checked_Error
                          ("invalid feature forwarding reference: "
                           & Reference);
                     elsif not Dependencies.Contains (Name) then
                        Raise_Checked_Error
                          ("feature '" & Feature & "' forwards to unknown "
                           & "dependency '" & Name & "'");
                     elsif Weak and then not Optional.Contains (Name) then
                        Raise_Checked_Error
                          ("weak feature forwarding requires an optional "
                           & "dependency: " & Reference);
                     end if;
                  end;
               elsif not This.Contains (Reference) then
                  Raise_Checked_Error
                    ("feature '" & Feature & "' references unknown local "
                     & "feature '" & Reference & "'");
               end if;
            end;
         end loop;
      end loop;
   end Validate;

end Alire.Crate_Features;
