with Alire.Crates.With_Releases;

package body Alire.Dependencies.States is

   use TOML;

   package Keys is

      Crate        : constant String := "crate";
      Fulfilment   : constant String := "fulfilment";
      Pin_Version  : constant String := "pin_version";
      Pinned       : constant String := "pinned";
      Release      : constant String := "release";
      Transitivity : constant String := "transitivity";
      Versions     : constant String := "versions";

   end Keys;

   --  The output format is plainly each field in State with its value in a
   --  table: field = "value"

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return State
   is
      Crate : constant Crate_Name :=
                +From.Checked_Pop (Keys.Crate, TOML_String).As_String;
      Versions : constant Semantic_Versioning.Extended.Version_Set :=
                   Semantic_Versioning.Extended.Value
                     (From.Checked_Pop (Keys.Versions,
                                        TOML_String).As_String);
   begin
      return This : State := New_Dependency (Crate, Versions) do

         --  Transitivity

         This.Transitivity :=
           Transitivities'Value
             (From.Checked_Pop (Keys.Transitivity, TOML_String).As_String);

         --  Pinning

         declare
            Data : Pinning_Data
              (From.Checked_Pop (Keys.Pinned, TOML_Boolean).As_Boolean);
         begin
            if Data.Pinned then
               Data.Version := Semantic_Versioning.Parse
                 (From.Checked_Pop (Keys.Pin_Version, TOML_String).As_String);
            end if;

            This.Pinning := Data;
         end;

         --  Fulfilling

         declare
            Data : Fulfilment_Data
              (Fulfilments'Value
                 (From.Checked_Pop (Keys.Fulfilment, TOML_String).As_String));
            Crate : Crates.With_Releases.Crate :=
                      Crates.With_Releases.New_Crate (This.Crate);
         begin

            --  Load the release for a solved dependency

            if Data.Fulfilment = Solved then
               Assert (Crate.From_TOML -- Load crate
                       (From.Descend   -- from adapter that is under 'release'
                          (From.Checked_Pop (Keys.Release, TOML_Table)
                               .Get (+Crate.Name), -- get the release top entry
                           "release: " & (+This.Crate))));

               if Crate.Releases.Length not in 1 then
                  Raise_Checked_Error
                    ("Expected one release per solved dependency"
                     & " in lockfile, but got:" & Crate.Releases.Length'Img);
               end if;

               Data.Release :=
                 Containers.Release_Holders.To_Holder
                   (Crate.Releases.First_Element);
            end if;

            This.Fulfilled := Data;
         end;
      end return;
   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   overriding function To_TOML (This : State) return TOML.TOML_Value
   is
      use TOML_Adapters;
      use Utils;
   begin
      return Table : constant TOML_Value := Create_Table do

         --  Base

         Table.Set (Keys.Crate, +(+This.Crate));
         Table.Set (Keys.Versions, +This.Versions.Image);

         --  Transitivity

         Table.Set (Keys.Transitivity, +To_Lower_Case (This.Transitivity'Img));

         --  Pinning

         Table.Set (Keys.Pinned, Create_Boolean (This.Pinning.Pinned));
         if This.Pinning.Pinned then
            Table.Set (Keys.Pin_Version, +This.Pinning.Version.Image);
         end if;

         --  Fulfilment

         declare
            Name : constant TOML_Value := Create_Table;
            --  This extra table is not really necessary, but it makes the
            --  output clearer and the tests simpler.
         begin
            Table.Set (Keys.Fulfilment,
                       +To_Lower_Case (This.Fulfilled.Fulfilment'Img));

            --  Release for a solved dependency

            if This.Is_Solved then
               Name.Set (+This.Crate,
                         This.Fulfilled.Release.Constant_Reference.To_TOML);
               Table.Set (Keys.Release, Name);
            end if;
         end;
      end return;
   end To_TOML;

end Alire.Dependencies.States;
