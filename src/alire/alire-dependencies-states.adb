with Alire.Manifest;
with Alire.Milestones;
with Alire.Origins;
with Alire.Roots.Optional;

package body Alire.Dependencies.States is

   overriding function "=" (L, R : Stored_Release) return Boolean
   is
      use type Milestones.Milestone;
      use type Origins.Origin;
   begin
      return
        (L.Is_Empty and then R.Is_Empty)
        or else
          (not L.Is_Empty and then not R.Is_Empty
           and then
           L.Element.Milestone = R.Element.Milestone
           and then
           L.Element.Origin = R.Element.Origin);
   end "=";

   ----------------------
   -- Optional_Release --
   ----------------------

   function Optional_Release (Crate     : Crate_Name;
                              Workspace : Any_Path)
                              return Stored_Release
   is
      Opt_Root : constant Roots.Optional.Root :=
                   Roots.Optional.Detect_Root (Workspace);
   begin
      if Opt_Root.Is_Valid then
         if Opt_Root.Value.Release.Name = Crate then
            return To_Holder (Opt_Root.Value.Release);
         else
            Raise_Checked_Error ("crate mismatch: expected "
                                 & Crate.TTY_Image
                                 & " but found "
                                 & Opt_Root.Value.Release.Name.TTY_Image
                                 & " at " & TTY.URL (Workspace));
         end if;
      else
         return (Releases.Containers.Release_Holders.Empty_Holder
                 with null record);
      end if;
   end Optional_Release;

   use TOML;

   package Keys is

      Crate        : constant String := "crate";
      Fulfilment   : constant String := "fulfilment";
      Link         : constant String := "link";
      Pin_Version  : constant String := "pin_version";
      Pinned       : constant String := "pinned";
      Reason       : constant String := "reason";
      Release      : constant String := "release";
      Shared       : constant String := "shared";
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

      ---------------------
      -- Load_Fulfilment --
      ---------------------

      function Load_Fulfilment return Fulfillment_Data is
         Data  : Fulfillment_Data
           (Fulfillments'Value
              (From.Checked_Pop (Keys.Fulfilment, TOML_String).As_String));
      begin

         --  Load particulars

         case Data.Fulfillment is
            when Hinted => null;

            when Linked =>
               Data.Target := To_Holder
                 (User_Pins.From_TOML
                    (From.Descend
                         (Value   => From.Checked_Pop (Keys.Link, TOML_Table),
                          Context => Keys.Link)));
               Data.Opt_Rel := Optional_Release (From_TOML.Crate,
                                                 Data.Target.Element.Path);

            when Missed =>
               if From.Contains (Keys.Reason) then
                  Data.Reason :=
                    Missed_Reasons'Value
                      (From.Checked_Pop (Keys.Reason, TOML_String).As_String);
               else
                  Data.Reason := Unavailable;
                  --  For back-compatibility. Will be properly informed on next
                  --  solution update.
               end if;

            when Solved =>
               Data.Release :=
                 To_Holder
                   (Releases.From_TOML
                      (From.Descend
                         (From.Checked_Pop (Keys.Release, TOML_Table),
                          "release: " & (+Crate)),
                       Manifest.Index,
                       Strict => False)); -- because it may come from elsewhere
               Data.Shared :=
                 From.Checked_Pop (Keys.Shared, TOML_Boolean).As_Boolean;
         end case;

         return Data;
      end Load_Fulfilment;

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

         This.Fulfilled := Load_Fulfilment;

      end return;
   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : State) return TOML.TOML_Value
   is
      use TOML_Adapters;
      use AAA.Strings;

      -------------
      -- To_TOML --
      -------------

      procedure To_TOML (Data : Fulfillment_Data; Table : TOML_Value) is
      begin
         Table.Set (Keys.Fulfilment,
                    +To_Lower_Case (This.Fulfilled.Fulfillment'Img));

         case Data.Fulfillment is
            when Hinted => null;
            when Linked =>
               Table.Set (Keys.Link, Data.Target.Get.To_TOML);

            when Missed =>
               Table.Set (Keys.Reason,
                          +To_Lower_Case (Data.Reason'Img));

            when Solved =>
               Table.Set
                 (Keys.Release,
                  This.Fulfilled.Release.Constant_Reference.To_TOML
                    (Manifest.Index));
               Table.Set
                 (Keys.Shared,
                  Create_Boolean (This.Fulfilled.Shared));
         end case;
      end To_TOML;

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

         To_TOML (This.Fulfilled, Table);

      end return;
   end To_TOML;

end Alire.Dependencies.States;
