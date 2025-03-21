with Alire.Settings.Edit;

procedure Alr_Tests.Config_Changes is

   --------------------------
   -- Check_Config_Changes --
   --------------------------

   procedure Check_Config_Changes is
      --  Ensure that configuration set in a run is also stored in memory
      Key  : constant String := "test_key";
      Val  : constant String := "nominal";
   begin
      Settings.Edit.Set_Globally (Key, Val);
      pragma Assert (Settings.DB.Defined (Key));
      pragma Assert (Settings.DB.Get (Key, "snafu") = Val);

      --  Check typed storing

      --  Raw storing of integer
      Settings.Edit.Set_Globally (Key, "777");
      pragma Assert (Integer (Settings.DB.Get (Key, 0)) = 777);

      --  Raw storing of boolean
      Settings.Edit.Set_Globally (Key, "true");
      pragma Assert (Settings.DB.Get (Key, False) = True);

      --  Typed storing of boolean
      Settings.Edit.Set_Boolean (Settings.Global, Key, False);
      pragma Assert (Settings.DB.Get (Key, True) = False);

      --  Raw storing of boolean with wrong type
      Settings.Edit.Set_Globally (Key, "True");
      --  This causes a string to be stored, as in TOML only "true" is bool
      pragma Assert (Settings.DB.Get (Key, "False") = "True");

      --  Remove test key
      Settings.Edit.Unset (Settings.Global, Key);
      pragma Assert (Settings.DB.Get (Key, "unset") = "unset");

   end Check_Config_Changes;

begin
   Check_Config_Changes;
end Alr_Tests.Config_Changes;
