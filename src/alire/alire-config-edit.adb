with Ada.Text_IO;
with Ada.Directories;

with TOML; use TOML;

with Alire;

package body Alire.Config.Edit is

   procedure Write_Config_File (Table : TOML_Value; Path : Absolute_Path)
     with Pre => Table.Kind = TOML_Table;

   procedure Remove_From_Table (Table : TOML_Value; Key : Config_Key)
     with Pre => Table.Kind = TOML_Table;

   procedure Add_In_Table (Table : TOML_Value;
                           Key   : Config_Key;
                           Val   : TOML_Value)
     with Pre => Table.Kind = TOML_Table;

   -----------------------
   -- Write_Config_File --
   -----------------------

   procedure Write_Config_File (Table : TOML_Value; Path : Absolute_Path) is
      use Ada.Text_IO;
      use Ada.Directories;
      File : File_Type;
   begin

      --  Create the directory for the config file, in case it doesn't exists
      Create_Path (Containing_Directory (Path));

      Create (File, Out_File, Path);
      Trace.Debug ("Write config: '" & TOML.Dump_As_String (Table) & "'");
      Put (File, TOML.Dump_As_String (Table));
      Close (File);
   end Write_Config_File;

   -----------------------
   -- Remove_From_Table --
   -----------------------

   procedure Remove_From_Table (Table : TOML_Value; Key : Config_Key) is
      Id   : constant String := Utils.Split (Key, '.', Raises => False);
      Leaf : constant Boolean := Id = Key;
   begin
      if not Table.Has (Id) then
         --  The key doesn't exist
         return;
      end if;

      if Leaf then
         Table.Unset (Id);
      else
         declare
            Sub : constant TOML_Value := Table.Get (Id);
         begin
            if Sub.Kind = TOML_Table then
               Remove_From_Table (Sub, Utils.Split (Key, '.', Utils.Tail));
            else
               raise Program_Error;
            end if;
         end;
      end if;
   end Remove_From_Table;

   ------------------
   -- Add_In_Table --
   ------------------

   procedure Add_In_Table (Table : TOML_Value;
                           Key   : Config_Key;
                           Val   : TOML_Value)
   is
      Id   : constant String := Utils.Split (Key, '.', Raises => False);
      Leaf : constant Boolean := Id = Key;
   begin
      if Leaf then
         Table.Set (Id, Val);
         return;
      end if;

      if not Table.Has (Id) then
         --  The subkey doesn't exist, create a table for it
         Table.Set (Id, Create_Table);
      end if;

      declare
         Sub : constant TOML_Value := Table.Get (Id);
      begin
         if Sub.Kind = TOML_Table then
            Add_In_Table (Sub, Utils.Split (Key, '.', Utils.Tail), Val);
         else
            Raise_Checked_Error ("Configuration key already defined");
         end if;
      end;
   end Add_In_Table;

   -----------
   -- Unset --
   -----------

   procedure Unset (Path : Absolute_Path; Key : Config_Key) is
      Table : constant TOML_Value := Load_Config_File (Path);
   begin

      if Table.Is_Null then
         --  The configuration file doesn't exist or is not valid
         return;
      end if;

      Remove_From_Table (Table, Key);
      Write_Config_File (Table, Path);
   end Unset;

   ---------
   -- Set --
   ---------

   procedure Set (Path : Absolute_Path; Key : Config_Key; Value : String) is
      Table : TOML_Value := Load_Config_File (Path);

      To_Add : constant TOML_Value := To_TOML_Value (Value);
   begin
      if To_Add.Is_Null then
         Raise_Checked_Error ("Invalid configuration value: '" & Value & "'");
      end if;

      if not Valid_Builtin (Key, To_Add) then
         Raise_Checked_Error ("Invalid value '" & Value &
                                "' for builtin configuration. " &
                                Image (Kind_Of_Builtin (Key)) & " expected.");
      end if;

      if Table.Is_Null then
         --  The configuration file doesn't exist or is not valid. Create an
         --  empty table.
         Table := TOML.Create_Table;
      end if;

      Add_In_Table (Table, Key, To_Add);

      Write_Config_File (Table, Path);
   end Set;

end Alire.Config.Edit;
