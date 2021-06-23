with Alire.TOML_Adapters;
package body Alire.Requisites.Cases is

   Dots : constant String := "...";

   use TOML;

   ----------------
   -- Is_Boolean --
   ----------------

   function Is_Boolean (This : Enumerable; I : Enum) return Boolean is
      (This.Cases (I).To_TOML.Kind = TOML_Boolean);

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (This : Enumerable; I : Enum) return Boolean is
      (This.Cases (I).To_TOML.As_Boolean);

   ------------------
   -- Is_Satisfied --
   ------------------

   function Is_Satisfied (E : Enum; V : Properties.Vector) return Boolean is
     (for some P of V =>
         P in Property and then
         Element (Property (P)) = E);

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Enumerable) return TOML.TOML_Value is

      ---------------
      -- Aggregate --
      ---------------

      --  With this function we construct a case entry with all matching
      --  (true/false) alternatives: "'foo|bar' = ..."
      function Aggregate (Bool  : Boolean;
                          I     : Enum;
                          Prev  : String) return String is
        (if This.Is_Boolean (I) and then This.As_Boolean (I) = Bool then
             (if Prev /= ""
              then Prev & "|"
              else "") & TOML_Adapters.Tomify (I'Img)
         else Prev);

      ----------------------
      -- Set_If_Not_Empty --
      ----------------------

      procedure Set_If_Not_Empty (Table : TOML.TOML_Value;
                                  Key   : String;
                                  Value : TOML.TOML_Value) is
      begin
         if Key /= "" then
            Table.Set (Key, Value);
         end if;
      end Set_If_Not_Empty;

      Same   : Boolean :=
                   This.Is_Boolean (Enum'First) and then
                   This.As_Boolean (Enum'First);
      --  Used to keep track that all entries in a case have the same value.

      Master : constant TOML.TOML_Value := TOML.Create_Table;
      --  The master TOML table "case(xx)"

      Cases  : constant TOML.TOML_Value := TOML.Create_Table;
      --  The child table with the entries.
   begin
      Master.Set ("case(" & TOML_Name & ")", Cases);

      --  Check that all are equal
      for I in This.Cases'Range loop
         Same := This.Is_Boolean (I) and then This.As_Boolean (I) = Same;
         exit when not Same;
      end loop;

      if Same then
         Cases.Set (Dots, TOML.Create_Boolean (Same));
      else
         Set_If_Not_Empty (Cases,
                           Aggregate (True,  Enum'First, ""),
                           TOML.Create_Boolean (True));
         Set_If_Not_Empty (Cases,
                           Aggregate (False, Enum'First, ""),
                           TOML.Create_Boolean (False));
         for I in This.Cases'Range loop
            if not This.Is_Boolean (I) then
               raise Unimplemented;
               --  TODO: convert tree to TOML, and get key from the first
               --  entry, which will be a case (see Master above). Use that
               --  case as key for the remainder of tree.
               --  There are still no entries in the index that require this.
            end if;
         end loop;
      end if;

      return Master;
   end To_TOML;

end Alire.Requisites.Cases;
