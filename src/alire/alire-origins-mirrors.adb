with Alire.VFS;

package body Alire.Origins.Mirrors is

   ---------------
   -- From_Toml --
   ---------------

   procedure From_TOML (From    : TOML_Adapters.Key_Queue;
                        This    : in out Mirror_Vector;
                        Primary : Origins.Origin)
   is

      -----------
      -- Reuse --
      -----------
      --  A mirror is an exact copy of the origin except for its url, so any
      --  Key it carries must match the origin's value; when it is omitted we
      --  inject the origin's value so the loader reuses it without complaint.

      procedure Reuse (Table : TOML.TOML_Value;
                       Key   : String;
                       Value : TOML.TOML_Value;
                       Error : String)
      is
      begin
         if Table.Has (Key) then
            if not TOML.Equals (Table.Get (Key), Value) then
               From.Checked_Error (Error);
            end if;
         else
            Table.Set (Key, Value);
         end if;
      end Reuse;

      ----------------
      -- Load_Entry --
      ----------------
      --  Validate one [[mirror]] entry against the primary and append it.

      procedure Load_Entry (Entry_Table : TOML.TOML_Value) is
         Table  : constant TOML.TOML_Value := Entry_Table.Clone;
         Mirror : Origins.Origin;
      begin
         if Entry_Table.Kind /= TOML.TOML_Table then
            From.Checked_Error
              ("each " & TOML_Keys.Mirror & " entry must be a table");
         end if;

         --  Inject (and validate against) the origin's commit/hashes, so the
         --  mirror is loaded as a copy that differs only in its url. Any other
         --  unexpected key is rejected by Load below.

         case Primary.Kind is
            when VCS_Kinds =>
               Reuse (Table, Keys.Commit,
                      TOML.Create_String (Primary.Commit),
                      Error => "mirror commit must match the origin's"
                               & " or be omitted");
               if Primary.Subdir /= "" then
                  Reuse (Table, Keys.Subdir,
                         TOML.Create_String
                           (String (VFS.To_Portable (Primary.Subdir))),
                         Error => "mirror subdir must match the origin's"
                                  & " or be omitted");
               end if;

            when Archive_Kinds =>
               Reuse (Table, Keys.Hashes,
                      TOML_Adapters.To_TOML (Primary.Unique_Ids),
                      Error => "mirror hashes must match the origin's"
                               & " or be omitted");
               if Primary.Kind in Binary_Archive
                 and then not Table.Has (Keys.Binary)
               then
                  Table.Set (Keys.Binary, TOML.Create_Boolean (True));
               end if;

            when others =>
               null; -- Excluded by the Mirror_Kinds check below
         end case;

         --  Reuse the regular origin loader on the resulting bare table
         Mirror.Load
           (From.Descend (Table, Context => TOML_Keys.Mirror)).Assert;

         if Mirror.Kind /= Primary.Kind then
            From.Checked_Error
              ("mirror is a "
               & AAA.Strings.To_Mixed_Case (Mirror.Kind'Image)
               & " origin, but the authoritative origin is "
               & AAA.Strings.To_Mixed_Case (Primary.Kind'Image));
         end if;

         This.Append (Mirror);
      end Load_Entry;

   begin
      if not From.Contains (TOML_Keys.Mirror) then
         return;
      end if;

      declare
         Arr : constant TOML.TOML_Value := From.Unwrap.Get (TOML_Keys.Mirror);
      begin
         if Arr.Kind /= TOML.TOML_Array then
            From.Checked_Error
              (TOML_Keys.Mirror & " must be an array of tables");
         end if;

         if Arr.Length > 0 and then Primary.Kind not in Mirror_Kinds then
            From.Checked_Error
              ("origins of kind "
               & AAA.Strings.To_Mixed_Case (Primary.Kind'Image)
               & " cannot have mirrors");
         end if;

         for I in 1 .. Arr.Length loop
            Load_Entry (Arr.Item (I));
         end loop;
      end;

      --  Remove what we just loaded
      From.Unwrap.Unset (TOML_Keys.Mirror);
   end From_TOML;

   --------------
   -- Whenever --
   --------------

   function Whenever (This : Mirror_Vector;
                      Env  : Properties.Vector)
                      return Mirror_Vector
   is
   begin
      return Result : Mirror_Vector do
         for Mirror of This loop
            Result.Append (Mirror.Whenever (Env));
         end loop;
      end return;
   end Whenever;

end Alire.Origins.Mirrors;
