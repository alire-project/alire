package body Alire.Origins.Mirrors is

   ---------------
   -- From_Toml --
   ---------------

   procedure From_TOML (From    : TOML_Adapters.Key_Queue;
                        This    : in out Mirror_Vector;
                        Primary : Origins.Origin)
   is

      ----------------
      -- Load_Entry --
      ----------------
      --  Load one [[mirror]] entry and append it. A mirror provides only its
      --  url(s); the identity fields (commit/hashes/subdir) are forbidden and
      --  taken from the origin, which the origin loader enforces in mirror
      --  mode. We only additionally require the mirror to be of the same kind.

      procedure Load_Entry (Entry_Table : TOML.TOML_Value) is
         Mirror : Origins.Origin;
         Table  : constant TOML.TOML_Value :=
                    (if Entry_Table.Kind in TOML.TOML_Table
                     then Entry_Table.Clone
                     else Entry_Table);
         Queue  : constant TOML_Adapters.Key_Queue :=
                    From.Descend (Table, Context => TOML_Keys.Mirror);
      begin
         if Entry_Table.Kind /= TOML.TOML_Table then
            From.Checked_Error
              ("each " & TOML_Keys.Mirror & " entry must be a table");
         end if;

         --  A binary origin's mirror is necessarily binary, so the `binary`
         --  marker is optional in mirrors: set it for a flat (non-conditional)
         --  entry so it isn't taken for a source archive. Conditional entries
         --  already load as binary, and an explicit `binary` on a non-binary
         --  origin is rejected below by the kind check.
         if Primary.Kind in Binary_Archive
           and then not Queue.Contains_Expression
           and then not Table.Has (Keys.Binary)
         then
            Table.Set (Keys.Binary, TOML.Create_Boolean (True));
         end if;

         --  Reuse the regular origin loader (including the conditional binary
         --  path) on the bare entry, in mirror mode.
         Mirror.Load (Queue, Is_Mirror => True).Assert;

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
