with TOML; use TOML;

package body Alire.Utils.Switches.Modifiers is

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (Cat : Switches_Categories;
                       T : TOML.TOML_Value)
                       return Switches_Modifier
   is
      Result : Switches_Modifier (Cat);
      List   : Switch_List;
   begin

      if T.Kind = TOML_Array then

         --  We have a custom list of switches

         for Index in 1 .. T.Length loop
            List.Append (T.Item (Index).As_String);
         end loop;

         case Cat is
         when Optimization =>
            Result.Optimization := (Custom, List);
         when Debug_Info =>
            Result.Debug_Info := (Custom, List);
         when Contracts =>
            Result.Contracts := (Custom, List);
         when Style_Checks =>
            Result.Style_Checks := (Custom, List);
         when others =>
            raise Program_Error; --  TODO
         end case;

      elsif T.Kind = TOML_String then

         begin
            case Cat is
            when Optimization =>
               declare
                  K : constant Optimization_Kind :=
                    Optimization_Kind'Value (T.As_String);
               begin
                  Result.Optimization :=
                    (case K is
                        when Performance => (Kind => Performance),
                        when Size        => (Kind => Size),
                        when Debug       => (Kind => Debug),
                        when Custom      => raise Constraint_Error);
               end;

            when Debug_Info =>
               declare
                  K : constant Debug_Info_Kind :=
                    Debug_Info_Kind'Value (T.As_String);
               begin
                  Result.Debug_Info :=
                    (case K is
                        when No     => (Kind => No),
                        when Yes    => (Kind => Yes),
                        when Custom => raise Constraint_Error);
               end;

            when Runtime_Checks =>
               declare
                  K : constant Runtime_Checks_Kind :=
                    Runtime_Checks_Kind'Value (T.As_String);
               begin
                  Result.Runtime_Checks :=
                    (case K is
                        when None       => (Kind => None),
                        when Default    => (Kind => Default),
                        when Overflow   => (Kind => Overflow),
                        when Everything => (Kind => Everything),
                        when Custom  => raise Constraint_Error);
               end;

               when Compile_Checks =>
               declare
                  K : constant Compile_Checks_Kind :=
                    Compile_Checks_Kind'Value (T.As_String);
               begin
                  Result.Compile_Checks :=
                    (case K is
                        when None     => (Kind => None),
                        when Warnings => (Kind => Warnings),
                        when Errors   => (Kind => Errors),
                        when Custom   => raise Constraint_Error);
               end;

               when Contracts =>
               declare
                  K : constant Contracts_Kind :=
                    Contracts_Kind'Value (T.As_String);
               begin
                  Result.Contracts :=
                    (case K is
                        when No     => (Kind => No),
                        when Yes    => (Kind => Yes),
                        when Custom => raise Constraint_Error);
               end;

            when Style_Checks =>
               declare
                  K : constant Style_Checks_Kind :=
                    Style_Checks_Kind'Value (T.As_String);
               begin
                  Result.Style_Checks :=
                    (case K is
                        when No     => (Kind => No),
                        when Yes    => (Kind => Yes),
                        when Custom => raise Constraint_Error);
               end;

            end case;
         exception
            when Constraint_Error =>
               Raise_Checked_Error
                 ("Invalid switch selector '" & T.As_String &
                    "' for catergory '" & Cat'Img & "'");
         end;

      else
         raise Program_Error;
      end if;

      return Result;
   end From_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (T : TOML.TOML_Value)
                       return Switches_Modifier_Lists.List
   is
      List : Switches_Modifier_Lists.List;
   begin
      if T.Kind /= TOML_Table then
         raise Program_Error;
      end if;

      for Key of T.Keys loop

         declare
            Cat : Switches_Categories;
         begin
            Cat := Switches_Categories'Value (+Key);
            Trace.Always (+Key);
            List.Append (From_TOML (Cat, T.Get (Key)));
         end;
      end loop;

      return List;
   end From_TOML;

   -----------
   -- Apply --
   -----------

   procedure Apply (Sw : in out Switches_Configuration;
                    M  :        Switches_Modifier)
   is
   begin
      case M.Cat is
         when Optimization =>
            Sw.Optimization := M.Optimization;
         when Debug_Info =>
            Sw.Debug_Info := M.Debug_Info;
         when Runtime_Checks =>
            Sw.Runtime_Checks := M.Runtime_Checks;
         when Compile_Checks =>
            Sw.Compile_Checks := M.Compile_Checks;
         when Contracts =>
            Sw.Contracts := M.Contracts;
         when Style_Checks =>
            Sw.Style_Checks := M.Style_Checks;
      end case;
   end Apply;

   -----------
   -- Apply --
   -----------

   procedure Apply (Sw : in out Switches_Configuration;
                    L  :        Switches_Modifier_Lists.List)
   is
   begin
      for Elt of L loop
         Apply (Sw, Elt);
      end loop;
   end Apply;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (T : TOML.TOML_Value)
                       return Profile_Modifier
   is
      Result : Profile_Modifier;
   begin
      if T.Kind /= TOML_Table then
         raise Program_Error;
      end if;

      for Key of T.Keys loop
         if (+Key) = "*" then
            Result.Wildcard := From_TOML (T.Get (Key));
         else
            declare
               Prof : Profile_Kind;
            begin
               Prof := Profile_Kind'Value (+Key);

               case Prof is
                  when Release =>
                     Result.Release := From_TOML (T.Get (Key));
                  when Validation =>
                     Result.Validation := From_TOML (T.Get (Key));
                  when Development =>
                     Result.Development := From_TOML (T.Get (Key));
               end case;

            exception
               when Constraint_Error =>
                  Raise_Checked_Error
                    ("Invalid profile name: '" & (+Key) & "'");
            end;
         end if;
      end loop;
      return Result;
   end From_TOML;

end Alire.Utils.Switches.Modifiers;
