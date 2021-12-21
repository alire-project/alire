with Alire.TOML_Keys;

with Alire.Utils.Switches; use Alire.Utils.Switches;

package body Alire.Properties.Build_Profiles is

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : Variable) return String
   is ("Build Profile: ");

   ---------
   -- Key --
   ---------

   overriding
   function Key (This : Variable) return String is
      pragma Unreferenced (This);
   begin
      return TOML_Keys.Build_Profiles;
   end Key;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is
      use type Conditional.Properties;
      use TOML;
      Env : TOML_Value;

      Var : Variable;
   begin
      if From.Unwrap.Kind /= TOML_Table then
         From.Checked_Error
           ("Build: table with assignments expected, but got: "
            & From.Unwrap.Kind'Img);
      end if;

      if From.Pop_Single_Table (Env, TOML_Table) /= TOML_Keys.Build_Profiles
      then
         raise Program_Error;
         --  Can't happen, unless the dispatch to us itself was erroneous
      end if;

      Var.T := Env.Clone;

      --  Check that the data is valid
      for Crate of Env.Keys loop
         declare
            Crate_Str : constant String := +Crate;
            Profile : constant TOML_Value := Env.Get (Crate);
         begin

            if Profile.Kind /= TOML_String then
               From.Checked_Error ("Should be string");
            end if;

            declare
               Profile_Str : constant String := Profile.As_String;
            begin

               if Crate_Str = "*" then

                  if Var.Wildcard_Found then
                     From.Checked_Error
                       ("Multiple definition of wildcard (""*"")" &
                          " build profile");
                  else
                     Var.Wildcard_Found := True;
                  end if;

               elsif not Is_Valid_Name (Crate_Str) then
                  From.Checked_Error
                    ("Invalid crate name for build profile (" &
                       Error_In_Name (Crate_Str) & ")");
               else

                  declare
                     Unused : Profile_Kind;
                  begin
                     Unused := Profile_Kind'Value (Profile_Str);
                  exception
                     when Constraint_Error =>
                        From.Checked_Error
                          ("Invalid build profile name: '" & Profile_Str
                           & "' for '" & Crate_Str & "'");
                  end;
               end if;
            end;

            Env.Unset (+Crate);
         end;
      end loop;

      return Props : Conditional.Properties do
         Props := Props and Var;
      end return;
   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Variable) return TOML.TOML_Value is
   begin
      return This.T.Clone;
   end To_TOML;

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (This : Variable) return String
   is ("Build profile: []");

   ---------------
   -- Selection --
   ---------------

   function Selection (This : Variable) return Profile_Selection_Maps.Map
   is
   begin
      return Result : Profile_Selection_Maps.Map do
         for Crate of This.T.Keys loop
            if (+Crate) /= "*" then
               declare
                  Val : constant TOML.TOML_Value := This.T.Get (Crate);
               begin
                  Result.Insert (+(+Crate),
                                 Profile_Kind'Value (Val.As_String));
               end;
            end if;
         end loop;
      end return;
   end Selection;

   ------------------
   -- Has_Wildcard --
   ------------------

   function Has_Wildcard (This : Variable) return Boolean is
   begin
      return This.Wildcard_Found;
   end Has_Wildcard;

   --------------
   -- Wildcard --
   --------------

   function Wildcard (This : Variable) return Profile_Kind is
   begin
      for Crate of This.T.Keys loop
         if (+Crate) = "*" then
            return Profile_Kind'Value (This.T.Get (Crate).As_String);
         end if;
      end loop;
      raise Program_Error;
   end Wildcard;

end Alire.Properties.Build_Profiles;
