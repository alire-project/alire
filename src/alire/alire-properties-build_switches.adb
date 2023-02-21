with Alire.TOML_Keys;
with TOML; use TOML;

with Alire.Utils.Did_You_Mean;

package body Alire.Properties.Build_Switches is

   function Profile_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Profile_Kind, Utils.Did_You_Mean.Lower_Case);

   function Switches_Categories_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Switches_Categories, Utils.Did_You_Mean.Lower_Case);

   function Optimization_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Optimization_Kind, Utils.Did_You_Mean.Lower_Case);

   function Debug_Info_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Debug_Info_Kind, Utils.Did_You_Mean.Lower_Case);

   function Runtime_Checks_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Runtime_Checks_Kind, Utils.Did_You_Mean.Lower_Case);

   function Compile_Checks_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Compile_Checks_Kind, Utils.Did_You_Mean.Lower_Case);

   function Contracts_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Contracts_Kind, Utils.Did_You_Mean.Lower_Case);

   function Style_Checks_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Style_Checks_Kind, Utils.Did_You_Mean.Lower_Case);

   function Ada_Version_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Ada_Version_Kind, Utils.Did_You_Mean.Lower_Case);

   function Unicode_Support_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Unicode_Support_Kind, Utils.Did_You_Mean.Lower_Case);

   -----------------------
   -- Switch_Suggestion --
   -----------------------

   function Switch_Suggestion (Str : String; Cat : Switches_Categories)
                               return String
   is
   begin
      case Cat is
         when Optimization => return Optimization_Suggestion (Str);
         when Debug_Info => return Debug_Info_Suggestion (Str);
         when Contracts => return Contracts_Suggestion (Str);
         when Compile_Checks => return Compile_Checks_Suggestion (Str);
         when Runtime_Checks => return Runtime_Checks_Suggestion (Str);
         when Style_Checks => return Style_Checks_Suggestion (Str);
         when Ada_Version => return Ada_Version_Suggestion (Str);
         when Unicode_Support => return Unicode_Support_Suggestion (Str);
      end case;
   end Switch_Suggestion;

   --------------
   -- Modifier --
   --------------

   function Modifier (This : Variable)
                      return Profile_Modifier
   is
   begin
      return This.Modif;
   end Modifier;

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : Variable) return String is
      ("Build Switches: ");

   ---------
   -- Key --
   ---------

   overriding
   function Key (This : Variable) return String is
      pragma Unreferenced (This);
   begin
      return TOML_Keys.Build_Switches;
   end Key;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue;
                       Cat  : Switches_Categories;
                       T    : TOML.TOML_Value)
                       return Switches_Modifier
   is
      Result : Switches_Modifier (Cat);
      List   : Switch_List;
   begin

      if T.Kind = TOML_Array then

         if (for some Index in 1 .. T.Length
             => T.Item (Index).Kind /= TOML_String)
         then
            From.Checked_Error
              ("At least one element on the switch list is not a string");
         end if;
         --  We have a custom list of switches

         for Index in 1 .. T.Length loop
            List.Append (T.Item (Index).As_String);
         end loop;

         case Cat is
         when Optimization =>
            Result.Optimization := (Custom => True, List => List);
         when Debug_Info =>
            Result.Debug_Info := (Custom => True, List => List);
         when Compile_Checks =>
            Result.Compile_Checks := (Custom => True, List => List);
         when Runtime_Checks =>
            Result.Runtime_Checks := (Custom => True, List => List);
         when Contracts =>
            Result.Contracts := (Custom => True, List => List);
         when Style_Checks =>
            Result.Style_Checks := (Custom => True, List => List);
         when Ada_Version =>
            Result.Ada_Version := (Custom => True, List => List);
         when Unicode_Support =>
            Result.Unicode_Support := (Custom => True, List => List);
         end case;

      elsif T.Kind = TOML_String then

         begin
            case Cat is
            when Optimization =>
               declare
                  K : constant Optimization_Kind :=
                    Optimization_Kind'Value (T.As_String);
               begin
                  Result.Optimization := (Custom => False, Value => K);
               end;

            when Debug_Info =>
               declare
                  K : constant Debug_Info_Kind :=
                    Debug_Info_Kind'Value (T.As_String);
               begin
                  Result.Debug_Info := (Custom => False, Value => K);
               end;

            when Runtime_Checks =>
               declare
                  K : constant Runtime_Checks_Kind :=
                    Runtime_Checks_Kind'Value (T.As_String);
               begin
                  Result.Runtime_Checks := (Custom => False, Value => K);
               end;

               when Compile_Checks =>
               declare
                  K : constant Compile_Checks_Kind :=
                    Compile_Checks_Kind'Value (T.As_String);
               begin
                  Result.Compile_Checks := (Custom => False, Value => K);
               end;

               when Contracts =>
               declare
                  K : constant Contracts_Kind :=
                    Contracts_Kind'Value (T.As_String);
               begin
                  Result.Contracts := (Custom => False, Value => K);
               end;

            when Style_Checks =>
               declare
                  K : constant Style_Checks_Kind :=
                    Style_Checks_Kind'Value (T.As_String);
               begin
                  Result.Style_Checks := (Custom => False, Value => K);
               end;

            when Ada_Version =>
               declare
                  K : constant Ada_Version_Kind :=
                    Ada_Version_Kind'Value (T.As_String);
               begin
                  Result.Ada_Version := (Custom => False, Value => K);
               end;

            when Unicode_Support =>
               declare
                  K : constant Unicode_Support_Kind :=
                        Unicode_Support_Kind'Value (T.As_String);
               begin
                  Result.Unicode_Support := (Custom => False, Value => K);
               end;

            end case;
         exception
            when Constraint_Error =>
               From.Checked_Error
                 ("Invalid switch selector '" & T.As_String &
                    "' for category '" & Cat'Img & "'." &
                    Switch_Suggestion (T.As_String, Cat));
         end;

      else
         From.Checked_Error
           ("String or array of string expected");
      end if;

      return Result;
   end From_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue;
                       T    : TOML.TOML_Value)
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
            List.Append (From_TOML (From, Cat, T.Get (Key)));
         exception
            when Constraint_Error =>
               From.Checked_Error
                 ("Invalid switch category: '" & (+Key) & "'."
                  & Switches_Categories_Suggestion (+Key));
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
         when Ada_Version =>
            Sw.Ada_Version := M.Ada_Version;
         when Unicode_Support =>
            Sw.Unicode_Support := M.Unicode_Support;
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

   function From_TOML (From : TOML_Adapters.Key_Queue;
                       T    : TOML.TOML_Value)
                       return Profile_Modifier
   is
      Result : Profile_Modifier;
   begin
      if T.Kind /= TOML_Table then
         raise Program_Error;
      end if;

      for Key of T.Keys loop
         if (+Key) = "*" then
            Result.Wildcard := From_TOML (From, T.Get (Key));
         else
            declare
               Prof : Profile_Kind;
            begin
               Prof := Profile_Kind'Value (+Key);

               case Prof is
                  when Release =>
                     Result.Release := From_TOML (From, T.Get (Key));
                  when Validation =>
                     Result.Validation := From_TOML (From, T.Get (Key));
                  when Development =>
                     Result.Development := From_TOML (From, T.Get (Key));
               end case;

            exception
               when Constraint_Error =>
                  From.Checked_Error
                    ("Invalid profile name: '" & (+Key) & "'." &
                       Profile_Suggestion (+Key));
            end;
         end if;
      end loop;
      return Result;
   end From_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is
      use type Conditional.Properties;
      Env : TOML_Value;

      Var : Variable;
   begin
      if From.Unwrap.Kind /= TOML_Table then
         From.Checked_Error
           ("Build_Switches: table expected, but got: "
            & From.Unwrap.Kind'Img);
      end if;

      if From.Pop_Single_Table (Env, TOML_Table) /= TOML_Keys.Build_Switches
      then
         raise Program_Error;
         --  Can't happen, unless the dispatch to us itself was erroneous
      end if;

      Var.T := Env.Clone;

      return Props : Conditional.Properties do
         Var.Modif := From_TOML (From, Env);
         Props := Props and Var;
      end return;

   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Variable) return TOML.TOML_Value is
   begin
      return This.T;
   end To_TOML;

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (This : Variable) return String is
     ("Build switches: 'TODO'");

end Alire.Properties.Build_Switches;
