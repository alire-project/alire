with CLIC.User_Input; use CLIC.User_Input;
with TOML;

package body Alire.Properties.Configurations.User_Input is

   -----------
   -- Query --
   -----------

   function Query (This : Config_Type_Definition) return String is

      function Int_Is_Valid (Str : String) return Boolean is
      begin
         return This.Valid (TOML.Create_Integer
                            (TOML.Any_Integer'Value (Str)));
      exception
         when others =>
            return False;
      end Int_Is_Valid;

      function Real_Is_Valid (Str : String) return Boolean is
      begin
         return This.Valid (TOML.Create_Float
                            ((TOML.Regular, TOML.Valid_Float'Value (Str))));
      exception
         when others =>
            return False;
      end Real_Is_Valid;

      function Bool_Is_Valid (Str : String) return Boolean is
      begin
         return This.Valid (TOML.Create_Boolean (Boolean'Value (Str)));
      exception
         when others =>
            return False;
      end Bool_Is_Valid;

   begin
      case This.Kind is
         when Str =>

            declare
               Default : constant String :=
                 (if This.Default.Is_Present
                   and then
                     This.Default.Kind = TOML_String
                  then This.Default.As_String
                  else "");
               Prompt : constant String :=
                 "Enter string value for template parameter " &
               (+This.Name) & " ";
            begin
               return Query_String
                 (Prompt,
                  Default,
                  Validation => null);
            end;

         when Enum =>

            declare
               Choices : AAA.Strings.Vector;

               Default : constant String :=
                 (if This.Default.Is_Present
                   and then
                     This.Default.Kind = TOML_String
                  then This.Default.As_String
                  else "");

               Prompt : constant String :=
                 "Select value for template parameter " &
               (+This.Name) & " ";
            begin

               --  Default value must be the first choice
               Choices.Append (Default);

               for Index in 1 .. This.Values.Length loop
                  declare
                     Item : constant String :=
                       This.Values.Item (Index).As_String;
                  begin
                     if Item /= Default then
                        Choices.Append (This.Values.Item (Index).As_String);
                     end if;
                  end;
               end loop;

               declare
                  Idx : constant Positive := Query_Multi (Prompt,
                                                          Choices);
               begin
                  return Choices (Idx);
               end;
            end;

         when Int =>
            declare
               Prompt : constant String :=
                 "Enter integer value for template parameter " &
               (+This.Name) & ", between " & Image (This.Int_First) &
                 " and " & Image (This.Int_Last);
            begin
               return Query_String (Prompt,
                                    Image (This.Default),
                                    Int_Is_Valid'Unrestricted_Access);
            end;
         when Real =>
            declare
               Prompt : constant String :=
                 "Enter real value for template parameter " &
               (+This.Name) & ", between " & Image (This.Real_First) &
                 " and " & Image (This.Real_Last);
            begin
               return Query_String (Prompt,
                                    Image (This.Default),
                                    Real_Is_Valid'Unrestricted_Access);
            end;

         when Bool =>
            return Query_String
              ("Enter boolean value for template parameter " &
               (+This.Name) & ", true, or false",
               Image (This.Default),
               Bool_Is_Valid'Unrestricted_Access);
      end case;
   end Query;

end Alire.Properties.Configurations.User_Input;
