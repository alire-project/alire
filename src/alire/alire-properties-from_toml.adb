with AAA.Enum_Tools;

with Alire.Utils;

package body Alire.Properties.From_TOML is

   ------------
   -- Loader --
   ------------

   function Loader (From    : TOML_Adapters.Key_Queue;
                    Loaders : Loader_Array;
                    Section : Crates.Sections;
                    Strict  : Boolean)
                    return Conditional.Properties
   is
   begin
      return Props : Conditional.Properties do
         loop
            declare
               function Is_Valid is
                 new AAA.Enum_Tools.Is_Valid (Property_Keys);
               Val     : TOML.TOML_Value;
               Key     : constant String := From.Pop (Val);
               Prop    : Property_Keys;
               Ada_Key : constant String := TOML_Adapters.Adafy (Key);
            begin
               if Key = "" then
                  return; -- No more keys
               end if;

               --  Extract property name from string
               Trace.Debug ("Loading property key = " & Key);
               Process_Property : -- Single-pass loop to emulate Continue
               loop
                  if Is_Valid (Ada_Key) then
                     Prop := Property_Keys'Value (TOML_Adapters.Adafy (Key));

                     --  Check that the property is expected in this section.
                     if Loaders (Prop) in null then
                        From.Recoverable_Error
                          ("property '" & Key
                           & "' must not appear in section "
                           & Utils.To_Lower_Case (Section'Image));
                        exit Process_Property;
                     end if;

                     --  Load property once we know its exact name, allowing
                     --  expressions were appropriate.
                     Props.Append
                       (Prop_Loader.Load
                          (From    => From.Descend
                               (Key     => Key,
                                Value   => Val,
                                Context => Key),
                           Loader  => Loaders (Prop),
                           Resolve => Is_Dynamic (Prop),
                           Strict  => Strict));

                     --  REMOVE AFTER REFACTOR. Keep for now to reproduce same
                     --  messages
                     --  --  Divert to the expr resolver if prop can be dynamic
                     --  if Loaders_During_Case (Prop) /= null then
                     --     Props := Props and
                     --       TOML_Expressions.Cases.Load_Property
                     --         (Key    => Key,
                     --          From   => From.Descend (Val, "property"),
                     --          Loader => Loaders_During_Case (Prop));
                     --  else
                     --     --  Ensure no dynamic expression in the incoming
                     --         values
                     --     if TOML_Expressions.Contains_Expression (Val) then
                     --        From.Checked_Error
                     --          ("property '" & Key
                     --           & "' must not contain dynamic expressions");
                     --     end if;
                     --
                     --     --  Actually load the property:
                     --     Props := Props and
                     --       Loaders (Prop) (From.Descend (Key, Val, Key));
                     --  end if;

                  else
                     From.Recoverable_Error ("invalid property: " & Key);
                  end if;

                  exit Process_Property;
               end loop Process_Property;
            end;
         end loop;
      end return;
   end Loader;

end Alire.Properties.From_TOML;
