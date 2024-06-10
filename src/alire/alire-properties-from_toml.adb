with AAA.Enum_Tools;
with Alire.Utils.Did_You_Mean;

package body Alire.Properties.From_TOML is

   function Property_Keys_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Property_Keys, Utils.Did_You_Mean.Tomify);

   ------------
   -- Loader --
   ------------

   function Loader (From    : TOML_Adapters.Key_Queue;
                    Loaders : Loader_Array;
                    Section : Crates.Sections;
                    Strict  : Boolean)
                    return Conditional.Properties
   is
      Props : Conditional.Properties;
   begin
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
               return Props; -- No more keys
            end if;

            --  Extract property name from string

            Process_Property : -- Single-pass loop to emulate Continue
            loop
               if Is_Valid (Ada_Key) then
                  Prop := Property_Keys'Value (TOML_Adapters.Adafy (Key));

                  --  Check that the property is expected in this section.
                  if Loaders (Prop) in null then
                     From.Recoverable_Error
                       ("property '" & Key
                        & "' must not appear in section "
                        & AAA.Strings.To_Lower_Case (Section'Image));
                     exit Process_Property;
                  end if;

                  --  If the property is an array of tables (e.g. actions),
                  --  reconstruct items as a single table to redispatch.
                  --  This ensures that properties that can have dynamic
                  --  expressions within each array entry are transparently
                  --  managed before they reach the actual property loader.

                  if Val.Kind = TOML_Array
                    and then
                      (for all I in 1 .. Val.Length =>
                         Val.Item (I).Kind = TOML_Table)
                  then

                     --  Load each array entry individually

                     for I in 1 .. Val.Length loop
                        Props.Append
                          (Prop_Loader.Load
                             (From    => From.Descend
                                  (Key     => Key,
                                   Value   => Val.Item (I),
                                   Context => Key & " (array item"
                                   & I'Image & ")"),
                              Loader  => Loaders (Prop),
                              Resolve => Is_Dynamic (Prop),
                              Strict  => Strict));
                     end loop;

                  else

                     --  Load a single property once we know its exact name,
                     --  allowing expressions were appropriate.

                     Props.Append
                       (Prop_Loader.Load
                          (From    => From.Descend
                               (Key     => Key,
                                Value   => Val,
                                Context => Key),
                           Loader  => Loaders (Prop),
                           Resolve => Is_Dynamic (Prop),
                           Strict  => Strict));

                  end if;

               else
                  From.Recoverable_Error
                    ("invalid property: '" & Key &
                       "'." & Property_Keys_Suggestion (Key));
               end if;

               exit Process_Property;
            end loop Process_Property;
         end;
      end loop;
   end Loader;

end Alire.Properties.From_TOML;
