with Alire.Conditional;

package body Alire.Properties is

   ------------
   -- Filter --
   ------------

   function Filter (V : Vector; Ancestor : Ada.Tags.Tag) return Vector is
      Result : Vector := No_Properties;
   begin
      for Prop of V loop
         if Ada.Tags.Is_Descendant_At_Same_Level (Prop'Tag, Ancestor) then
            Result.Append (Prop);
         end if;
      end loop;

      return Result;
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter (V : Vector; Key : String) return Vector is
      Result : Vector := No_Properties;
   begin
      for Prop of V loop
         if Prop.Key = Key then
            Result.Append (Prop);
         end if;
      end loop;

      return Result;
   end Filter;

   -------------
   -- To_TOML --
   -------------

   overriding function To_TOML (V : Vector) return TOML.TOML_Value is
      use Conditional.For_Properties;
      Tree : Conditional.Properties;
   begin
      --  Convert to a conditional vector to reuse the export function there,
      --  which is more general and takes property cardinalities into account.
      for Prop of V loop
         Tree := Tree and Prop;
      end loop;

      return Tree.To_TOML;
   end To_TOML;

end Alire.Properties;
