with Alire.Properties.Labeled;
with Alire.TOML_Keys;

package body Alire.Externals.Lists is

   ------------
   -- Detect --
   ------------

   function Detect (This : List;
                    Name : Crate_Name;
                    Env  : Properties.Vector) return Containers.Release_Set is
   begin
      return Detected : Containers.Release_Set do
         for External of This loop
            if External.Available.Check (Env) then
               Trace.Debug ("Attempting detection of available external: "
                            & (+Name));
               Detected.Union (External.Detect (Name));
            else
               Trace.Debug ("Skipping detection of unavailable external: "
                            & (+Name));
            end if;
         end loop;
      end return;
   end Detect;

   -----------
   -- Hints --
   -----------

   function Hints (This : List;
                   Name : Crate_Name;
                   Env  : Properties.Vector := Properties.No_Properties)
                   return Utils.String_Vector
   is
      Hints : Properties.Vector;
      use type Properties.Vector;
   begin
      for External of This loop
         if Env.Is_Empty then

            --  Look for all hints, ignoring environment

            Hints := Hints and
              Properties.Filter
                (Conditional.Enumerate (External.Properties),
                 TOML_Keys.Hint);

         elsif
           External.Available.Check (Env) and then
           External.Detect (Name).Is_Empty
         then

            --  Look for externals in the platform that don't detect
            --  anything, so their hinting message is useful.

            Hints := Hints and
              Properties.Filter
                (Conditional.Enumerate (External.Properties.Evaluate (Env)),
                 TOML_Keys.Hint);
         end if;
      end loop;

      return Result : Utils.String_Vector do
         for Hint of Hints loop
            Result.Append (Properties.Labeled.Label'Class (Hint).Value);
         end loop;
      end return;
   end Hints;

end Alire.Externals.Lists;
