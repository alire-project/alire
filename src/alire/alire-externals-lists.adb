with Alire.Properties.Labeled;
with Alire.TOML_Keys;

with Simple_Logging;

package body Alire.Externals.Lists is

   ------------
   -- Detect --
   ------------

   function Detect (This : List;
                    Name : Crate_Name;
                    Env  : Properties.Vector)
                    return Releases.Containers.Release_Set
   is
   begin

      --  Avoid the log message if there's nothing to detect

      if This.Is_Empty then
         return (Releases.Containers.Release_Sets.Empty_Set with null record);
      end if;

      declare
         Busy : Simple_Logging.Ongoing := Simple_Logging.Activity
           ("Looking for external crate: " & (+Name));
      begin
         return Detected : Releases.Containers.Release_Set do
            for External of This loop
               if External.Available.Is_Available (Env) then
                  Trace.Debug ("Attempting detection of available external: "
                               & (+Name));
                  Detected.Union (External.Detect (Name));
               else
                  Trace.Debug ("Skipping detection of unavailable external: "
                               & (+Name));
               end if;

               Busy.Step;
            end loop;
         end return;
      end;
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
           External.Available.Is_Available (Env) and then
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

   -------------
   -- To_List --
   -------------

   function To_List (This : External'Class) return List is
   begin
      return L : List do
         L.Append (This);
      end return;
   end To_List;

end Alire.Externals.Lists;
