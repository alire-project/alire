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

end Alire.Externals.Lists;
