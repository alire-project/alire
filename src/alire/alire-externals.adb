package body Alire.Externals is

   ------------
   -- Detect --
   ------------

   function Detect (This : List) return Containers.Release_Set is
   begin
      return Detected : Containers.Release_Set do
         for External of This loop
            Detected.Union (External.Detect);
         end loop;
      end return;
   end Detect;

end Alire.Externals;
