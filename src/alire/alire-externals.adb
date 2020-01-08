with TOML;

package body Alire.Externals is

   ------------
   -- Detect --
   ------------

   function Detect (This : List;
                    Name : Crate_Name) return Containers.Release_Set is
   begin
      return Detected : Containers.Release_Set do
         for External of This loop
            Detected.Union (External.Detect (Name));
         end loop;
      end return;
   end Detect;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return External'Class is
     (raise Unimplemented); -- No concrete externals defined yet

end Alire.Externals;
