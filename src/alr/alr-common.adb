with Alire.Index;
with Alire.Utils.TTY;

package body Alr.Common is

   --------------------
   -- Show_Providers --
   --------------------

   function Show_Providers (Dep : Alire.Dependencies.Dependency) return Boolean
   is
      use Alire;
   begin
      if Index.All_Crate_Aliases.Contains (Dep.Crate) then
         Trace.Always ("Crate " & Utils.TTY.Name (Dep.Crate)
                       & " is abstract and provided by:");
         for Provider of Index.All_Crate_Aliases.all (Dep.Crate) loop
            Trace.Always ("   " & Utils.TTY.Name (Provider));
         end loop;

         return True;
      else
         return False;
      end if;
   end Show_Providers;

end Alr.Common;
