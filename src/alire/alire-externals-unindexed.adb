with Alire.Externals.Lists;

package body Alire.Externals.Unindexed is

   ------------
   -- Detail --
   ------------

   overriding
   function Detail (This          : External;
                    Unused_Distro : Platforms.Distributions)
                    return AAA.Strings.Vector
   is
      Result : AAA.Strings.Vector;
   begin
      for Hint of Lists.To_List (This).Hints (+"unused_name") loop
         Result.Append (Hint);
      end loop;

      if Result.Is_Empty then
         return AAA.Strings.Empty_Vector.Append
           ("Must be provided by the user");
      else
         return Result;
      end if;
   end Detail;

end Alire.Externals.Unindexed;
