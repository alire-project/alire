with Alire.OS_Lib;
with Alire.TOML_Index;

package body Alire.Features.Index is

   --------------
   -- Load_All --
   --------------

   procedure Load_All (Platform : Environment.Setup;
                       From    : Absolute_Path) is
   begin
      declare
         Result : Alire.TOML_Index.Load_Result;
         Env    : Alire.TOML_Index.Environment_Variables;
      begin
         Alire.TOML_Index.Set_Environment
           (Env,
            Platform.Distro,
            Platform.OS,
            Platform.Compiler);
         Alire.TOML_Index.Load_Catalog
           --  TODO: use configured indexes with `alr index`
           (From, Env, Result);

         if not Result.Success then
            Trace.Error ("Error while loading the index:");
            Trace.Error (Alire.TOML_Index.Error_Message (Result));
            OS_Lib.Bailout (1);
         end if;
      end;
   end Load_All;

end Alire.Features.Index;
