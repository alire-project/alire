-- TODO remove these dependency inversions
with Alr.Platform;
with Alr.Self;

with Alire.TOML_Index;

with Alr.Os_Lib;

package body Alire.Features.Index is

   --------------
   -- Load_All --
   --------------

   procedure Load_All is
   begin
      declare
         use Alr;
         use Alr.Os_Lib.Paths;

         Result : Alire.TOML_Index.Load_Result;
         Env    : Alire.TOML_Index.Environment_Maps.Map;
      begin
         Alire.TOML_Index.Set_Environment
           (Env,
            Platform.Distribution,
            Platform.Operating_System,
            Platform.Compiler);
         Alire.TOML_Index.Load_Catalog
           (Self.Src_Folder / "deps" / "alire" / "index",
            Env, Result);

         if not Result.Success then
            Trace.Error ("Error while loading the index:");
            Trace.Error (Alire.TOML_Index.Error_Message (Result));
            OS_Lib.Bailout (1);
         end if;
      end;
   end Load_All;

end Alire.Features.Index;
