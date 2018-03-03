with Alire.Index;
with Alire.Properties;

with Alr.Hardcoded;
with Alr.Origins;
with Alr.OS;
with Alr.Query;

package Alr.Checkout is

   Platform_Properties : constant Alire.Properties.Vector := OS.Properties;
   --  Cached to not query the OS repeatedly

   function Available_Currently (R : Alire.Index.Release) return Boolean;
   --  The release knows the requisites on the platform; here we evaluate these against the current platform
   --  Current checks include the "available" requisites and that the native package do exist

   type Policies is (Overwrite, Skip, Error);
   --  What to do when checking out to something that already exists

   procedure Generate_GPR_Builder (Root : Alire.Index.Release);
   --  Resolves dependencies and generates builder file

   procedure Generate_GPR_Builder (Depends : Query.Instance; Root : Alire.Index.Release);
   --  Generates the project_alr.gpr for given dependencies (e.g. found by Upgrade)

   procedure Working_Copy (R              : Alire.Index.Release;
                           Deps           : Query.Instance;
                           Parent_Folder  : String;
                           Generate_Files : Boolean := True;
                           If_Conflict    : Policies := Skip);
   --  A working copy might not have alr and gpr files, that will be generated if needed

   procedure To_Folder (Projects : Query.Instance;
                        Parent   : String := Hardcoded.Projects_Folder;
                        But      : Alire.Project_Name := "");
   --  Retrieves all releases into a folder, typically the main cache
   --  One project in the solution (typically the root project itself) can be ignored

private

   function Available_Currently (R : Alire.Index.Release) return Boolean is
     (R.Available.Check (Platform_Properties) and then
          (if R.Origin.Is_Native
           then Origins.Native_Package_Exists (R.Origin.Package_Name (OS.Distribution))));

end Alr.Checkout;
