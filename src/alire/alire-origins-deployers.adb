with Ada.Directories;

with Alire.Directories;
with Alire.Origins.Deployers.External;
with Alire.Origins.Deployers.Filesystem;
with Alire.Origins.Deployers.Git;
with Alire.Origins.Deployers.Hg;
with Alire.Origins.Deployers.Source_Archive;
with Alire.Origins.Deployers.System;
with Alire.Origins.Deployers.SVN;

with GNAT.OS_Lib;

package body Alire.Origins.Deployers is

   ------------------
   -- Compute_Hash --
   ------------------

   function Compute_Hash (This   : Deployer;
                          Folder : String;
                          Kind   : Hashes.Kinds) return Hashes.Any_Digest is
     (raise Program_Error with "Should not be called unless overridden");

   ------------------
   -- New_Deployer --
   ------------------

   function New_Deployer (From : Origin) return Deployer'Class is
   begin
      case From.Kind is
         when Origins.Binary_Archive =>
            --  We can reuse the Source_Archive.Deployer
            return Source_Archive.Deployer'(Deployer'(Base => From)
                                            with null record);
         when Origins.External =>
            return External.Deployer'(Deployer'(Base => From)
                                      with null record);

         when Origins.Filesystem =>
            return Filesystem.Deployer'(Deployer'(Base => From)
                                        with null record);
         when Origins.Git =>
            return Git.Deployer'(Deployer'(Base => From) with null record);

         when Alire.Origins.Hg =>
            return Hg.Deployer'(Deployer'(Base => From) with null record);

         when Alire.Origins.SVN =>
            return SVN.Deployer'(Deployer'(Base => From) with null record);

         when Alire.Origins.Source_Archive =>
            return Source_Archive.Deployer'(Deployer'(Base => From)
                                            with null record);
         when Alire.Origins.System =>
            return System.Platform_Deployer (From);
      end case;
   end New_Deployer;

   ------------------
   -- Deploy_Steps --
   ------------------

   function Deploy_Steps (Rel    : Releases.Release;
                          Folder : String) return Outcome
   is
      use Directories.Operators;

      ----------------------
      -- Create_Info_File --
      ----------------------

      procedure Create_Info_File is
         Parent   : constant String :=
                      Ada.Directories.Containing_Directory (Folder);
         Location : constant String :=
                      Ada.Directories.Simple_Name (Folder);
         Filename : constant String :=
                      Parent
                        / (Rel.Name_Str & "_"
                           & AAA.Strings.Head
                             (AAA.Strings.Head (Rel.Version.Image, '-'), '+')
                           & "_in_" & Location);
         use GNAT.OS_Lib;
         Success : Boolean;
      begin
         Close (Create_File (Filename, Text), Success);
         --  This is merely informative, so any error can be silently ignored
         --  (we do test for this in the thest suite though).
         if not Success then
            Trace.Debug ("Creation of info file failed for: " & Filename);
         end if;
      end Create_Info_File;

      From : constant Origin := Rel.Origin;
      Temp_Dir      : Directories.Temp_File :=
                        Directories.With_Name
                          ((if Folder /= "" -- Empty for system releases
                            then Ada.Directories.Containing_Directory (Folder)
                            else Directories.Current)
                           / Directories.Temp_Name);
      --  We use a temporary location to fetch and verify, as otherwise any
      --  failure before final deployment may result in considering a crate
      --  already deployed.

      The_Deployer  : constant Deployer'Class := New_Deployer (From);
      Result        : Outcome;
   begin

      --  1. Fetch sources
      Result := The_Deployer.Fetch (Temp_Dir.Filename);
      if not Result.Success then
         return Result;
      end if;

      --  2. Verify sources
      Result := The_Deployer.Verify_Hashes (Temp_Dir.Filename);
      if not Result.Success then
         return Result;
      end if;

      --  3. Deploy final sources
      The_Deployer.Deploy (Temp_Dir.Filename).Assert;

      --  4. Rename into final location. This is always in the same drive (as
      --  we created the temporary as a sibling of the final location) so it
      --  should be an instant operation. We check for the folder existence
      --  as some deployers may not need one (like system packages).
      Temp_Dir.Keep;
      if Ada.Directories.Exists (Temp_Dir.Filename) then
         Trace.Debug ("Renaming into place " & TTY.URL (Temp_Dir.Filename)
                      & " as " & TTY.URL (Folder));
         Ada.Directories.Rename (Old_Name => Temp_Dir.Filename,
                                 New_Name => Folder);
      end if;

      --  Add an info file for monorepos to make explicit where a release is
      if From.Is_Monorepo then
         Create_Info_File;
      end if;

      return Outcome_Success;
   exception
      when E : others =>
         Log_Exception (E);
         if Folder /= "" and then Ada.Directories.Exists (Folder) then
            Ada.Directories.Delete_Tree (Folder);
         end if;
         return Outcome_Failure ("Deployment of " & From.Image
                                 & (if From.Is_System
                                    then " from system packages"
                                    else " to " & Folder)
                                 & " failed");
   end Deploy_Steps;

   ------------
   -- Deploy --
   ------------

   function Deploy (Release : Releases.Release;
                    Folder  : String := "") return Outcome
   is
   begin
      return Deploy_Steps (Release, Folder);
   end Deploy;

   ------------
   -- Deploy --
   ------------

   function Deploy (This : Deployer; Folder : String) return Outcome
   is (raise Program_Error with "should never be called for base class");

   -----------
   -- Fetch --
   -----------

   function Fetch (This   : Deployer; Folder : String) return Outcome
   is (raise Program_Error with "should never be called for base class");

   -------------------
   -- Verify_Hashes --
   -------------------

   function Verify_Hashes (This   : Deployer'Class;
                           Folder : String) return Outcome is
   begin
      if This.Supports_Hashing then

         --  Emit a note if we might profit from hashes:
         if This.Base.Get_Hashes.Is_Empty then
            Trace.Warning ("No integrity hashes provided for "
                           & This.Base.Image);
            --  TODO: make this an error once all crates are updated with
            --  their hashes.
         else
            Trace.Detail ("Verifying integrity...");
         end if;

         --  Compute hashes from downloaded release and verify:
         for Index_Hash of This.Base.Get_Hashes loop
            Trace.Debug ("Computing " & Hashes.Kind (Index_Hash)'Img & "...");
            declare
               use type Hashes.Any_Digest;
               Local_Digest : constant Hashes.Any_Digest :=
                                This.Compute_Hash (Folder,
                                                   Hashes.Kind (Index_Hash));
               Local_Hash   : constant Hashes.Any_Hash :=
                                Hashes.New_Hash (Hashes.Kind (Index_Hash),
                                                 Local_Digest);
            begin
               if Hashes.Digest (Index_Hash) /= Local_Digest then
                  return Outcome_Failure
                    ("release integrity test failed: "
                     & "expected ["  & String (Index_Hash)
                     & "] but got [" & String (Local_Hash) & "]");
               end if;
            end;
         end loop;
      end if;

      return Outcome_Success;
   exception
      when E : Checked_Error =>
         return Outcome_From_Exception (E);
      when E : others =>
         --  May happen if Compute_Hash for some reason errs out.
         return Outcome_From_Exception
           (E,
            "Unexpected error while verify origin integrity"
            & " (use -d for details)");
   end Verify_Hashes;

end Alire.Origins.Deployers;
