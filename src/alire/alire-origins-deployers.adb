with Ada.Directories;

with Alire.Origins.Deployers.External;
with Alire.Origins.Deployers.Filesystem;
with Alire.Origins.Deployers.Git;
with Alire.Origins.Deployers.Hg;
with Alire.Origins.Deployers.Source_Archive;
with Alire.Origins.Deployers.System;
with Alire.Origins.Deployers.SVN;

package body Alire.Origins.Deployers is

   ------------------
   -- Compute_Hash --
   ------------------

   function Compute_Hash (This   : Deployer;
                          Folder : String;
                          Kind   : Hashes.Kinds) return Hashes.Any_Digest is
     (raise Program_Error with "Should not be called unless overriden");

   ------------------
   -- New_Deployer --
   ------------------

   function New_Deployer (From : Origin) return Deployer'Class is
   begin
      case From.Kind is
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

   function Deploy_Steps (From   : Origin;
                          Folder : String) return Outcome
   is
      The_Deployer  : constant Deployer'Class := New_Deployer (From);
      Result        : Outcome;
   begin

      --  1. Fetch sources
      Result := The_Deployer.Fetch (Folder);
      if not Result.Success then
         return Result;
      end if;

      --  2. Verify sources
      Result := The_Deployer.Verify_Hashes (Folder);
      if not Result.Success then
         return Result;
      end if;

      --  3. Deploy final sources
      return The_Deployer.Deploy (Folder);
   exception
      when E : others =>
         Log_Exception (E);
         if Ada.Directories.Exists (Folder) then
            Ada.Directories.Delete_Tree (Folder);
         end if;
         return Outcome_Failure ("Deployment of " & From.Image
                                 & " to " & Folder & " failed");
   end Deploy_Steps;

   ------------
   -- Deploy --
   ------------

   function Deploy (Release : Releases.Release;
                    Folder  : String := "") return Outcome
   is
   begin
      return Deploy_Steps (Release.Origin, Folder);
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
         if This.Base.Data.Hashes.Is_Empty then
            Trace.Warning ("No integrity hashes provided for "
                           & This.Base.Image);
            --  TODO: make this an error once all crates are updated with
            --  their hashes.
         else
            Trace.Detail ("Verifying integrity...");
         end if;

         --  Compute hashes from downloaded release and verify:
         for Index_Hash of This.Base.Data.Hashes loop
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
      when E : others =>
         --  May happen if Compute_Hash for some reason errs out.
         return Outcome_From_Exception
           (E,
            "Unexpected error while verify origin integrity"
            & " (use -d for details)");
   end Verify_Hashes;

end Alire.Origins.Deployers;
