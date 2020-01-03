with Alire.VCSs.Git;

package body Alire.Index_On_Disk.Git is

   ---------
   -- Add --
   ---------

   overriding
   function Add (This : Index) return Outcome is
     (VCSs.Git.Handler.Clone
        (VCSs.Repo_And_Commit (This.Origin), This.Index_Directory));

   -----------------
   -- New_Handler --
   -----------------

   overriding
   function New_Handler (Origin : URL;
                         Name   : Restricted_Name;
                         Parent : Any_Path)
                         return Index
   is
   begin
      return Idx : constant Index :=
        Index'(URL_Len    => Origin'Length,
               Name_Len   => Name'Length,
               Dir_Len    => Parent'Length,
               Origin     => Origin,
               Name       => Name,
               Parent     => Parent,
               Priority   => <>);
   end New_Handler;

   ------------
   -- Update --
   ------------

   overriding
   function Update (This : Index) return Outcome is
   begin
      if VCSs.Git.Handler.Is_Detached (This.Index_Directory) then
         --  Trying to pull from a detached repo is a failure
         Trace.Detail ("Skipping update of detached index: " & This.Name);
         return Outcome_Success;
      else
         Trace.Detail ("Updating index: " & This.Name);
         return VCSs.Git.Handler.Update (This.Index_Directory);
      end if;
   end Update;

end Alire.Index_On_Disk.Git;
