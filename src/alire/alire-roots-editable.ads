private with Ada.Finalization;

with Alire.Dependencies;
with Alire.Errors;
with Alire.Optional;
with Alire.TTY;

package Alire.Roots.Editable is

   --  This type mimics manual edition of the manifest. All operations in here
   --  modify the manifest as if it were done by hand.

   type Root (<>) is tagged limited private;

   function New_Root (Original : in out Roots.Root) return Root;
   --  This creates a temporary root with separate manifest and lockfile,
   --  but shared cache with the Original. Changes will be forgotten unless
   --  Confirm_And_Commit is called.

   procedure Confirm_And_Commit (This : in out Root);
   --  Present differences in the solutions
   --  of Original and Edited and ask the user to accept, in which case
   --  Edited.Commit is called. Edited is expected to be a temporary copy
   --  of Original. Performs an Edited.Reload_Manifest, so any changes done to
   --  the Edited manifest about dependency/pin adition/removal are applied.

   --  A few proxies so useful predicates can be kept

   function Name (This : Root) return Crate_Name;

   function Solution (This : in out Root) return Solutions.Solution;

   --  Edition procedures

   procedure Add_Dependency (This : in out Root;
                             Dep  : Dependencies.Dependency)
     with Pre =>
       not This.Solution.Depends_On (Dep.Crate)
       or else raise Checked_Error with Errors.Set
         (TTY.Name (Dep.Crate) & " is already a dependency of "
          & TTY.Name (This.Name));
   --  Add a dependency

   procedure Remove_Dependency (This  : in out Root;
                                Crate : Crate_Name;
                                Unpin : Boolean := True)
     with Pre =>
       This.Solution.Depends_On (Crate)
       or else raise Checked_Error with Errors.Set
         (TTY.Name (Crate) & " is not a dependency or pin of "
          & TTY.Name (This.Name));
   --  Remove any dependency (and pin) on crate

   function Can_Be_Pinned (This  : in out Root;
                           Crate : Crate_Name)
                           return Boolean
   is (not This.Solution.Depends_On (Crate)
       or else not This.Solution.State (Crate).Is_User_Pinned
       or else Force
       or else raise Checked_Error with Errors.Set
         (TTY.Name (Crate) & " is already pinned to "
          & This.Solution.State (Crate).Image));
   --  Says if a pin can be added: not already a pin, or Force. As an
   --  exception, the body is here as this function is intended to serve as
   --  a precondition, an hence serve as documentation.

   procedure Add_Version_Pin (This    : in out Root;
                              Crate   : Crate_Name;
                              Version : String)
     with Pre => This.Can_Be_Pinned (Crate);
   --  Add a version pin; if the root doesn't depend on it previously, the
   --  dependency will be added too.

   procedure Add_Path_Pin (This  : in out Root;
                           Crate : Optional.Crate_Name;
                           Path  : Any_Path)
     with Pre =>
       Crate.Is_Empty
       or else This.Can_Be_Pinned (Crate.Element.Ptr.all);
   --  Add a pin to a folder; if Crate.Is_Empty then Path must point to an
   --  Alire workspace from which it can be deduced. If Crate.Has_Element, the
   --  crates should match. If the root does not depend already on the crate,
   --  a dependency will be added.

   procedure Add_Remote_Pin (This   : in out Root;
                             Crate  : Optional.Crate_Name;
                             Origin : URL;
                             Commit : String := "";
                             Branch : String := "")
     with Pre =>
       (not (Commit /= "" and then Branch /= "")
        or else raise Checked_Error with
          Errors.Set ("Simultaneous Branch and Commit pins are incompatible"))
       and then
         (Crate.Is_Empty
          or else This.Can_Be_Pinned (Crate.Element.Ptr.all));
   --  Add a pin to a remote repo, with optional Commit xor Branch. if
   --  Crate.Is_Empty then Path must point to an Alire workspace for which it
   --  can be deduced. If Crate.Has_Element, the crates should match. If the
   --  root does not depend already on the crate, a dependency will be added.

   procedure Remove_Pin (This : in out Root; Crate : Crate_Name);
   --  Remove the pin for Crate if existing; otherwise do nothing

private

   type Root is new Ada.Finalization.Limited_Controlled with record
      Orig : Roots.Root;
      Edit : Roots.Root;
   end record;

   overriding procedure Finalize (This : in out Root);

   ----------
   -- Name --
   ----------

   function Name (This : Root) return Crate_Name
   is (This.Edit.Name);

   --------------
   -- Solution --
   --------------

   function Solution (This : in out Root) return Solutions.Solution
   is (This.Edit.Solution);

end Alire.Roots.Editable;
