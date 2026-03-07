private with Ada.Finalization;

with Alire.Dependencies;
with Alire.Errors;
with Alire.Optional;
with Alire.Utils.TTY;

with Semantic_Versioning;

package Alire.Roots.Editable is

   --  This type mimics manual edition of the manifest. All operations in here
   --  modify the manifest as if it were done by hand.

   type Root (<>) is tagged limited private;

   function New_Root (Original : in out Roots.Root) return Root;
   --  This creates a temporary root with separate manifest and lockfile,
   --  but shared cache with the Original. Changes will be forgotten unless
   --  Confirm_And_Commit is called.

   procedure Confirm_And_Commit (This : in out Root);
   --  Present differences in the solutions of Original and Edited and ask
   --  the user to accept, in which case Edited.Commit is called. Edited
   --  is expected to be a temporary copy of Original. Performs an
   --  Edited.Reload_Manifest, so any changes done to the Edited manifest
   --  about dependency/pin addition/removal are applied.

   --  A few proxies so useful predicates can be kept

   function Old (This : Root) return Roots.Root;
   --  The original root this editable copy was made from

   type Reference (Element : not null access Roots.Root)
   is limited null record with Implicit_Dereference => Element;

   function Current (This : in out Root) return Reference;
   --  Retrieve the temporary copy. This is read/write because the caching of
   --  solutions requires it, but it is intended for read-only use.

   function Name (This : Root) return Crate_Name;

   function Solution (This : in out Root) return Solutions.Solution;

   procedure Set (This : in out Root; Solution : Solutions.Solution);
   --  Bulk replace the solution in the temporary copy

   --  Edition procedures

   procedure Reload_Manifest (This : in out Root);
   --  If changes have been done to the manifest, either via the dependency/pin
   --  modification procedures, or somehow outside alire after This was
   --  created, we need to reload the manifest. The solution remains
   --  untouched (use Update to recompute a fresh solution).

   procedure Add_Dependency (This          : in out Root;
                             Dep           : Dependencies.Dependency;
                             Allow_Unknown : Boolean := Alire.Force);
   --  Add a dependency, or raise Checked_Error is Dep is already among direct
   --  dependencies. Recoverable error if Dep is unknown, unless Allow_Unknown.

   procedure Remove_Dependency (This  : in out Root;
                                Crate : Crate_Name;
                                Unpin : Boolean := True);
   --  Remove any dependency (and pin) on crate; will raise Checked_Error if
   --  Crate is not a dependency already.

   function Can_Be_Pinned (This  : in out Root;
                           Crate : Crate_Name)
                           return Boolean
   is (not Release (This.Old).Pins.Contains (Crate)
       or else Force
       or else raise Checked_Error with Errors.Set
         (Utils.TTY.Name (Crate) & " is already pinned with pin "
          & Release (This.Old).Pins.Element (Crate).Image (User => False)));
   --  Says if a pin can be added: not already a pin, or Force. As an
   --  exception, the body is here as this function is intended to serve as
   --  a precondition, an hence serve as documentation.

   procedure Add_Version_Pin (This    : in out Root;
                              Crate   : Crate_Name;
                              Version : Semantic_Versioning.Version)
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
                             Ref    : String := "";
                             Branch : String := "";
                             Subdir : Relative_Path := "")
     with Pre =>
       (not (Ref /= "" and then Branch /= "")
        or else raise Checked_Error with
          Errors.Set ("Simultaneous Branch and Ref pins are incompatible"))
       and then
         (Crate.Is_Empty
          or else This.Can_Be_Pinned (Crate.Element));
   --  Add a pin to a remote repo, with optional Ref xor Branch. If Ref is
   --  not a Commit, it will be converted to one using `git ls-remote`. If
   --  Crate.Is_Empty then Origin must point to an Alire workspace for which it
   --  can be deduced. If Crate.Has_Element, the crates should match. If the
   --  root does not depend already on the crate, a dependency will be added.

   procedure Remove_Pin (This : in out Root; Crate : Crate_Name);
   --  Remove the pin for Crate if existing; otherwise do nothing

private

   type Root is new Ada.Finalization.Limited_Controlled with record
      Orig : Roots.Root;
      Edit : aliased Roots.Root;
   end record;

   overriding procedure Finalize (This : in out Root);

   -------------
   -- Current --
   -------------

   function Current (This : in out Root) return Reference
   is (Element => This.Edit'Unrestricted_Access);
   --  CE2021 is happy with 'Access but 9.3 complains about a dangling pointer.
   --  We are returning a short-lived pointer to a limited value so I don't see
   --  the problem.

   ----------
   -- Name --
   ----------

   function Name (This : Root) return Crate_Name
   is (This.Edit.Name);

   ---------
   -- Old --
   ---------

   function Old (This : Root) return Roots.Root
   is (This.Orig);

   --------------
   -- Solution --
   --------------

   function Solution (This : in out Root) return Solutions.Solution
   is (This.Edit.Solution);

end Alire.Roots.Editable;
