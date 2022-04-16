with Ada.Containers.Indefinite_Ordered_Sets;

package Alire.Index_On_Disk.Loading is

   -------------------
   -- Index loading --
   -------------------

   package Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Index_On_Disk.Index'Class, Index_On_Disk."<", Index_On_Disk."=");

   type Set is new Sets.Set with null record;

   function Default return Set;

   function Find_All
     (Under  : Absolute_Path;
      Result : out Outcome) return Set;
   --  Find all indexes available on a disk location. If valid indexes are
   --  found or none, set Result to Outcome_Success and return the
   --  corresponding set. If at least one found index is invalid, set Result to
   --  an outcome failure and return en empty set.
   --
   --  We abort at the first invalid index as it's likely in this case that
   --  users misconfigured something, so this helps them notice the issue
   --  instead of proceeding with default behaviors, such as getting the
   --  community index.

   procedure Setup_And_Load (From   : Absolute_Path;
                             Strict : Boolean;
                             Force  : Boolean := False);
   --  If there are no crates loaded, load from all configured indexes at the
   --  configured location. If Force, load even if some crates are already
   --  loaded. If no index is configured, set up the default community index.

   function Load_All (From : Absolute_Path; Strict : Boolean) return Outcome;
   --  Load all indexes available at the given location

   procedure Load (Crate            : Crate_Name;
                   Detect_Externals : Boolean;
                   Strict           : Boolean;
                   From             : Set := Default;
                   Path             : Any_Path := "")
   with Pre => Path = "" or else From.Is_Empty;
   --  Load a single crate, optionally detecting its externals. If a set of
   --  already detected indexes is provided, detection is not reattempted.
   --  If no path for detection is given, default one is used.
   --  May raise Checked_Error. Loading twice the same crate is idempotent.

   function Update_All (Under : Absolute_Path) return Outcome;
   --  Find and update all indexes at given location

   function Add (Origin : URL;
                 Name   : String;
                 Under  : Absolute_Path;
                 Before : String := "") return Outcome;
   --  Add a new remote index under a folder that possibly contains more
   --    indexes in child folders. That is, Under is the parent of all indexes
   --  Index will be set as last one, or before given index name

   function Add_Or_Reset_Community return Outcome;
   --  Adds the community index, if not already configured. If configured,
   --  re-adds it at the required branch by Index.Community_Branch with the
   --  same priority (i.e., maintaining the relative ordering);

private

   function Default return Set is (Empty);
   --  Workaround for a visibility bug that manifests in body otherwise

end Alire.Index_On_Disk.Loading;
