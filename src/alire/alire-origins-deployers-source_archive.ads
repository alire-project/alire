package Alire.Origins.Deployers.Source_Archive is

   type Deployer is new Deployers.Deployer with null record;

   overriding
   function Fetch (This   : Deployer; Folder : String) return Outcome;

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome;

   overriding
   function Compute_Hash (This   : Deployer;
                          Folder : String;
                          Kind   : Hashes.Kinds) return Hashes.Any_Digest;

   overriding
   function Supports_Hashing (This : Deployer) return Boolean is (True);

   procedure Unpack (Src_File : String;
                     Dst_Dir  : String;
                     Delete   : Boolean;
                     Move_Up  : Boolean);
   --  Unpack a file in one of the known formats into the destination dir. If
   --  Delete, remove the Src_File after unpacking. If Move_Up, then if Src
   --  contains a single root folder, its contents are moved up one level
   --  into Dst_Dir. Otherwise, Checked_Error is raised.

end Alire.Origins.Deployers.Source_Archive;
