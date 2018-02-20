--  with Hello_Alr; -- Only in real in-project builds

with Alr.Hardcoded;

package Alr.Session is

   --  This file is used to determine the current working session (active project)
   --  This is only a placeholder; the actual file is generated for each build

   Hash : constant String := Hardcoded.Bootstrap_Hash;
   --  Hash of the project-alire.ads file.
   --  If it does not match the one computed on the fly, it means we must recompile

   --  Special values: bootstrap for a manually compiled build
   --  Any other not a hash: default session compiled without project

end Alr.Session;
