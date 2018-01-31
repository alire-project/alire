--  with Hello_Alr; -- Only in real in-project builds

package Alr.Session is
   
   --  This file is used to determine the current working session (active project)
   --  This is only a placeholder; the actual file is generated for each build

   Hash : constant String := "none";
   --  Hash of the project-alire.ads file. 
   --  If it does not match the one computed on the fly, it means we must recompile

end Alr.Session;
