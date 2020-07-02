with Alire.Utils;
with Alire;

package Alr.Utils.Auto_GPR_With is

   procedure Update (GPR_File : Alire.Absolute_Path;
                     Withs    : Alire.Utils.String_Set);
   --  Update the list of with statement in project file.
   --
   --  This is done in a specific section of the project file guarded by tokens
   --  in comments. The previous list of "auto-withs" is completely removed and
   --  replaced by the one provided in the Withs String_Set.

end Alr.Utils.Auto_GPR_With;
