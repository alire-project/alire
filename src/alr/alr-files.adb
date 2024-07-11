with Den;

package body Alr.Files is

   -------------------------
   -- Locate_Any_GPR_File --
   -------------------------

   function Locate_Any_GPR_File return Natural is

      Candidates : AAA.Strings.Vector;

      -----------
      -- Check --
      -----------

      procedure Check (File : Alire.Any_Path; Stop : in out Boolean) is
         use AAA.Strings;
      begin
         Stop := False;
         if Den.Kind (File) in Den.File
           and then Has_Suffix (To_Lower_Case (File), ".gpr")
         then
            Candidates.Append (Den.Full (File));
         end if;
      end Check;
   begin
      Alire.Directories.Traverse_Tree
        (Alire.Directories.Current,
         Check'Access);

      return Natural (Candidates.Length);
   end Locate_Any_GPR_File;

end Alr.Files;
