with Alr.Session;

package body Alr.Project is

   ----------------------
   -- Set_Root_Project --
   ----------------------

   function Set_Root_Project (Project    : Alire.Project_Name;
                              Version    : Semantic_Versioning.Version;
                              Depends_On : Alire.Depends.Dependencies := Alire.Depends.Nothing;
                              License    : Alire.Licenses := Unknown)
                              return Release
   is
      Rel : constant Release := Alire.Index.Register_Local (Project,
                                                            Version,
                                                            Depends_On,
                                                            License);
   begin
      Alr.Session.Current_Project.Replace_Element (Rel);

      return Rel;
   end Set_Root_Project;

end Alr.Project;
