package Alr.Commands.Withing is

   --  With resolution in Alire relies on two assumptions:
   --  All releases of a project are in the same spec file
   --  Every project has a "master entry" in their spec file which is a instance of the function
   --    Alire.Index.Catalogued_Project, indicating their name and parent (for subprojects)

   type Command is new Commands.Command with null record;

   overriding procedure Execute (Cmd : in out Command);

   overriding function Short_Description (Cmd : Command) return String is
     ("Locate project index file");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("<project>");

   function Locate_Package (Name : Alire.Project) return String;
   --  returns the package name for the project, based on the file that
   --  contains its releases.

   function With_Line (Name : Alire.Project) return String;
   --  The "with Alire.Index.Project;" full line

end Alr.Commands.Withing;
