with Alire.Projects;

package Alr.Commands.Withing is

   type Command is new Commands.Command with null record;

   overriding procedure Execute (Cmd : in out Command);

   overriding function Short_Description (Cmd : Command) return String is
     ("Locate project index file");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("<project>");

   function Locate_Package (Name : Alire.Projects.Names) return String;
   --  returns the package name for the project, based on the file that
   --  contains its releases.
   --  This makes the implicit assumption that all releases are in a same file

end Alr.Commands.Withing;
