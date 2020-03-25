
package Alire.OS_Lib.Download is

   function File (URL      : String;
                  Filename : Any_Path;
                  Folder   : Directory_Path)
                  return Outcome;

end Alire.OS_Lib.Download;
