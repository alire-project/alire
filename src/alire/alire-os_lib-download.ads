package Alire.OS_Lib.Download is

   function File
     (URL : String; Filename : Any_Path; Folder : Directory_Path)
      return Outcome;
   --  Download a single file using `curl`
   --
   --  Specifically, uses 'curl <URL> --location --silent --output <DEST>'
   --  (except that --silent may be replaced by --progress-bar depending on the
   --  log level).
   --
   --  Note that this doesn't check that 'curl' is available (calling code may
   --  wish to use 'Utils.Tools.Check_Tool'), nor that the destination file
   --  doesn't already exist.

   procedure Mark_Executable (Path : Any_Path);
   --  Mark a downloaded binary as executable. On macOS, also removes the
   --  quarantine attribute.

end Alire.OS_Lib.Download;
