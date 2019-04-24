package Alire.TOML_Keys with Preelaborate is

   --  Constants for TOML keys in the index format

   Action       : constant String := "actions";
   Archive_Name : constant String := "archive-name";
   Author       : constant String := "authors";
   Comment      : constant String := "comments";
   Compiler     : constant String := "compiler";
   Dependency   : constant String := "depends-on";
   Description  : constant String := "description";  
   Distribution : constant String := "distribution";
   Executable   : constant String := "executables";
   Forbidden    : constant String := "forbids";
   General      : constant String := "general";   
   GPR_Ext      : constant String := "gpr-externals";
   GPR_Set_Ext  : constant String := "gpr-set-externals";
   License      : constant String := "licenses";
   Maintainer   : constant String := "maintainers";
   Notes        : constant String := "notes";
   Origin       : constant String := "origin";
   OS           : constant String := "os";
   Path         : constant String := "path";
   Project_File : constant String := "project-files";
   Provides     : constant String := "provides";
   Target       : constant String := "target";
   Website      : constant String := "website";
   Word_Size    : constant String := "word-size";
   
end Alire.TOML_Keys;
