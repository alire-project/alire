package Alire.TOML_Keys with Preelaborate is

   --  Constants for TOML keys in the index format

   Action         : constant String := "actions";
   Action_Type    : constant String := "type";
   Action_Command : constant String := "command";
   Action_Folder  : constant String := "directory";
   Archive_Name   : constant String := "archive-name";
   Author         : constant String := "authors";
   Available      : constant String := "available";
   Compiler       : constant String := "compiler";
   Dependency     : constant String := "depends-on";
   Description    : constant String := "description";
   Distribution   : constant String := "distribution";
   Executable     : constant String := "executables";
   External       : constant String := "external";
   Forbidden      : constant String := "forbids";
   General        : constant String := "general";
   GPR_Ext        : constant String := "gpr-externals";
   GPR_Set_Ext    : constant String := "gpr-set-externals";
   License        : constant String := "licenses";
   Long_Descr     : constant String := "long-description";
   Maintainer     : constant String := "maintainers";
   Maint_Logins   : constant String := "maintainers-logins";
   Notes          : constant String := "notes";
   Origin         : constant String := "origin";
   Origin_Hashes  : constant String := "origin-hashes";
   Origin_Source  : constant String := "archive-name";
   OS             : constant String := "os";
   Path           : constant String := "path";
   Project_File   : constant String := "project-files";
   Provides       : constant String := "provides";
   Tag            : constant String := "tags";
   Target         : constant String := "target";
   Website        : constant String := "website";
   Word_Size      : constant String := "word-size";

   --  Constants used elsewhere

   Index_URL      : constant String := "url";
   Index_Name     : constant String := "name";
   Index_Priority : constant String := "priority";

end Alire.TOML_Keys;
