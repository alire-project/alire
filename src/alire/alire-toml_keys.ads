package Alire.TOML_Keys with Preelaborate is

   --  Constants for TOML keys in the index format

   Action         : constant String := "actions";
   Action_Type    : constant String := "type";
   Action_Command : constant String := "command";
   Action_Folder  : constant String := "directory";
   Author         : constant String := "authors";
   Auto_GPR_With  : constant String := "auto-gpr-with";
   Available      : constant String := "available";
   Compiler       : constant String := "compiler";
   Configuration  : constant String := "configuration";
   Config_Vars    : constant String := "configuration.variables";
   Config_Values  : constant String := "configuration.values";
   Depends_On     : constant String := "depends-on";
   Description    : constant String := "description";
   Distribution   : constant String := "distribution";
   Environment    : constant String := "environment";
   Executable     : constant String := "executables";
   External       : constant String := "external";
   External_Kind  : constant String := "kind";
   Forbidden      : constant String := "forbids";
   General        : constant String := "general";
   GPR_Ext        : constant String := "gpr-externals";
   GPR_Set_Ext    : constant String := "gpr-set-externals";
   Hint           : constant String := "hint";
   License        : constant String := "licenses";
   Long_Descr     : constant String := "long-description";
   Maintainer     : constant String := "maintainers";
   Maint_Logins   : constant String := "maintainers-logins";
   Name           : constant String := "name";
   Notes          : constant String := "notes";
   Origin         : constant String := "origin";
   OS             : constant String := "os";
   Path           : constant String := "path";
   Pinned         : constant String := "pinned";
   Project_File   : constant String := "project-files";
   Provides       : constant String := "provides";
   Tag            : constant String := "tags";
   Target         : constant String := "target";
   Toolchain      : constant String := "toolchain";
   Version        : constant String := "version";
   Version_Cmd    : constant String := "version-command";
   Version_Regexp : constant String := "version-regexp";
   Website        : constant String := "website";
   Word_Size      : constant String := "word-size";

   --  Constants used elsewhere

   Index_URL      : constant String := "url";
   Index_Name     : constant String := "name";
   Index_Priority : constant String := "priority";

end Alire.TOML_Keys;
