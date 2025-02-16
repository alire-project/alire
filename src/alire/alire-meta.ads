package Alire.Meta with Preelaborate is

   --  Root package for information about Alire itself. Alire.Version could be
   --  moved here if at some point there is a reason good enough to expend the
   --  energy.

   package Working_Tree is

      Commit  : constant String := "unknown";
      Changes : constant String := "unknown";

      Main_Branch : constant String := "master";
      --  In case some day we rename the master branch in the repo

   end Working_Tree;

end Alire.Meta;
