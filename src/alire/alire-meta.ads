package Alire.Meta with Preelaborate is

   --  Root package for information about Alire itself. Alire.Version could be
   --  moved here if at some point there is a reason good enough to expend the
   --  energy.

   package Working_Tree is

      Commit  : constant String := "64e6d326732ffbe0ccdb9276fdfe73e85beaeaeb";
      Changes : constant String := "dirty";

      Main_Branch : constant String := "master";
      --  In case some day we rename the master branch in the repo

   end Working_Tree;

end Alire.Meta;
