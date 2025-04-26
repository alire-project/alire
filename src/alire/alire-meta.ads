package Alire.Meta with Preelaborate is

   --  Root package for information about Alire itself. Alire.Version could be
   --  moved here if at some point there is a reason good enough to expend the
   --  energy.

   package Working_Tree is

      Commit  : constant String := "7507d3fff57dfa4916e81aa80a876e9cb1e06908";
      Changes : constant String := "dirty";

      Main_Branch : constant String := "master";
      --  In case some day we rename the master branch in the repo

   end Working_Tree;

end Alire.Meta;
