with Alire.Utils.Tables;

package body Alire.Dependencies.Diffs is

   -------------
   -- Between --
   -------------

   function Between (Former, Latter : Containers.Lists.List) return Diff is
   begin
      return This : Diff do

         --  Identify new:

         for Dep of Latter loop
            if not (for some Old of Former => Dep = Old) then
               This.Added.Append (Dep);
            end if;
         end loop;

         --  Identify old:

         for Dep of Former loop
            if not (for some Novel of Latter => Dep = Novel) then
               This.Removed.Append (Dep);
            end if;
         end loop;

      end return;
   end Between;

   -------------
   -- Between --
   -------------

   function Between (Former, Latter : Conditional.Dependencies) return Diff is
     (Between (Conditional.Enumerate (Former),
               Conditional.Enumerate (Latter)));

   ----------------------
   -- Contains_Changes --
   ----------------------

   function Contains_Changes (This : Diff) return Boolean is
     (not (This.Added.Is_Empty and then This.Removed.Is_Empty));

   -----------
   -- Print --
   -----------

   procedure Print (This : Diff) is
      Table : Utils.Tables.Table;

      procedure Summarize (List    : Containers.Lists.List;
                           Comment : String;
                           Icon    : String)
      is
      begin
         for Dep of List loop
            Table
              .Append ("   " & Icon)
              .Append (+Dep.Crate)
              .Append (Dep.Versions.Image)
              .Append (Comment)
              .New_Row;
         end loop;
      end Summarize;

   begin
      if This.Contains_Changes then
         Summarize (This.Added,   "(added)",   "✓");
         Summarize (This.Removed, "(removed)", "✗");
         Table.Print (Info);
      else
         Trace.Info ("   No changes.");
      end if;
   end Print;

end Alire.Dependencies.Diffs;
