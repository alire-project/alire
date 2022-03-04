separate (Alire)
function Check_Absolute_Path (Path : Any_Path) return Boolean is
begin
   return (Path'Length >= 3
           and then Path (Path'First) in 'A' .. 'Z' | 'a' .. 'z'
           and then Path (Path'First + 1) = ':'
           and then Path (Path'First + 2) in '\' | '/');
   --  Even strictly speaking, smthg like C:/ can only be an absolute path that
   --  comes from some non-Windows native program (git from msys2 or the like).
end Check_Absolute_Path;
