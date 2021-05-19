separate (Alire)
function Check_Absolute_Path (Path : Any_Path) return Boolean is
begin
   return (Path'Length >= 3
           and then Path (Path'First) in 'A' .. 'Z' | 'a' .. 'z'
           and then Path (Path'First + 1) = ':'
           and then Path (Path'First + 2) = GNAT.OS_Lib.Directory_Separator);
end Check_Absolute_Path;
