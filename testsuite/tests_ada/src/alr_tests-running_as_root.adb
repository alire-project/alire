with AAA.Strings;
with Alire.OS_Lib;
with Alire.OS_Lib.Subprocess;
with Alire.Platforms.Current;

procedure Alr_Tests.Running_As_Root is

   EUID : constant String :=
     Alire.OS_Lib.Getenv ("EUID");

begin
  if Alire.Platforms.Current.On_Windows then
    pragma Assert
      (not Alire.Platforms.Current.Running_As_Root,
      "Running_As_Root should always be False on Windows");
  elsif EUID /= "" then
    pragma Assert
      (Alire.Platforms.Current.Running_As_Root = (EUID = "0"),
      "Running_As_Root result does not match EUID env var");
  elsif Alire.OS_Lib.Subprocess.Locate_In_Path ("id") /= "" then
    declare
      Output : AAA.Strings.Vector;
      Code   : constant Integer :=
        Alire.OS_Lib.Subprocess.Unchecked_Spawn_And_Capture
          ("id",
           AAA.Strings.Empty_Vector.Append ("-u"),
           Output);
    begin
      if Code = 0 and then not Output.Is_Empty then
        pragma Assert
          (Alire.Platforms.Current.Running_As_Root
           = (AAA.Strings.Trim (Output.First_Element) = "0"),
           "Running_As_Root result does not match `id -u` output");
      end if;
    end;
  end if;
end Alr_Tests.Running_As_Root;
