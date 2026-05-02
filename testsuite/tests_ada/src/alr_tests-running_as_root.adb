with Alire.OS_Lib;
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
  end if;
end Alr_Tests.Running_As_Root;
