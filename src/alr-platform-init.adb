with Alire.Platform;
with Alire.Properties.Dependencies;

with Alr.Query;

procedure Alr.Platform.Init (Current : Platforms.Supported'Class) is
   use all type Alire.Properties.Vector;
begin
   Alire.Platform.Set (Current);
   Alr.Platform.Set   (Current);

   Alr.Platform.Set_Propertes (Basic_Properties and
                                 Alire.Properties.Dependencies.New_Property
                                   (Query.Is_Resolvable'Access,
                                    Basic_Properties));

end Alr.Platform.Init;
