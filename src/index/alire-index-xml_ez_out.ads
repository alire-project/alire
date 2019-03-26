package Alire.Index.XML_EZ_Out is

   function Project is new Catalogued_Project
     ("Creation of XML-formatted output from Ada programs");

   Repo : constant URL := "https://github.com/alire-project/xmlezout.git";

   Base_Properties : constant Release_Properties :=
                       Author     ("Marc A. Criley") and
                       Website    ("http://www.mckae.com/xmlEz.html") and
                       License    (Unknown);

   V_1_6 : constant Release :=
               Project.Register
                 (V ("1.6"),
                  Git (Repo, "48bf688f0eb672b597ed5a4f54cd6c535be452f2"),
                  Properties =>
                    Base_Properties and
                    Executable ("tmeztf")
                 );

end Alire.Index.XML_EZ_Out;
