package body Alire.URI is

   -----------------------------------
   -- Authority_Without_Credentials --
   -----------------------------------

   function Authority_Without_Credentials (This : URL) return String is
      Auth : constant String := Authority (This);
   begin
      if (for some Char of Auth => Char = '@') then
         return AAA.Strings.Tail (Auth, '@');
      else
         return Auth;
      end if;
   end Authority_Without_Credentials;

   ------------
   -- Scheme --
   ------------

   function Scheme (This : URL) return Schemes
   is
      Img : constant String := L (U.Scheme (This));
   begin
      return
        (if Img = "" then
              None
         elsif Img = "external" then
            External
         elsif Img = "file" then
            File
         elsif AAA.Strings.Has_Prefix (Img, "git+") then
              Git
         elsif AAA.Strings.Has_Prefix (Img, "git@") then
              Pure_Git
         elsif AAA.Strings.Has_Prefix (Img, "hg+") then
              Hg
         elsif AAA.Strings.Has_Prefix (Img, "svn+") then
              SVN
         elsif Img = "http" then
            HTTP
         elsif Img = "https" then
            HTTP
         elsif Img = "system" then
            System
         elsif Img'Length = 1 and then Img (Img'First) in 'a' .. 'z' then
            None -- A Windows drive letter, so a path without scheme
         else
            Unknown);
   end Scheme;

   package body Operators is

      ---------
      -- "/" --
      ---------

      function "/" (L, R : String) return String
      is (L & "/" & R);

   end Operators;

end Alire.URI;
