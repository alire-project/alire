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

   package body Operators is

      ---------
      -- "/" --
      ---------

      function "/" (L, R : String) return String
      is (L & "/" & R);

   end Operators;

   --------------
   -- URI_Kind --
   --------------

   function URI_Kind (This : String) return URI_Kinds is
      Scheme : constant String := L (U.Scheme (This));
   begin
      if Scheme = "external" then
         return External;
      elsif Scheme = "system" then
         return System;
      elsif Scheme = "" then
         return Bare_Path;
      elsif Scheme = "file" then
         return File;
      elsif Has_Prefix (Scheme, "git@") then
         return Private_Git;
      elsif Scheme = "git+https" or else Scheme = "git+http" then
         return Public_Git;
      elsif Scheme = "git+file" then
         return Local_Git;
      elsif Has_Prefix (Scheme, "git+") then
         return Private_Git;
      elsif Scheme = "hg+https" or else Scheme = "hg+http" then
         return Public_Hg;
      elsif Scheme = "hg+file" then
         return Local_Hg;
      elsif Has_Prefix (Scheme, "hg+") then
         return Private_Hg;
      elsif Scheme = "svn+https" or else Scheme = "svn+http" then
         return Public_SVN;
      elsif Scheme = "svn+file" then
         return Local_SVN;
      elsif Has_Prefix (Scheme, "svn+") then
         return Private_SVN;
      elsif Scheme = "http" or else Scheme = "https" then
         if Has_Git_Suffix (This) then
            return Public_Git;
         elsif Is_Known_Git_Host (Authority (This)) then
            --  These are known git hosts, so recognise them even without a
            --  ".git" suffix
            return Public_Git;
         elsif Is_Known_Git_Host (Authority_Without_Credentials (This)) then
            --  E.g. https://user:token@github.com/user/repo is private
            return Private_Git;
         else
            return Public_Other;
         end if;
      elsif Scheme = "ssh" then
         if Has_Git_Suffix (This) then
            return Private_Git;
         elsif Is_Known_Git_Host (Authority_Without_Credentials (This)) then
            --  These are known git hosts, so recognise them even without a
            --  ".git" suffix
            return Private_Git;
         else
            return SSH_Other;
         end if;
      elsif Scheme'Length = 1 and then Scheme (Scheme'First) in 'a' .. 'z' then
         --  Something starting with "C:" or similar is probably a windows path
         return Bare_Path;
      else
         return Unknown;
      end if;
   end URI_Kind;

   function Strip_VCS_Prefixes (URL : String) return String is
      Scheme : constant String := L (U.Scheme (URL));
   begin
      if Has_Prefix (Scheme, "git+")
        or else Has_Prefix (Scheme, "hg+")
        or else Has_Prefix (Scheme, "svn+")
      then
         return Tail (URL, '+');
      else
         return URL;
      end if;
   end Strip_VCS_Prefixes;
end Alire.URI;
