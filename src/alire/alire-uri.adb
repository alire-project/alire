package body Alire.URI is

   -----------------------------------
   -- Authority_Without_Credentials --
   -----------------------------------

   function Authority_Without_Credentials (This : URL) return String is
      Auth : constant String := Authority (This);
   begin
      if Contains (Auth, "@") then
         return Tail (Auth, '@');
      else
         return Auth;
      end if;
   end Authority_Without_Credentials;

   --------------
   -- Userinfo --
   --------------

   function Userinfo (This : URL) return String is
      Auth : constant String := Authority (This);
   begin
      if Contains (Auth, "@") then
         return Head (Auth, '@');
      else
         return "";
      end if;
   end Userinfo;

   -------------------
   -- Host_From_URL --
   -------------------

   --  Return the host part of a URL.
   --
   --  Doesn't check whether This is actually a URL.
   function Host_From_URL (This : URL) return String is
      Auth : constant String := Authority_Without_Credentials (This);
   begin
      --  Return Auth with any trailing port removed (note that the host may be
      --  an IPv6 address in square brackets)
      if Has_Prefix (Auth, "[") then
         if Contains (Auth, "]:") then
            return Head (Auth, "]:") & "]";
         else
            return Auth;
         end if;
      else
         return Head (Auth, ":");
      end if;
   end Host_From_URL;

   ----------
   -- Host --
   ----------

   function Host (This : URL) return String is
   begin
      if URI_Kind (This) in SCP_Style_Git then
         --  This has the form git@X:Y, so return X
         return Head (Tail (This, "@"), ":");
      elsif URI_Kind (This) in Non_URLs | Local_URIs then
         --  Return empty string for other non-URLs or local URLs
         return "";
      else
         return Host_From_URL (This);
      end if;
   end Host;

   --------------
   -- Fragment --
   --------------

   function Fragment (This : URL) return String
   is (if URI_Kind (This) in Non_URLs | Local_URIs then ""
       else U.Extract (This, U.Fragment));

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
      Scheme       : constant String := L (U.Scheme (This));
      Has_Userinfo : constant Boolean := (Userinfo (This) /= "");
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
         --  We use the fact that URI schemes must not contain a '/', so this
         --  condition is only satisfied if there is no '/' before the ':', as
         --  required by git (https://git-scm.com/docs/git-clone#URLS)
         return SCP_Style_Git;
      elsif Scheme = "git+https" or else Scheme = "git+http" then
         return (if Has_Userinfo
                 then Private_Definitely_Git
                 else Public_Definitely_Git);
      elsif Scheme = "git+file" then
         return Local_Git;
      elsif Has_Prefix (Scheme, "git+") then
         return Private_Definitely_Git;
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
            return (if Has_Userinfo
                    then Private_Definitely_Git
                    else Public_Definitely_Git);
         elsif Is_Known_Git_Host (Host_From_URL (This)) then
            --  These are known git hosts, so recognize them even without a
            --  ".git" suffix
            return (if Has_Userinfo
                    then Private_Probably_Git
                    else Public_Probably_Git);
         else
            return (if Has_Userinfo
                    then Private_HTTP_Other
                    else Public_HTTP_Other);
         end if;
      elsif Scheme = "ssh" then
         if Has_Git_Suffix (This) then
            return Private_Definitely_Git;
         elsif Is_Known_Git_Host (Host_From_URL (This)) then
            --  These are known git hosts (over SSH, so This can't be a raw
            --  file), so recognize them even without a ".git" suffix
            return Private_Definitely_Git;
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

   ------------------------
   -- Strip_VCS_Prefixes --
   ------------------------

   function Strip_VCS_Prefixes (This : String) return String is
      Scheme : constant String := L (U.Scheme (This));
   begin
      if (for some P of VCS_Prefixes => Has_Prefix (Scheme, P.all)) then
         return Tail (This, '+');
      else
         return This;
      end if;
   end Strip_VCS_Prefixes;

   -----------------------
   -- Make_VCS_Explicit --
   -----------------------

   function Make_VCS_Explicit (This : String; Kind : VCS_Kinds) return String
   is
      VCS_Prefix   : constant String := VCS_Prefixes (Kind).all;
      Current_Kind : constant URI_Kinds := URI_Kind (This);
   begin
      case Current_Kind is
         when Bare_Path =>
            --  Convert "/some/path" to "vcs+file:/some/path"
            return VCS_Prefix & To_URL (This);
         when HTTP_Other | SSH_Other | File =>
            --  Not recognizable, so prepend prefix
            return VCS_Prefix & This;
         when VCS_URIs =>
            if VCS_Kind (This) /= Kind then
               raise Program_Error
                  with "URL already looks like a different VCS";
            elsif Current_Kind = SCP_Style_Git then
               --  git@host:/path is already explicit
               return This;
            elsif not Has_Prefix (This, VCS_Prefix) then
               --  Ensure a 'vcs+' prefix is present
               --
               --  We do this even for URLs with a '.git' suffix, as they are
               --  not otherwise recognized by pre-2.1 versions of alr.
               return VCS_Prefix & This;
            else
               --  This already has a 'vcs+' prefix, so do nothing
               return This;
            end if;
         when others =>
            raise Program_Error with "Inappropriate VCS URL";
      end case;
   end Make_VCS_Explicit;

   -------------------
   -- In_Local_URIs --
   -------------------

   function In_Local_URIs (K : URI_Kinds) return Boolean
   is (K in Local_URIs);
end Alire.URI;
