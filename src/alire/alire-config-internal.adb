with AAA.Strings;

package body Alire.Config.Internal is

   function Key (K : Keys) return String
   is ("alire." & AAA.Strings.To_Lower_Case (K'Image));

end Alire.Config.Internal;