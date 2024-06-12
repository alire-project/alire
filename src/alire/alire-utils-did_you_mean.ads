package Alire.Utils.Did_You_Mean with Preelaborate is

   function Suggestion (Input           : String;
                        Possible_Values : AAA.Strings.Vector)
                        return String;
   --  Return "Did you mean '<suggestion>'?" if a good enought suggestion
   --  can be found in Possible_Value, otherwise return an empty string.

   type String_Transform is (None, Lower_Case, Upper_Case, Tomify);

   generic
      type Enum is (<>);
      Transform : String_Transform;
   function Enum_Suggestion (Input : String) return String;
   --  Convert all possible Enum values in a string vector and call Suggestion.
   --  The enum string values are converted acording to the Transform value.

end Alire.Utils.Did_You_Mean;
