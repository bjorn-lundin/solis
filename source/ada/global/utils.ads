

with Types; use Types;
with Unicode;
with Unicode.CES;

package Utils is

  function Expand_File_Path (File_Path : String) return String ;
--  function F8_Image(F : Fixed_Type; Aft : Natural := 2 ; Exp : Natural := 0) return String ;
  function F8_Image(F : Fixed_Type) return String;
  function Trim (What : String) return String ;
  function Skip_All_Blanks (S : String) return String ;
  function Position (S, Match : String) return Integer;
  function Lower_Case (C : Character) return Character;
  function Lower_Case (S : String) return String;
  function Upper_Case (C : Character) return Character;
  function Upper_Case (S : String) return String;



  function To_Iso_Latin_15(Str : Unicode.CES.Byte_Sequence) return String;
  function To_Utf8(Str : Unicode.CES.Byte_Sequence) return String;

end Utils;
