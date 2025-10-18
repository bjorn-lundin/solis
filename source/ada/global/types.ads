
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

package Types is

  type Byte is range 0 .. 255;
  for  Byte'Size use 8;

  type Integer_2 is range -32_768 .. 32_767;
  for  Integer_2'Size use 16;

  type Word is range 0 .. 2 ** 16 - 1;
  for Word'Size use 16;

  type Integer_4 is range -2_147_483_648 .. 2_147_483_647;
  for  Integer_4'Size use 32;

  type Integer_8 is range -9_223_372_036_854_775_808 .. 9_223_372_036_854_775_807;
  for  Integer_8'Size use 64;

  -- type Float_8 is new Long_Float;
  -- package F8 is new Ada.Text_IO.Float_IO (Fixed_Type);
--  type Fixed_Type is delta 0.01 range -10_000_000.0 .. 10_000_000.0;
  type Fixed_Type is delta 0.001 digits 18;

--  function "*" (Left, Right : Fixed_Type) return Fixed_Type;

--  function "/" (Left, Right : Fixed_Type) return Fixed_Type;

 -- package F8 is new Ada.Text_IO.Fixed_IO (Fixed_Type);


  type String_Object is tagged private;

  procedure Set (Self : in out String_Object; What : String);
  procedure Reset (Self : in out String_Object);
  function Fix_String ( Self : String_Object) return String;
  function Fix_String( Self : String_Object; Length : Integer; Justify : Ada.Strings.Alignment := Ada.Strings.Left  ) return String;

  function Trim(Self : String_Object) return String;
  function UBString ( Self : String_Object) return Unbounded_String;
  function Lower_Case ( Self : String_Object) return String;
  function Upper_Case ( Self : String_Object) return String ;
  function Empty_String_Object return String_Object;
  procedure Append (Self : in out String_Object; What : String);
  function Camel_Case (Self : String_Object) return String ;
  procedure Delete_Last_Char (Self : in out String_Object);
  type How_Type is (Fix, Upper, Lower, Camel);
  procedure Put_Line (Self : in out String_Object;
                      How  : in How_Type := Fix;
                      File : in Ada.Text_Io.File_Type := Ada.Text_Io.Standard_Output);
  procedure Put (Self : in out String_Object;
                 How  : in How_Type := Fix;
                 File : in Ada.Text_Io.File_Type := Ada.Text_Io.Standard_Output);

  function "<" ( Left, Right : String_Object) return Boolean;
  overriding function "=" ( Left, Right : String_Object) return Boolean;
  function ">" ( Left, Right : String_Object) return Boolean;
  function Create (What : String) return String_Object;

private
  type String_Object is tagged record
    Value ,
    Camel_Case_Cache,
    Lower_Case_Cache,
    Upper_Case_Cache : Unbounded_String := Null_Unbounded_String;
  end record;
end Types;
