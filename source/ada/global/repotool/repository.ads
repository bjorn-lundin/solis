

pragma Warnings(Off);
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists; 
pragma Warnings(On);

with Unchecked_Conversion;
with Unicode.CES;
with Types; use Types;

package Repository is
  Configuration_Error,
  Sequence_Error  : exception;

  function To_Iso_Latin_15(Str : Unicode.CES.Byte_Sequence) return String ;
  procedure Feedback(What : in String);
  procedure Set_Silent;
  
  type Entity_Type_Type is (Db, Ud4);
  procedure Set_Debug_Level(Level : Integer_4);
  
  type Root_Type is abstract tagged record 
    Is_Initialized : Boolean := False;
  end record;  
  function To_String(Self : Root_Type ) return String is abstract;
--  procedure Create(Self : in out Root_Type ) is abstract;
  procedure Reset(Self : in out Root_Type ) is abstract;
  
  type Size_Type is new Integer_4 range -1 .. 8_000;
  type Field_Size_Type is new Integer_4 range -1 .. 10_000;
  type Code_Type is new Integer_4 range -10_000 .. 10_000;
  type Language_Type is (Swe, Eng, Den, Nld, Nor, Fin);

  function To_Language(L : String) return Language_Type;

--  type Type_Of_Type is (String_Format,
--                        Integer_4_Format,
--                        Fixed_Type_Format,
--                        Date_Format,
--                        Time_Format,
--                        Timestamp_Format,
--                        Clob_Format,
--                        Nclob_Format,
--                        Blob_Format);

  type Type_Type is new Integer_4 range -1 .. 26;
  for Type_Type'size use 32; 
  
  type Config_Type_Type is (Tables, 
                            Views,
                            Clreqs, 
                            Terms,
                            Codes,
                            Labels);
                            
  type Listing_Type_Type is (Full, Name);
                            
  --- start Item_Type ----------
  type Item_Type is new Root_Type with record
    Directory : String_Object;
    Pattern   : String_Object;
    Prefix    : String_Object;
  end record;
  overriding function To_String(Self : Item_Type) return String;
  overriding procedure Reset(Self : in out Item_Type) is null; 
  --- stop Item_Type ----------  
  
  type Item_Array_Type is array (Config_Type_Type'range) of Item_Type;
  
  type Output_Type is (Oracle, Sql_Server, Postgresql, Ada_Package);
  type Database_Type_Type is new Output_Type range Oracle .. Postgresql;
  
  ---- start Config type ------
  type Config_Type is new Root_Type with record
    Item : Item_Array_Type;
  end record;
  
  -- reads sattmate.xml and returns paths
  -- also prints the ddls for collections
  
  procedure Create (Self : in out Config_Type); 
  overriding function To_String(Self : Config_Type) return String;
  overriding procedure Reset(Self : in out Config_Type ) is null; --no need to reset config
  
  procedure Print_DDL_Create_Table_For_All(Self : in out Config_Type; Database : Database_Type_Type); 
  procedure Print_DDL_Drop_Table_For_All  (Self : in out Config_Type; Database : Database_Type_Type); 

  procedure Print_DDL_Create_View_For_All (Self : in out Config_Type; Database : Database_Type_Type); 
  procedure Print_DDL_Drop_View_For_All   (Self : in out Config_Type; Database : Database_Type_Type); 

  
  function All_Entities_Defined_Full_Path(Self : in out Config_Type; Entity : Config_Type_Type ) return String_Object'class;
  function All_Entities_Defined_Names(Self : in out Config_Type; Entity : Config_Type_Type) return String_Object'class;
  procedure Create_Table_Makefile(Self : in out Config_Type);
  function List_Coded_Values(Self : in out Config_Type; Listing_Type : in Listing_Type_Type) return String_Object'class;
  procedure Make_Coded_Values(Self : in out Config_Type);
  procedure Make_Extract_Package(Self : in out Config_Type);
  procedure Make_Xml_To_Ud4(Self : in out Config_Type);
  procedure Make_C_Sharp_Class(Self : in out Config_Type; Clreq_Name : String_Object);
  procedure Make_M2_Setup_Separates(Self : in out Config_Type);

  
  
  
  ---- stop Config type ------
  
  function Validate(Xml, Xsd : in String) return Boolean;
  procedure Debug(S : String);
  
  type Data_Type_Type is (A_Char,
                          A_Int,     --32 bit signed
                          A_Big_Int, -- 64-bit signed
                          A_Long,
                          A_Float,
                          A_Double,
                          A_Boolean,
                          A_Char_Code,
                          A_Short_Code,
                          A_Date,
                          A_Time,
                          A_Timestamp,
                          A_Clob,
                          A_Nclob,
                          A_Blob);
  for Data_Type_Type'size use Type_Type'size; 
                     
  for Data_Type_Type use (A_Char       => 1,
                          A_Int        => 2,
                          A_Big_Int    => 3,
                          A_Long       => 4,
                          A_Float      => 5,
                          A_Double     => 6,
                          A_Boolean    => 7,
                          A_Char_Code  => 8,
                          A_Short_Code => 9,
                          A_Date       => 10,
                          A_Time       => 11,
                          A_Timestamp  => 15,
                          A_Clob       => 23,
                          A_Nclob      => 24,
                          A_Blob       => 26);            
  function Data_Type is new Unchecked_Conversion(Type_Type, Data_Type_Type);
  function Data_Type is new Unchecked_Conversion(Data_Type_Type, Type_Type);
                     
  function To_SQL_Type(Data    : Data_Type_Type; Database: Database_Type_Type) return String;
  function Default_Value(Data  : Data_Type_Type; Database: Database_Type_Type) return String;
  function Null_Value(Data     : Data_Type_Type; Size_Of: Size_Type) return String;
  function To_Ada_Type(Data    : Data_Type_Type; Size_Of: Size_Type) return String;
  
  function Null_Value_For_Type_At_Comparison(Data : Data_Type_Type; Size_Of : Size_Type; Var_Name : String) return String;
  
  function Header return String_Object;
  
  type String_Access is access all String;
  Ud4_Type_Mapper : array(Data_Type_Type) of String_Access := (
      A_Char       => new String'("STRING_FORMAT"),
      A_Int        => new String'("INTEGER_4_FORMAT"),
      A_Big_Int    => new String'("INTEGER_8_FORMAT"),
      A_Long       => new String'("INTEGER_4_FORMAT"),
      A_Float      => new String'("Fixed_Type_FORMAT"),
      A_Double     => new String'("Fixed_Type_FORMAT"),
      A_Boolean    => new String'("INTEGER_4_FORMAT"),
      A_Char_Code  => new String'("INTEGER_4_FORMAT"),
      A_Short_Code => new String'("INTEGER_4_FORMAT"),
      A_Date       => new String'("DATE_FORMAT"),
      A_Time       => new String'("TIME_FORMAT"),
      A_Timestamp  => new String'("TIMESTAMP_FORMAT"),
      A_Clob       => new String'("STRING_FORMAT"),
      A_Nclob      => new String'("STRING_FORMAT"),
      A_Blob       => new String'("STRING_FORMAT")  
  );
  CSharp_Type_Mapper : array(Data_Type_Type) of String_Access := (
      A_Char       => new String'("string"),
      A_Int        => new String'("int"),
      A_Big_Int    => new String'("bigint"),
      A_Long       => new String'("int"),
      A_Float      => new String'("double"),
      A_Double     => new String'("double"),
      A_Boolean    => new String'("bool"),
      A_Char_Code  => new String'("int"),
      A_Short_Code => new String'("int"),
      A_Date       => new String'("DateTime"),
      A_Time       => new String'("DateTime"),
      A_Timestamp  => new String'("DateTime"),
      A_Clob       => new String'("clob"),
      A_Nclob      => new String'("nclob"),
      A_Blob       => new String'("blob")
  );

  
private 
  procedure Code_Debug(S : String);
  function Quote(What : String) return String ;
  
end Repository;

  
