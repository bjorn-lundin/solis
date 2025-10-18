

with Repository.Column;


package Repository.Index is

  package C renames Repository.Column;

  type Index_Type_Type is (Primary,
                           Unique,
                           Index,
                           Functional);

  type Index_Type is new Root_Type with record
    Columns         : String_Object;
    Type_Of         : Index_Type_Type;
    Sequence_Number : Integer_4 := 0;
    Column_List     : C.Columns_Type;
  end record;

  overriding function To_String(Self : Index_Type ) return String;
  overriding procedure Reset(Self : in out Index_Type ) ;





end Repository.Index ;
