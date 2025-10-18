

package body Repository.Index is 
  
  overriding   
  function To_String(Self : Index_Type ) return String is
    Tmp : String_Object;
  begin
    for C of Self.Column_List loop
      Tmp.Set(Tmp.Camel_Case & " " & C.Name.Camel_Case);
    end loop;
    
    return Self.Columns.Camel_Case & " " & Self.Type_Of'Img & Self.Sequence_Number'Img & Tmp.Camel_Case;
  end To_String;
  
  overriding 
  procedure Reset(Self : in out Index_Type ) is
  begin
    Self.Columns.Reset;
    Self.Type_Of         := Index_Type_Type'First;
    Self.Sequence_Number := 0;
    Self.Column_List.Clear;
  end Reset;

end Repository.Index ;
