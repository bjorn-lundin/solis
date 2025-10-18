
package body Repository.Presentation is

  overriding 
  function To_String(Self : Presentation_Type ) return String is
  begin
    return Self.Size_Of'Img & " " & Self.Long_Desc.Fix_String ;
  end To_String;
  
  overriding 
  procedure Reset(Self : in out Presentation_Type) is
  begin
    Self.Size_Of   := 0;
    Self.Long_Desc.Reset;  
  end Reset;
  
end Repository.Presentation ;


