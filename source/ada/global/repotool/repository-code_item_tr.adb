

package body Repository.Code_Item_Tr is
  ------------------------------------- 
  overriding 
  function To_String(Self : Code_Item_Tr_Type ) return String is
  begin
    return Self.Code'Img & " " & Self.Text.Fix_String;
  end To_String;
  ------------------------------------- 
  overriding 
  procedure Reset(Self : in out Code_Item_Tr_Type ) is
  begin
    Self.Code   := Code_Type'First;
    Self.Text.Reset;
  end Reset;
  
  ------------------------------------- 
  
  
end Repository.Code_Item_Tr ;

