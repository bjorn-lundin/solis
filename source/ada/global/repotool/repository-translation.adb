

package body Repository.Translation is

  -------------------------------------------------------------------
  overriding
  function To_String(Self : Translation_Type) return String is 
    Tmp : String_Object;
  begin
    if not Self.Code_List_Tr.Is_Empty then
      for Code_Item_Tr_Data of Self.Code_List_Tr loop
        Tmp.Set(Tmp.Fix_String & Code_Item_Tr_Data.To_String & Ascii.Lf);
      end loop;
    end if;
    return  Self.Long_Desc.Fix_String & " " & 
            Self.Short_Desc.Fix_String & " " & Ascii.Lf & Tmp.Fix_String;
  end To_String;
  
  -------------------------------------------------------------------
  overriding
  procedure Reset(Self : in out Translation_Type) is
  begin
    Self.Long_Desc.Reset;
    Self.Short_Desc.Reset;
        
    if not Self.Code_List_Tr.Is_Empty then
      for Code_Item_Tr_Data of Self.Code_List_Tr loop
        Code_Item_Tr_Data.Reset;
      end loop;
      Self.Code_List_Tr.Clear;
    end if;
    
  end Reset; 
  -------------------------------------------------------------------
  
end Repository.Translation;
