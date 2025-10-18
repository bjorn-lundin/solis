
with Repository.Code_Item_Tr;

package Repository.Translation is

  type Translation_Type is new Root_Type with record
    Long_Desc    : String_Object;
    Short_Desc   : String_Object;
    Code_List_Tr : Code_Item_Tr.Code_Tr_Pkg.List;
  end record;
  overriding function To_String(Self :Translation_Type) return String;
  overriding procedure Reset(Self : in out Translation_Type);
   
end Repository.Translation;