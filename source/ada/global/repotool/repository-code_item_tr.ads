
package Repository.Code_Item_Tr is

  type Code_Item_Tr_Type is new Root_Type with record   
    Code   : Code_Type;
    Text   : String_Object;
  end record;
  overriding function To_String(Self : Code_Item_Tr_Type ) return String;
  overriding procedure Reset(Self : in out Code_Item_Tr_Type );
  
  package Code_Tr_Pkg is new Ada.Containers.Doubly_Linked_Lists(Code_Item_Tr_Type);
  
end Repository.Code_Item_Tr ;

