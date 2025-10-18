



package Repository.Code_Item is

  type Code_Item_Type is new Root_Type with record   
    Code   : Code_Type;
    Text   : String_Object;
    Define : String_Object;
  end record;
  overriding function To_String(Self : Code_Item_Type ) return String;
  overriding procedure Reset(Self :in out Code_Item_Type );
  
  package Code_Pkg is new Ada.Containers.Doubly_Linked_Lists(Code_Item_Type);
  
end Repository.Code_Item ;


