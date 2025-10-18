
with Repository.Presentation;
with Repository.Translation;
with Repository.Code_Item;
with Repository.Code_Item_Tr;

package Repository.Column is

  package P   renames Repository.Presentation;
  package T   renames Repository.Translation;
  package CI  renames Repository.Code_Item;
  package CIT renames Repository.Code_Item_Tr ;
  
  
  type Translation_Array_Type is array (Language_Type'range) of T.Translation_Type;
  
  
  type Column_Type is new Root_Type with record
--    Name             : Unbounded_String;
    Name             : String_Object ;
    Type_Of          : Type_Type;
    Size_Of          : Size_Type;
    Presentation     : P.Presentation_Type;
    
--    Define           : Unbounded_String; 
--    Description      : Unbounded_String;    

    Define           : String_Object; 
    Description      : String_Object;    

    Code_List        : Ci.Code_Pkg.List;
    
    Translations     : Translation_Array_Type;
  end record;
  overriding function To_String(Self : Column_Type ) return String;
  procedure   Create(Self : in out Column_Type ; Config : Repository.Config_Type'class);
  procedure   Create(Self : in out Column_Type ; Name : String; Config : Repository.Config_Type'class);
  procedure   Set_Name(Self : in out Column_Type; Filename : String; Config : Repository.Config_Type'class);
  overriding procedure Reset(Self : in out Column_Type ) ;
  
  
 -- type Columns_Type is array(<>) of Column_Type 
  package Columns_Pkg is new Ada.Containers.Doubly_Linked_Lists(Column_Type); 
  subtype Columns_Type is Columns_Pkg.List;
  
 
--  type Table_State_Type is (Term, CodeList, CodeItem, Translationlist, CodeListTr, CodeItemTr)
 
 
end Repository.Column ;
