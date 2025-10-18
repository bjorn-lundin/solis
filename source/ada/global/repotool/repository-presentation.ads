
package Repository.Presentation is

  type Presentation_Type is new Root_Type with record
    Size_Of  : Size_Type := 0;
    Long_Desc : String_Object;
  end record;
  
  overriding function To_String(Self : Presentation_Type ) return String;
  overriding procedure Reset(Self : in out Presentation_Type);
end Repository.Presentation ;
