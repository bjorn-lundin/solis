with Table_Apriceshistory;
with Sql;
with Ada.Containers.Doubly_Linked_Lists;
with Types; use Types;
--with Bot_Types; use Bot_Types;
package Price_Histories is
  type Price_History_Type is new Table_Apriceshistory.Data_Type with null record;
  function Empty_Data return Price_History_Type ;

  package Lists is new Ada.Containers.Doubly_Linked_Lists(Price_History_Type);
  procedure Read_List(Stm  : in     Sql.Statement_Type;
                      List : in out Lists.List;
                      Max  : in     Integer_4 := Integer_4'Last) ;
  
  
end Price_Histories;
