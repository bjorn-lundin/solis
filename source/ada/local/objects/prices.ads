with Table_Aprices;
with Sql;
with Ada.Containers.Doubly_Linked_Lists;
with Types; use Types;
--with Bot_Types; use Bot_Types;
package Prices is
  type Price_Type is new Table_Aprices.Data_Type with null record;
  -------------------------------------------------------------
  function Empty_Data return Price_Type ;
  -------------------------------------------------------------

  package Lists is new Ada.Containers.Doubly_Linked_Lists(Price_Type);

  -------------------------------------------------------------

  procedure Read_List(Stm  : in     Sql.Statement_Type;
                      List : in out Lists.List;
                      Max  : in     Integer_4 := Integer_4'Last);

  -------------------------------------------------------------

  procedure Read_I1_Marketid(
                           Data  : in     Table_Aprices.Data_Type'class;
                           List  : in out Lists.List;
                           Order : in     Boolean := False;
                           Max   : in     Integer_4 := Integer_4'Last) ;
  -------------------------------------------------------------



end Prices;
