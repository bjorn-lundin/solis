with Ada.Containers.Doubly_Linked_Lists;
--with Sql;
--with Types; use Types;
with Table_Abalances;

package Balances is
  type Balance_Type is new Table_Abalances.Data_Type with null record;
  function Empty_Data return Balance_Type ;
  package List_Pack is new Ada.Containers.Doubly_Linked_Lists(Balance_Type);

--    procedure Read_Eventid(  Data  : in out Balance_Type'Class;
--                             List  : in out List_Pack.List;
--                             Order : in     Boolean := False;
--                             Max   : in     Integer_4 := Integer_4'Last);
--
--
--    procedure Read_List(Stm  : in     Sql.Statement_Type;
--                        List : in out List_Pack.List;
--                        Max  : in     Integer_4 := Integer_4'Last);




end Balances;
