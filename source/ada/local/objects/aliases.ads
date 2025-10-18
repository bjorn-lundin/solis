with Ada.Containers.Doubly_Linked_Lists;
--with Sql;
--with Types; use Types;
with Table_Aaliases;

package Aliases is
  type Alias_Type is new Table_Aaliases.Data_Type with null record;
  function Empty_Data return Alias_Type ;
  package Lists is new Ada.Containers.Doubly_Linked_Lists(Alias_Type);

--    procedure Read_Eventid(  Data  : in out Alias_Type'Class;
--                             List  : in out List_Pack.List;
--                             Order : in     Boolean := False;
--                             Max   : in     Integer_4 := Integer_4'Last);
--
--
--    procedure Read_List(Stm  : in     Sql.Statement_Type;
--                        List : in out List_Pack.List;
--                        Max  : in     Integer_4 := Integer_4'Last);




end Aliases;
