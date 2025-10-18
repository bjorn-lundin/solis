with Ada.Containers.Doubly_Linked_Lists;
--with Sql;
--with Types; use Types;
with Table_Aunknowns;

package Unknowns is
  type Unknown_Type is new Table_AUnknowns.Data_Type with null record;
  function Empty_Data return Unknown_Type ;
  package Lists is new Ada.Containers.Doubly_Linked_Lists(Unknown_Type);

--    procedure Read_Eventid(  Data  : in out Unknown_Type'Class;
--                             List  : in out List_Pack.List;
--                             Order : in     Boolean := False;
--                             Max   : in     Integer_4 := Integer_4'Last);
--
--
--    procedure Read_List(Stm  : in     Sql.Statement_Type;
--                        List : in out List_Pack.List;
--                        Max  : in     Integer_4 := Integer_4'Last);




end Unknowns;
