with Ada.Containers.Doubly_Linked_Lists;
--with Sql;
--with Types; use Types;
with Table_Agames;

package Games is
  type Game_Type is new Table_Agames.Data_Type with null record;
  function Empty_Data return Game_Type ;
  package Lists is new Ada.Containers.Doubly_Linked_Lists(Game_Type);

--    procedure Read_Eventid(  Data  : in out Game_Type'Class;
--                             List  : in out List_Pack.List;
--                             Order : in     Boolean := False;
--                             Max   : in     Integer_4 := Integer_4'Last);
--
--
--    procedure Read_List(Stm  : in     Sql.Statement_Type;
--                        List : in out List_Pack.List;
--                        Max  : in     Integer_4 := Integer_4'Last);




end Games;
