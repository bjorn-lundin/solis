with Ada.Containers.Doubly_Linked_Lists;
with Sql;
with Table_Amarkets;
with Types; use Types;

with Bot_Types; use Bot_Types;
package Markets is

  type Market_Subtype_Type is (Plain, Chase, Hurdle);

  type Market_Type is new Table_Amarkets.Data_Type with null record;
  function Empty_Data return Market_Type ;

  -- for a win market find the place market
  procedure Corresponding_Place_Market(Self         : in out Market_Type;
                                       Place_Market :    out Market_Type;
                                       Found        :    out Boolean);
  -- for a place market find the win market
  procedure Corresponding_Win_Market(Self       : in out Market_Type;
                                     Win_Market :    out Market_Type;
                                     Found      :    out Boolean);

  function Marketname_Ok(Self : Market_Type) return Boolean;
  function Marketname_Ok2(Self : Market_Type; Allow_Chase : Boolean := True; Allow_Hurdle : Boolean := True) return Boolean; -- allows Hrd and Chs as well
  function Marketname_Ok3(Self : Market_Type) return Boolean; -- only 5f,6f,f7,1m, 1m1f - Hcp allowed

  function Distance(Self : in out Market_Type) return Integer_4;
  function Distance_Name(Self : in out Market_Type) return Distancename_Type;
  function Market_Subtype(Self : in out Market_Type) return Market_Subtype_Type;



  package Lists is new Ada.Containers.Doubly_Linked_Lists(Market_Type);

  procedure Read_Eventid(  Data  : in out Market_Type'Class;
                           List  : in out Lists.List;
                           Order : in     Boolean := False;
                           Max   : in     Integer_4 := Integer_4'Last);


  procedure Read_List(Stm  : in     Sql.Statement_Type;
                      List : in out Lists.List;
                      Max  : in     Integer_4 := Integer_4'Last);


  procedure Check_Market_Status ;
  procedure Check_Unsettled_Markets(Inserted_Winner : in out Boolean) ;



end Markets;
