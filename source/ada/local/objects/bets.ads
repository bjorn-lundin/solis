with Table_Abets;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Calendar2;
with Ada.Containers.Doubly_Linked_Lists;
with Runners;
with Markets;
with Sql;


package Bets is
  function Profit_Today(Bet_Name : Betname_Type) return Fixed_Type ;
  function Exists(Bet_Name : Betname_Type; Market_Id : Marketid_Type) return Boolean;

  --Commission : constant Fixed_Type := 6.5/100.0;
  Commission : constant Fixed_Type := 2.0/100.0;

  type Bet_Type is new Table_Abets.Data_Type with null record;

  function Create(Name : Betname_Type;
                  Side : Bet_Side_Type;
                  Size : Bet_Size_Type;
                  Price : Price_Type;
                  Placed : Calendar2.Time_Type;
                  Runner : Runners.Runner_Type;
                  Market : Markets.Market_Type) return Bet_Type;

  procedure Check_Matched(Self : in out Bet_Type);
  procedure Clear(Self : in out Bet_Type);

  procedure Match_Directly(Self : in out Bet_Type; Value : Boolean );
  function  Match_Directly(Self : in out Bet_Type) return Boolean;
  procedure Nullify_Betwon(Self : in out Bet_Type);
  function  Is_Matched(Self : in out Bet_Type) return Boolean;

  procedure Update_And_Nullify_Betwon(Self : in out Bet_Type; Keep_Timestamp : Boolean := False);
  procedure Insert_And_Nullify_Betwon(Self : in out Bet_Type; Keep_Timestamp : in Boolean := False);
  function Is_Existing_Marketid_Selectionid(Self : in out Bet_Type) return Boolean ;

  procedure Check_Outcome ( Self   : in out Bet_Type ;
                            Runner : in     Runners.Runner_Type := Runners.Empty_Data) ;


  function Empty_Data return Bet_Type;
  package Lists is new Ada.Containers.Doubly_Linked_Lists(Bet_Type);

  function Is_Existing_I7(Betname : in String) return Boolean renames Table_Abets.Is_Existing_I7;
  procedure Read_List(Stm  : in     Sql.Statement_Type;
                      List : in out Lists.List;
                      Max  : in     Integer_4 := Integer_4'Last) ;

  procedure Read_Marketid( Data  : in out Bet_Type'class;
                           List  : in out Lists.List;
                           Order : in     Boolean := False;
                           Max   : in     Integer_4 := Integer_4'Last);

  procedure Check_Bets;
  procedure Check_If_Bet_Accepted;

  procedure Reset_Sum_Laybets ;
  procedure Sum_Laybets(List : Lists.List; Threshold : Fixed_Type) ;




end Bets;
