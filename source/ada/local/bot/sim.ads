
with Types; use Types;
with Bot_Types; use Bot_Types;
with Markets;
with Prices;
with Bets;
with Price_Histories;
with Runners;
with Events;
with Calendar2;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings;
with Ada.Strings.Hash;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;


package Sim is


  generic
    type Data_Type is private;
    Animal : Animal_Type ;
  package Disk_Serializer is
    function File_Exists(Filename : String) return Boolean ;
    procedure Write_To_Disk (Container : in Data_Type; Filename : in String);
    procedure Read_From_Disk (Container : in out Data_Type; Filename : in String);
  end Disk_Serializer;


  procedure Get_Market_Prices(Market_Id  : in     Marketid_Type;
                              Market     : in out Markets.Market_Type;
                              Animal     : in     Animal_Type;
                              Price_List : in out Prices.Lists.List;
                              In_Play    :    out Boolean);


  procedure Place_Bet (Bet_Name         : in     Betname_Type;
                       Market_Id        : in     Marketid_Type;
                       Side             : in     Bet_Side_Type;
                       Runner_Name      : in     Runnername_Type;
                       Selection_Id     : in     Integer_4;
                       Size             : in     Bet_Size_Type;
                       Price            : in     Bet_Price_Type;
                       Bet_Persistence  : in     Bet_Persistence_Type;
                       Bet_Placed       : in     Calendar2.Time_Type := Calendar2.Time_Type_First;
                       Bet              :    out Bets.Bet_Type);

  type Algorithm_Type is (None, Avg);
  procedure Filter_List(Price_List, Avg_Price_List : in out Prices.Lists.List; Alg : Algorithm_Type := None);


  subtype Num_Runners_Type is Integer range 1..36;
  type Fifo_Type is tagged record
    Selectionid    : Integer_4 := 0;
    One_Runner_Sample_List : Prices.Lists.List;
    Avg_Lay_Price  : Fixed_Type := 0.0;
    Avg_Back_Price : Fixed_Type := 0.0;
    In_Use         : Boolean := False;
    Index          : Num_Runners_Type := Num_Runners_Type'first;
  end record;
  procedure Clear(F : in out Fifo_Type);

  Fifo : array (Num_Runners_Type'range) of Fifo_Type;


  procedure Read_Marketid (Marketid : in     Marketid_Type;
                           Animal   : in     Animal_Type;
                           List     :    out Price_Histories.Lists.List) ;

  procedure Read_Marketid_Selectionid(Marketid    : in     Marketid_Type;
                                      Selectionid : in     Integer_4 ;
                                      Animal      : in     Animal_Type;
                                      List        :    out Price_Histories.Lists.List) ;


  procedure Create_Runner_Data(Price_List : in Prices.Lists.List;
                               Alg        : in Algorithm_Type;
                               Is_Winner  : in Boolean;
                               Is_Place   : in Boolean ) ;

  procedure Create_Bet_Data(Bet : in Bets.Bet_Type ) ;

  function Get_Win_Market(Place_Market_Id : Marketid_Type) return Markets.Market_Type ;

  -- for lay_during_race2 start


  --package Market_With_Data_Pack is new Ada.Containers.Doubly_Linked_Lists(Marketid_Type);
  package Markets_Pack is new Ada.Containers.Doubly_Linked_Lists(Markets.Market_Type, Markets."=");

  procedure Read_All_Markets(Date   : in     Calendar2.Time_Type;
                             Animal : in     Animal_Type;
                             List   :    out Markets_Pack.List) ;

  package Timestamp_Pack is new Ada.Containers.Doubly_Linked_Lists(Calendar2.Time_Type, Calendar2."=");

  package Marketid_Pricets_Maps is new Ada.Containers.Hashed_Maps
        (Marketid_Type,
         Timestamp_Pack.List,
         Ada.Strings.Hash,
         "=",
         Timestamp_Pack."=");

  procedure Fill_Marketid_Pricets_Map (Market_With_Data_List   : in     Markets_Pack.List;
                                       Date                    : in     Calendar2.Time_Type;
                                       Animal                  : in     Animal_Type;
                                       Marketid_Pricets_Map    :    out Marketid_Pricets_Maps.Map);


  package Marketid_Prices_Maps is new Ada.Containers.Hashed_Maps
        (Marketid_Type,
         Prices.Lists.List,
         Ada.Strings.Hash,
         "=",
         Prices.Lists."=");

  procedure Fill_Prices_Map (Market_With_Data_List    : in     Markets_Pack.List;
                             Date                     : in     Calendar2.Time_Type;
                             Animal                   : in     Animal_Type;
                             Prices_Map               :    out Marketid_Prices_Maps.Map );

  package Eventid_Events_Maps is new Ada.Containers.Hashed_Maps
        (Eventid_Type,
         Events.Event_Type,
         Ada.Strings.Hash,
         "=",
         Events."=");

  procedure Fill_Events_Map (Market_With_Data_List    : in     Markets_Pack.List;
                             Date                     : in     Calendar2.Time_Type;
                             Animal                   : in     Animal_Type;
                             Events_Map               :    out Eventid_Events_Maps.Map );


  package Marketid_Winner_Maps is new Ada.Containers.Hashed_Maps
        (Marketid_Type,
         Runners.Lists.List,
         Ada.Strings.Hash,
         "=",
         Runners.Lists."=");

  procedure Fill_Winners_Map (Market_With_Data_List    : in     Markets_Pack.List;
                              Date                     : in     Calendar2.Time_Type;
                              Animal                   : in     Animal_Type;
                              Winners_Map              :    out Marketid_Winner_Maps.Map );


--  procedure Fill_Winners_Map (Market_List : in     Markets.Lists.List;
--                              Animal      : in     Animal_Type;
--                              Winners_Map :    out Marketid_Winner_Maps.Map );


  -- for lay_during_race2 stop
  package Win_Place_Maps is new Ada.Containers.Hashed_Maps (
         Marketid_Type,
         Marketid_Type,
         Ada.Strings.Hash,
         "=",
         "=");
  procedure Fill_Win_Place_Map (Date          : in     Calendar2.Time_Type;
                                Animal        : in     Animal_Type;
                                Win_Place_Map :    out Win_Place_Maps.Map);


  --    package Place_Win_Maps is new Ada.Containers.Hashed_Maps (
--           Marketid_Type,
--           Marketid_Type,
--           Ada.Strings.Hash,
--           "=",
--           "=");
--    procedure Fill_Place_Win_Map (Date          : in     Calendar2.Time_Type;
--                                  Animal        : in     Animal_Type;
--                                  Place_Win_Map :    out Place_Win_Maps.Map);

  -- for timestamp slices start

  --'2015-04-12 16:41:25.500'
  subtype Timestamp_String_Key_Type is String(1..23);

  package Timestamp_To_Prices_History_Maps is new Ada.Containers.Hashed_Maps (
         Timestamp_String_Key_Type,
         Price_Histories.Lists.List,
         Ada.Strings.Hash,
         "=",
         Price_Histories.Lists."=");

  package Marketid_Timestamp_To_Prices_History_Maps is new Ada.Containers.Hashed_Maps
        (Marketid_Type,
         Timestamp_To_Prices_History_Maps.Map,
         Ada.Strings.Hash,
         "=",
         Timestamp_To_Prices_History_Maps."=");


  procedure Fill_Marketid_Runners_Pricets_Map (
                                               Market_With_Data_List                    : in     Markets_Pack.List;
                                               Marketid_Pricets_Map                     : in     Marketid_Pricets_Maps.Map;
                                               Date                                     : in     Calendar2.Time_Type;
                                               Animal                                   : in     Animal_Type;
                                               Marketid_Timestamp_To_Apriceshistory_Map :    out Marketid_Timestamp_To_Prices_History_Maps.Map) ;

  -- for timestamp slices stop



  -- rewards

  package Timestamp_To_Reward_Maps is new Ada.Containers.Hashed_Maps (
         Timestamp_String_Key_Type,
         Fixed_Type,
         Ada.Strings.Hash,
         "=",
         "=");


  package Selectionid_Maps is new Ada.Containers.Ordered_Maps (
         Integer_4,
         Timestamp_To_Reward_Maps.Map,
         "<",
         Timestamp_To_Reward_Maps."=");


  package Rewards_Maps is new Ada.Containers.Hashed_Maps (
         Marketid_Type,
         Selectionid_Maps.Map,
         Ada.Strings.Hash,
         "=",
         Selectionid_Maps."=");

  procedure Fill_Rewards_Map(Date   : in     Calendar2.Time_Type;
                             Animal : in     Animal_Type;
                             Side   : in     Bot_Types.Bet_Side_Type;
                             Rm     :    out Rewards_Maps.Map);
  -- end rewards

  -- start timestamp to timestamps win/place markets

  package Wints_Placets_Maps is new Ada.Containers.Hashed_Maps (
         Timestamp_String_Key_Type,
         Timestamp_String_Key_Type,
         Ada.Strings.Hash,
         "=",
         "=");

  procedure Fill_Wints_Placets_Map (Date              : in     Calendar2.Time_Type;
                                    Animal            : in     Animal_Type;
                                    Wints_Placets_Map :    out Wints_Placets_Maps.Map);


  function Is_Race_Winner(Runner               : Runners.Runner_Type;
                          Marketid             : Marketid_Type) return Boolean;

  function Is_Race_Winner(Selectionid          : Integer_4;
                          Marketid             : Marketid_Type) return Boolean;

  procedure Fill_Data_Maps (Date   : in Calendar2.Time_Type;
                            Animal : in Animal_Type;
                            Rewards : Boolean := True;
                         --   Racetimes : Boolean := True;
                            Race_Prices : Boolean := True) ;


  procedure Delete_Shared_Mem (Date   : in Calendar2.Time_Type;
                               Animal : in Animal_Type) ;


  function Get_Place_Price(Win_Data : Price_Histories.Price_History_Type) return Price_Histories.Price_History_Type;


  Market_With_Data_List                    : Sim.Markets_Pack.List;
  Marketid_Timestamp_To_Prices_History_Map : Sim.Marketid_Timestamp_To_Prices_History_Maps.Map;
  Marketid_Pricets_Map                     : Sim.Marketid_Pricets_Maps.Map;
  Winners_Map                              : Sim.Marketid_Winner_Maps.Map;
  Prices_Map                               : Sim.Marketid_Prices_Maps.Map;
  Win_Place_Map                            : Sim.Win_Place_Maps.Map;
--  Place_Win_Map                            : Sim.Place_Win_Maps.Map;
  Events_Map                               : Sim.Eventid_Events_Maps.Map;
--  Racetime_Map                             : Sim.Racetime_Maps.Map;
  Rewards_Map                              : Sim.Rewards_Maps.Map;

  Wints_Placets_Map                        : Sim.Wints_Placets_Maps.Map;

end Sim ;
