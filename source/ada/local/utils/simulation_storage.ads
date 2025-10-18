
with Ada;
with Ada.Strings;
with Ada.Strings.Hash;

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Calendar2;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Table_Apricesfinish;
with Table_Arunners;

with Repository_Types;

package Simulation_Storage is
--  pragma Warnings(Off);
  package Sample_Map_Pack is new Ada.Containers.Ordered_Maps
        (Key_Type     => Calendar2.Time_Type,
         Element_Type => Table_Apricesfinish.Apricesfinish_List_Pack2.List,
         "<"          => Calendar2."<",
         "="          => Table_Apricesfinish.Apricesfinish_List_Pack2."=");
 
  package Marketid_Map_Pack is new Ada.Containers.Hashed_Maps
        (Market_Id_Type,
         Sample_Map_Pack.Map,
         Ada.Strings.Hash,
         "=",
         Sample_Map_Pack."=");
         
  package Winner_Map_Pack is new Ada.Containers.Hashed_Maps
        (Market_Id_Type,
         Table_Arunners.Arunners_List_Pack2.List,
         Ada.Strings.Hash,
         "=",
         Table_Arunners.Arunners_List_Pack2."=");
         
  package Win_Place_Map_Pack is new Ada.Containers.Hashed_Maps
        (Market_Id_Type,
         Market_Id_Type,
         Ada.Strings.Hash,
         "=",
         "=");
  
  -- fill maps from files. if no files exists, create new and use them      
  procedure Fill_Maps(Marketid_Map  : out Marketid_Map_Pack.Map;
                      Winner_Map    : out Winner_Map_Pack.Map; 
                      Win_Place_Map : out Win_Place_Map_Pack.Map) ;
--  pragma Warnings(On);
                      

  --------------------------------------------------------------------------

                      
  type Place_Of_Next_Type is new Integer range 2 .. 4 ;
  type Place_Of_Runner_Type is new Integer range 1 .. 3 ;
  type Strategy_Type is record
    Betname           : Repository_Types.String_Object;
    Marketid          : Market_Id_Type := (others => ' ');
    Leader_At_Max     : Fixed_Type := 0.0;
    Next_At_Min       : Fixed_Type := 0.0;
    Place_Of_Next     : Place_Of_Next_Type:= Place_Of_Next_Type'First;
    Place_Of_Runner   : Place_Of_Runner_Type:= Place_Of_Runner_Type'First;
    Ts_Of_Fulfill     : Calendar2.Time_Type := Calendar2.Time_Type_First;
    Backprice_Matched : Fixed_Type := 0.0;
    Profit            : Fixed_Type := 0.0;
    Profit_102        : Fixed_Type := 0.0;
    Profit_103        : Fixed_Type := 0.0;
    Profit_104        : Fixed_Type := 0.0;
    Num_Matched       : Integer_4 := 0;
    Num_Wins          : Integer_4 := 0;
    Num_Lost          : Integer_4 := 0;
  end record;
  
  package Strategy_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Strategy_Type, "=");
                      
  procedure Load_Strategies(Strategy_List : out Strategy_List_Pack.List);                 
                       
end Simulation_Storage;