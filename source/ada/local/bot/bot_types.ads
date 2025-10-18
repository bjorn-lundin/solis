
with Types; use Types;
with Unchecked_Conversion;
--with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded ;

with Table_Amarkets;
with Table_Arunners;
with Table_Abets;
with Table_Aevents;
with Table_Ateams;

package Bot_Types is


   subtype Long_Long_Integer is Long_Integer;

--   subtype Bot_Name_Type is Unbounded_String;
--   subtype Bot_Log_File_Name_Type is Unbounded_String;
   type Bet_Market_Type is (Place,
                            Winner,
                            Match_Odds,
                            Correct_Score,
                            Half_Time_Score,
                            Hat_Tricked_Scored,
                            Penalty_Taken,
                            Sending_Off);
--     type Bet_Side_Type is (Back, Lay);
--     type Team_Type is (Home, Away);
--     type Animal_Type is (Horse, Hound, Human);
--     type Max_Daily_Profit_Type is new Fixed_Type;
--     type Max_Daily_Loss_Type is new Fixed_Type;
--     type Max_Lay_Price_Type is new Fixed_Type;
--     type Min_Lay_Price_Type is new Fixed_Type;
--     type Price_Type is new Fixed_Type;
--     subtype Lay_Price_Type is Price_Type;
--     subtype Back_Price_Type is Price_Type;
--     type Delta_Price_Type is new Fixed_Type;
--     type Bet_Size_Type is new Fixed_Type;
--     type Bet_Price_Type is new Fixed_Type;
--     type Min_Num_Runners_Type is new Byte;
--     type Max_Num_Runners_Type is new Byte;
--     type Num_Winners_Type is new Byte;
--     type Favorite_By_Type is new Fixed_Type;
--     type Profit_Type is new Fixed_Type;
--     type Max_Exposure_Type is new Fixed_Type;
--     type Bet_Size_Portion_Type is new Fixed_Type;

   type Bet_Side_Type is (Back, Lay);
   type Team_Type is (Home, Away);
   type Animal_Type is (Horse, Hound, Human);
   type Max_Daily_Profit_Type is new Fixed_Type;
   type Max_Daily_Loss_Type is new Fixed_Type;
   type Max_Lay_Price_Type is new Fixed_Type;
   type Min_Lay_Price_Type is new Fixed_Type;
   type Price_Type is new Fixed_Type;
   --subtype Lay_Price_Type is Price_Type;
   --subtype Back_Price_Type is Price_Type;
   type Lay_Price_Type is new Fixed_Type;
   type Back_Price_Type is new Fixed_Type;
   type Delta_Price_Type is new Fixed_Type;
   type Bet_Size_Type is new Fixed_Type;
   type Bet_Price_Type is new Fixed_Type;
   type Min_Num_Runners_Type is new Byte;
   type Max_Num_Runners_Type is new Byte;
   type Num_Winners_Type is new Byte;
  -- type Favorite_By_Type is new Fixed_Type;
   type Profit_Type is new Fixed_Type;
   type Max_Exposure_Type is new Fixed_Type;
   type Bet_Size_Portion_Type is new Fixed_Type;

   type Bet_Persistence_Type is (Lapse, Persist, Market_On_Close);

   type Bot_Mode_Type is (Real, Simulation);
   for Bot_Mode_Type'Size use Integer_4'Size;
   for Bot_Mode_Type use ( Real => 2, Simulation => 3);
   function Bot_Mode is new Unchecked_Conversion(Bot_Mode_Type, Integer_4);
   function Bot_Mode is new Unchecked_Conversion(Integer_4, Bot_Mode_Type);

   type Bet_Status_Type is (Executable, Execution_Complete, Voided, Cancelled, Lapsed, Settled);
   subtype Cleared_Bet_Status_Type is Bet_Status_Type range  Voided .. Settled ;

   type JSON_Data_Type is (I4,Flt,Ts,Str);

   type Green_Up_Mode_Type is (None, Lay_First_Then_Back, Back_First_Then_Lay);

   subtype Marketid_Type            is String(Table_Amarkets.Empty_Data.Marketid'range);
   subtype Marketname_Type          is String(Table_Amarkets.Empty_Data.Marketname'range);
   subtype Markettype_Type          is String(Table_Amarkets.Empty_Data.Markettype'range);

   subtype Eventid_Type             is String(Table_Aevents.Empty_Data.Eventid'range);
   subtype Eventname_Type           is String(Table_Aevents.Empty_Data.Eventname'range);

   subtype Runnername_Type          is String(Table_Arunners.Empty_Data.Runnername'range);
   subtype Status_Type              is String(Table_Arunners.Empty_Data.Status'range);
   subtype Betname_Type             is String(Table_Abets.Empty_Data.Betname'range);
   subtype Teamname_Type            is String(Table_Ateams.Empty_Data.Teamname'range);
   subtype Bet_Side_String_Type     is String(Table_Abets.Empty_Data.Side'range);
   subtype Bet_Timestamp_Image_Type is String(1..23);
   subtype Venue_Type               is String(1..50);
   subtype Distancename_Type        is String(1..4);

   function "-" (Left : Back_Price_Type ; Right : Delta_Price_Type) return Back_Price_Type;
   function "-" (Left : Bet_Price_Type ; Right : Delta_Price_Type) return Bet_Price_Type;

   function "+" (Left : Bet_Price_Type ; Right : Delta_Price_Type) return Bet_Price_Type;
   function "+" (Left : Back_Price_Type ; Right : Delta_Price_Type) return Back_Price_Type;
 --  function "+" (Left : Back_Price_Type ; Right : Favorite_By_Type) return Back_Price_Type;
  -- function "+" (Left : Fixed_Type ; Right : Favorite_By_Type) return Back_Price_Type ;
 --  function "+" (Left : Fixed_Type ; Right : Favorite_By_Type) return Fixed_Type;

   function "<" (Left : Min_Lay_Price_Type ; Right : Fixed_Type) return Boolean;
   function "<" (Left : Profit_Type ; Right : Max_Daily_Loss_Type) return Boolean ;
   function "<" (Left : Integer ; Right : Min_Num_Runners_Type) return Boolean ;

   function ">" (Left : Integer ; Right : Max_Num_Runners_Type) return Boolean ;
   function ">" (Left : Fixed_Type ; Right : Max_Exposure_Type) return Boolean ;

   function "<=" (Left : Back_Price_Type ; Right : Fixed_Type) return Boolean;
--   function "<=" (Left : Fixed_Type ; Right : Back_Price_Type) return Boolean ;
   function "<=" (Left : Min_Lay_Price_Type ; Right : Fixed_Type) return Boolean;
--   function "<=" (Left : Fixed_Type ; Right : Max_Lay_Price_Type) return Boolean ;
   function "<=" (Left : Fixed_Type ; Right : Price_Type) return Boolean ;
   function "<=" (Left : Price_Type ; Right : Fixed_Type) return Boolean ;

   function ">=" (Left : Profit_Type ; Right : Max_Daily_Profit_Type) return Boolean ;
   function ">=" (Left : Profit_Type ; Right : Max_Daily_Loss_Type) return Boolean ;


   function "*" (Left : Bet_Size_Type ; Right : Lay_Price_Type) return Fixed_Type;
   function "*" (Left : Bet_Size_Type ; Right : Price_Type) return Fixed_Type;
   function "*" (Left : Bet_Size_Type ; Right : Bet_Price_Type) return Bet_Size_Type;
   function "*" (Left : Bet_Size_Type ; Right : Fixed_Type) return Bet_Size_Type;
 --  function "*" (Left : Bet_Size_Type ; Right : Lay_Price_Type) return Fixed_Type;
   function "*" (Left : Bet_Size_Type ; Right : Bet_Size_Portion_Type) return Bet_Size_Type;
   function "*" (Left,Right : Bet_Size_Type) return Bet_Size_Type;
   function "*" (Left,Right : Bet_Price_Type) return Bet_Price_Type;

   function "=" (Left : Integer_4 ; Right : Num_Winners_Type) return Boolean ;

   function "/" (Left : Bet_Size_Type ; Right : Bet_Price_Type) return Bet_Size_Type;
   function "/" (Left : Bet_Size_Type ; Right : Fixed_Type) return Bet_Size_Type;
   function "/" (Left : Fixed_Type ; Right : Back_Price_Type) return Bet_Size_Type ;
   function "/" (Left : Bet_Size_Type ; Right : Bet_Size_Portion_Type) return Bet_Size_Type;
   function "/" (Left,Right : Bet_Size_Type) return Bet_Size_Type;
   function "/" (Left,Right : Bet_Price_Type) return Bet_Price_Type;

end Bot_Types;
