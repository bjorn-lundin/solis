
with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
with Types; use Types;
with Bot_Types ; use Bot_Types;
with Ada.Containers.Doubly_Linked_Lists;
with Calendar2;

package Bot_Config is

  Bad_Config : exception;

  type Allowed_Days_Array is array(Calendar2.Week_Day_Type'range) of Boolean;

  type Global_Section_Type is record
    Delay_Between_Turns_Bad_Funding : Fixed_Type := 60.0;
    Delay_Between_Turns_No_Markets  : Fixed_Type := 7.0;
    Delay_Between_Turns             : Fixed_Type := 5.0;
    Network_Failure_Delay           : Fixed_Type := 60.0;
    Logging                         : Boolean := True;
  end record;

  type Bet_Section_Type is record
    Bet_Name         : Unbounded_String       := Null_Unbounded_String;
    Max_Daily_Loss   : Max_Daily_Loss_Type    := 0.0;
    Max_Daily_Profit : Max_Daily_Profit_Type  := 0.0;
    Max_Num_In_The_Air : Integer_4            := 0;
    Delta_Price      : Delta_Price_Type       := 0.0;
    Max_Price        : Bet_Price_Type         := 0.0;
    Min_Price        : Bet_Price_Type         := 0.0;
    Bet_Size         : Bet_Size_Type          := 0.0;
 --   Delta_Size       : Bet_Size_Type          := 20.0;
    Enabled          : Boolean                := False;
    Allow_In_Play    : Boolean                := True;
    Animal           : Animal_Type            := Horse;
    Market_Type      : Bet_Market_Type        := Winner;
    Max_Num_Runners  : Max_Num_Runners_Type   := 25;
    Min_Num_Runners  : Min_Num_Runners_Type   := 8;
    Num_Winners      : Num_Winners_Type       := 1;
    Countries        : Unbounded_String       := Null_Unbounded_String ;
--    Bet_Mode         : Bet_Mode_Type          := Sim;
    Bet_Type         : Bet_Side_Type          := Back;
    Allowed_Days     : Allowed_Days_Array     := (others => False);
    Green_Up_Mode    : Green_Up_Mode_Type     := None;
    Max_Exposure     : Max_Exposure_Type      := 600.0;
    Lay_First_Bet_Persistance   : Bet_Persistence_Type := Lapse;
    Back_First_Bet_Persistance  : Bet_Persistence_Type := Lapse;
    Lay_Second_Bet_Persistance  : Bet_Persistence_Type := Persist;
    Back_Second_Bet_Persistance : Bet_Persistence_Type := Persist;
    Min_Num_Runners_Better_Ranked  : Integer_4   := 3;
    Race_Favorite_Max_Price        : Bet_Price_Type := 6.0;
  end record;
  package Bet_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_Section_Type);

  type System_Section_Type is record
    -- BOT_ROOT is set in bashrc.bash
    Bot_Root   : Unbounded_String := Null_Unbounded_String ; --  =$BOT_ROOT
    Bot_Config : Unbounded_String := Null_Unbounded_String ; --  =$BOT_ROOT/config
    Bot_Target : Unbounded_String := Null_Unbounded_String ; --  =$BOT_ROOT/target
    Bot_Source : Unbounded_String := Null_Unbounded_String ; --  =$BOT_ROOT/source
    Bot_Script : Unbounded_String := Null_Unbounded_String ; --  =$BOT_ROOT/script
    Bot_Home   : Unbounded_String := Null_Unbounded_String ; --  =$BOT_ROOT/script
    Daemonize  : Boolean          := True;
    Bot_Mode   : Bot_Mode_Type        := Real;
  end record;


  type Login_Betfair_Section_Type is record
    Username   : Unbounded_String := Null_Unbounded_String ;
    Password   : Unbounded_String := Null_Unbounded_String ;
    Product_Id : Unbounded_String := Null_Unbounded_String ;
    Vendor_Id  : Unbounded_String := Null_Unbounded_String ;
    App_Key    : Unbounded_String := Null_Unbounded_String ;
  end record;

  type Login_Database_Section_Type is record
    Name     : Unbounded_String := Null_Unbounded_String ;
    Username : Unbounded_String := Null_Unbounded_String ;
    Password : Unbounded_String := Null_Unbounded_String ;
    Host     : Unbounded_String := Null_Unbounded_String ;
  end record;

  type Config_Type is tagged record
     Bot_User          : Unbounded_String      := Null_Unbounded_String;
     Bot_Log_File_Name : Unbounded_String      := Null_Unbounded_String;
     Bot_Ini_File_Name : Unbounded_String      := Null_Unbounded_String ;
     System_Section    : System_Section_Type;
     Global_Section    : Global_Section_Type;
--     Bet_Section       : Bet_Section_Type;
     Bet_Section_List  : Bet_Pack.List;
     Betfair_Section   : Login_Betfair_Section_Type;
     Database_Section  : Login_Database_Section_Type;
  end record;

  procedure Read(Cfg : in out Config_Type);
  procedure Clear(Cfg : in out Config_Type);
  function To_String(Cfg : Config_Type) return String;



  Config : Config_Type;

  procedure Re_Read_Config ;





end Bot_Config;
