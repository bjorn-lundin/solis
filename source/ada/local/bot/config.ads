with Bot_Types     ; use Bot_Types;
with Types; use Types;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Calendar2;

package Config is

  type Bet_Type is (
                    Horse_Back_1_10_07_1_2_Plc_1_01,
                    Horse_Back_1_28_02_1_2_Plc_1_01,
                    Horse_Back_1_38_00_1_2_Plc_1_01,
                    Horse_Back_1_56_00_1_4_Plc_1_01); --,
--                    Horse_Back_AI_Nfl_1_Hn_100_Lr_0p10_E_12_Plc,
--                    Horse_Back_AI_Nfl_2_Hn_100_Lr_0p50_E_06_Plc,
--                    Horse_Back_AI_Nfl_3_Hn_100_Lr_0p01_E_12_Plc,
--                    Horse_Back_AI_Nfl_4_Hn_200_Lr_0p05_E_08_Plc,
--                    Horse_Back_AI_Nfl_5_Hn_300_Lr_0p05_E_12_Plc,
--                    Horse_Back_AI_nfl_0_hn_300_lr_1p00_E_12_Plc,
--                    Horse_Back_AI_nfl_1_hn_200_lr_1p00_E_12_Plc,
--                    Horse_Back_AI_nfl_2_hn_300_lr_1p00_E_12_Plc,
--                    Horse_Back_AI_nfl_3_hn_300_lr_1p00_E_12_Plc,
--                    Horse_Back_AI_NFL_0_HN_300_LR_1p00_E_12_Win




--43851_Mo_10_Nfl_1_Hn_100_Lr_0.1_e_12
--53726_mo_10_nfl_2_hn_100_lr_0.5_e_6
--51372_mo_10_nfl_3_hn_100_lr_0.01_e_12
--63254_mo_10_nfl_4_hn_200_lr_0.05_e_8
--61355_mo_10_nfl_5_hn_300_lr_0.05_e_12



--                      Horse_Back_1_10_07_1_2_Plc_1_01_Chs,
--                      Horse_Back_1_28_02_1_2_Plc_1_01_Chs,
--                      Horse_Back_1_38_00_1_2_Plc_1_01_Chs,
--                      Horse_Back_1_56_00_1_4_Plc_1_01_Chs
--                      Horse_Back_1_10_07_1_2_Plc_1_01_Hrd,
--                      Horse_Back_1_28_02_1_2_Plc_1_01_Hrd,
--                      Horse_Back_1_38_00_1_2_Plc_1_01_Hrd,
--                      Horse_Back_1_56_00_1_4_Plc_1_01_Hrd
--                   );


  type Allowed_Days_Array is array(Calendar2.Week_Day_Type'Range) of Boolean;

  type Bet_Config_Type is tagged record
    Size                       : Bet_Size_Type    := 30.0;
    Max_Loss_Per_Day           : Fixed_Type       := -200.0;
    Max_Earnings_Per_Day       : Fixed_Type       := 999_999.0;
    Min_Price                  : Unbounded_String := Null_Unbounded_String;
    Enabled                    : Boolean          := False;
    Chase_Allowed              : Boolean          := False;
    Hurdle_Allowed             : Boolean          := False;
    Allowed_Days               : Allowed_Days_Array  := (others => False);
  end record;

  type Bet_Config_Array_Type is array(Bet_Type'Range) of Bet_Config_Type;

  type Config_Type is tagged record
    --Size                       : Bet_Size_Type    := 30.0;
    Max_Exposure               : Fixed_Type          := 0.0;
    Max_Turns_Not_Started_Race : Integer_4           := 4000;
    Enabled                    : Boolean             := False;
    Allowed_Countries          : Unbounded_String    := Null_Unbounded_String;
    Allowed_Days               : Allowed_Days_Array  := (others => False);
    Bet                        : Bet_Config_Array_Type;
    Max_Total_Loss_Per_Day     : Fixed_Type          := -800.0;
  end record;

  function Create(Filename : String) return Config_Type;
  function Country_Is_Ok (Cfg : Config_Type; Country_Code : String) return Boolean;
  function To_String(Cfg : Config_Type) return String ;

  procedure Print_Strategies;

end Config;
