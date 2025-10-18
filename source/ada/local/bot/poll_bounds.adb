with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Containers.Doubly_Linked_Lists;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
with Bot_Messages;
with Rpc;
with Lock ;
with Posix;
with Ini;
with Logging; use Logging;
with Process_IO;
with Core_Messages;
with Markets;
with Events;
with Prices;
with Table_Abets;
with Bot_Svn_Info;
with Utils; use Utils;
with Bot_System_Number;

procedure Poll_Bounds is
  package EV renames Ada.Environment_Variables;
  --use type Rpc.Result_Type;

  Me              : constant String := "Poll.";
  Timeout         : Duration := 120.0;
  My_Lock         : Lock.Lock_Type;
  Msg             : Process_Io.Message_Type;
  Find_Plc_Market : Sql.Statement_Type;
--  Select_Bet_Size_Portion_Back : Sql.Statement_Type;
  --Select_Bet_Profit : Sql.Statement_Type;

  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Ba_Daemon       : aliased Boolean := False;
  Cmd_Line        : Command_Line_Configuration;
  Now             : Calendar2.Time_Type;
  Ok,
  Is_Time_To_Exit : Boolean := False;

  type Market_Type is (Win, Place);
  type Best_Runners_Array_Type is array (1..4) of Prices.Price_Type ;

  Data : Bot_Messages.Poll_State_Record ;
  This_Process    : Process_Io.Process_Type := Process_IO.This_Process;
  Markets_Fetcher : Process_Io.Process_Type := (("markets_fetcher"),(others => ' '));


  Update_Betwon_To_Null : Sql.Statement_Type;

  -------------------------------------------------------------
  type Bet_Type is (
      Back_1_01_1_05_01_04_1_2_WIN,
      Back_1_01_1_05_05_07_1_2_WIN,
      Back_1_01_1_05_08_10_1_2_WIN,
      Back_1_01_1_05_11_13_1_2_WIN,
      Back_1_01_1_05_14_17_1_2_WIN,
      Back_1_01_1_05_18_20_1_2_WIN,
      Back_1_01_1_05_21_23_1_2_WIN,
      Back_1_01_1_05_24_26_1_2_WIN,
      Back_1_01_1_05_27_30_1_2_WIN,
      Back_1_01_1_05_31_33_1_2_WIN,
      Back_1_01_1_05_34_37_1_2_WIN,
      Back_1_01_1_05_38_40_1_2_WIN,

      Back_1_06_1_10_01_04_1_2_WIN,
      Back_1_06_1_10_05_07_1_2_WIN,
      Back_1_06_1_10_08_10_1_2_WIN,
      Back_1_06_1_10_11_13_1_2_WIN,
      Back_1_06_1_10_14_17_1_2_WIN,
      Back_1_06_1_10_18_20_1_2_WIN,
      Back_1_06_1_10_21_23_1_2_WIN,
      Back_1_06_1_10_24_26_1_2_WIN,
      Back_1_06_1_10_27_30_1_2_WIN,
      Back_1_06_1_10_31_33_1_2_WIN,
      Back_1_06_1_10_34_37_1_2_WIN,
      Back_1_06_1_10_38_40_1_2_WIN,

      Back_1_11_1_15_01_04_1_2_WIN,
      Back_1_11_1_15_05_07_1_2_WIN,
      Back_1_11_1_15_08_10_1_2_WIN,
      Back_1_11_1_15_11_13_1_2_WIN,
      Back_1_11_1_15_14_17_1_2_WIN,
      Back_1_11_1_15_18_20_1_2_WIN,
      Back_1_11_1_15_21_23_1_2_WIN,
      Back_1_11_1_15_24_26_1_2_WIN,
      Back_1_11_1_15_27_30_1_2_WIN,
      Back_1_11_1_15_31_33_1_2_WIN,
      Back_1_11_1_15_34_37_1_2_WIN,
      Back_1_11_1_15_38_40_1_2_WIN,

      Back_1_16_1_20_01_04_1_2_WIN,
      Back_1_16_1_20_05_07_1_2_WIN,
      Back_1_16_1_20_08_10_1_2_WIN,
      Back_1_16_1_20_11_13_1_2_WIN,
      Back_1_16_1_20_14_17_1_2_WIN,
      Back_1_16_1_20_18_20_1_2_WIN,
      Back_1_16_1_20_21_23_1_2_WIN,
      Back_1_16_1_20_24_26_1_2_WIN,
      Back_1_16_1_20_27_30_1_2_WIN,
      Back_1_16_1_20_31_33_1_2_WIN,
      Back_1_16_1_20_34_37_1_2_WIN,
      Back_1_16_1_20_38_40_1_2_WIN,

      Back_1_21_1_25_01_04_1_2_WIN,
      Back_1_21_1_25_05_07_1_2_WIN,
      Back_1_21_1_25_08_10_1_2_WIN,
      Back_1_21_1_25_11_13_1_2_WIN,
      Back_1_21_1_25_14_17_1_2_WIN,
      Back_1_21_1_25_18_20_1_2_WIN,
      Back_1_21_1_25_21_23_1_2_WIN,
      Back_1_21_1_25_24_26_1_2_WIN,
      Back_1_21_1_25_27_30_1_2_WIN,
      Back_1_21_1_25_31_33_1_2_WIN,
      Back_1_21_1_25_34_37_1_2_WIN,
      Back_1_21_1_25_38_40_1_2_WIN,

      Back_1_26_1_30_01_04_1_2_WIN,
      Back_1_26_1_30_05_07_1_2_WIN,
      Back_1_26_1_30_08_10_1_2_WIN,
      Back_1_26_1_30_11_13_1_2_WIN,
      Back_1_26_1_30_14_17_1_2_WIN,
      Back_1_26_1_30_18_20_1_2_WIN,
      Back_1_26_1_30_21_23_1_2_WIN,
      Back_1_26_1_30_24_26_1_2_WIN,
      Back_1_26_1_30_27_30_1_2_WIN,
      Back_1_26_1_30_31_33_1_2_WIN,
      Back_1_26_1_30_34_37_1_2_WIN,
      Back_1_26_1_30_38_40_1_2_WIN,

      Back_1_31_1_35_01_04_1_2_WIN,
      Back_1_31_1_35_05_07_1_2_WIN,
      Back_1_31_1_35_08_10_1_2_WIN,
      Back_1_31_1_35_11_13_1_2_WIN,
      Back_1_31_1_35_14_17_1_2_WIN,
      Back_1_31_1_35_18_20_1_2_WIN,
      Back_1_31_1_35_21_23_1_2_WIN,
      Back_1_31_1_35_24_26_1_2_WIN,
      Back_1_31_1_35_27_30_1_2_WIN,
      Back_1_31_1_35_31_33_1_2_WIN,
      Back_1_31_1_35_34_37_1_2_WIN,
      Back_1_31_1_35_38_40_1_2_WIN,

      Back_1_36_1_40_01_04_1_2_WIN,
      Back_1_36_1_40_05_07_1_2_WIN,
      Back_1_36_1_40_08_10_1_2_WIN,
      Back_1_36_1_40_11_13_1_2_WIN,
      Back_1_36_1_40_14_17_1_2_WIN,
      Back_1_36_1_40_18_20_1_2_WIN,
      Back_1_36_1_40_21_23_1_2_WIN,
      Back_1_36_1_40_24_26_1_2_WIN,
      Back_1_36_1_40_27_30_1_2_WIN,
      Back_1_36_1_40_31_33_1_2_WIN,
      Back_1_36_1_40_34_37_1_2_WIN,
      Back_1_36_1_40_38_40_1_2_WIN,

      Back_1_41_1_45_01_04_1_2_WIN,
      Back_1_41_1_45_05_07_1_2_WIN,
      Back_1_41_1_45_08_10_1_2_WIN,
      Back_1_41_1_45_11_13_1_2_WIN,
      Back_1_41_1_45_14_17_1_2_WIN,
      Back_1_41_1_45_18_20_1_2_WIN,
      Back_1_41_1_45_21_23_1_2_WIN,
      Back_1_41_1_45_24_26_1_2_WIN,
      Back_1_41_1_45_27_30_1_2_WIN,
      Back_1_41_1_45_31_33_1_2_WIN,
      Back_1_41_1_45_34_37_1_2_WIN,
      Back_1_41_1_45_38_40_1_2_WIN,

      Back_1_46_1_50_01_04_1_2_WIN,
      Back_1_46_1_50_05_07_1_2_WIN,
      Back_1_46_1_50_08_10_1_2_WIN,
      Back_1_46_1_50_11_13_1_2_WIN,
      Back_1_46_1_50_14_17_1_2_WIN,
      Back_1_46_1_50_18_20_1_2_WIN,
      Back_1_46_1_50_21_23_1_2_WIN,
      Back_1_46_1_50_24_26_1_2_WIN,
      Back_1_46_1_50_27_30_1_2_WIN,
      Back_1_46_1_50_31_33_1_2_WIN,
      Back_1_46_1_50_34_37_1_2_WIN,
      Back_1_46_1_50_38_40_1_2_WIN,

      Back_1_51_1_55_01_04_1_2_WIN,
      Back_1_51_1_55_05_07_1_2_WIN,
      Back_1_51_1_55_08_10_1_2_WIN,
      Back_1_51_1_55_11_13_1_2_WIN,
      Back_1_51_1_55_14_17_1_2_WIN,
      Back_1_51_1_55_18_20_1_2_WIN,
      Back_1_51_1_55_21_23_1_2_WIN,
      Back_1_51_1_55_24_26_1_2_WIN,
      Back_1_51_1_55_27_30_1_2_WIN,
      Back_1_51_1_55_31_33_1_2_WIN,
      Back_1_51_1_55_34_37_1_2_WIN,
      Back_1_51_1_55_38_40_1_2_WIN,

      Back_1_56_1_60_01_04_1_2_WIN,
      Back_1_56_1_60_05_07_1_2_WIN,
      Back_1_56_1_60_08_10_1_2_WIN,
      Back_1_56_1_60_11_13_1_2_WIN,
      Back_1_56_1_60_14_17_1_2_WIN,
      Back_1_56_1_60_18_20_1_2_WIN,
      Back_1_56_1_60_21_23_1_2_WIN,
      Back_1_56_1_60_24_26_1_2_WIN,
      Back_1_56_1_60_27_30_1_2_WIN,
      Back_1_56_1_60_31_33_1_2_WIN,
      Back_1_56_1_60_34_37_1_2_WIN,
      Back_1_56_1_60_38_40_1_2_WIN,

      Back_1_61_1_65_01_04_1_2_WIN,
      Back_1_61_1_65_05_07_1_2_WIN,
      Back_1_61_1_65_08_10_1_2_WIN,
      Back_1_61_1_65_11_13_1_2_WIN,
      Back_1_61_1_65_14_17_1_2_WIN,
      Back_1_61_1_65_18_20_1_2_WIN,
      Back_1_61_1_65_21_23_1_2_WIN,
      Back_1_61_1_65_24_26_1_2_WIN,
      Back_1_61_1_65_27_30_1_2_WIN,
      Back_1_61_1_65_31_33_1_2_WIN,
      Back_1_61_1_65_34_37_1_2_WIN,
      Back_1_61_1_65_38_40_1_2_WIN,

      Back_1_66_1_70_01_04_1_2_WIN,
      Back_1_66_1_70_05_07_1_2_WIN,
      Back_1_66_1_70_08_10_1_2_WIN,
      Back_1_66_1_70_11_13_1_2_WIN,
      Back_1_66_1_70_14_17_1_2_WIN,
      Back_1_66_1_70_18_20_1_2_WIN,
      Back_1_66_1_70_21_23_1_2_WIN,
      Back_1_66_1_70_24_26_1_2_WIN,
      Back_1_66_1_70_27_30_1_2_WIN,
      Back_1_66_1_70_31_33_1_2_WIN,
      Back_1_66_1_70_34_37_1_2_WIN,
      Back_1_66_1_70_38_40_1_2_WIN,

      Back_1_71_1_75_01_04_1_2_WIN,
      Back_1_71_1_75_05_07_1_2_WIN,
      Back_1_71_1_75_08_10_1_2_WIN,
      Back_1_71_1_75_11_13_1_2_WIN,
      Back_1_71_1_75_14_17_1_2_WIN,
      Back_1_71_1_75_18_20_1_2_WIN,
      Back_1_71_1_75_21_23_1_2_WIN,
      Back_1_71_1_75_24_26_1_2_WIN,
      Back_1_71_1_75_27_30_1_2_WIN,
      Back_1_71_1_75_31_33_1_2_WIN,
      Back_1_71_1_75_34_37_1_2_WIN,
      Back_1_71_1_75_38_40_1_2_WIN,

      Back_1_76_1_80_01_04_1_2_WIN,
      Back_1_76_1_80_05_07_1_2_WIN,
      Back_1_76_1_80_08_10_1_2_WIN,
      Back_1_76_1_80_11_13_1_2_WIN,
      Back_1_76_1_80_14_17_1_2_WIN,
      Back_1_76_1_80_18_20_1_2_WIN,
      Back_1_76_1_80_21_23_1_2_WIN,
      Back_1_76_1_80_24_26_1_2_WIN,
      Back_1_76_1_80_27_30_1_2_WIN,
      Back_1_76_1_80_31_33_1_2_WIN,
      Back_1_76_1_80_34_37_1_2_WIN,
      Back_1_76_1_80_38_40_1_2_WIN,

      Back_1_81_1_85_01_04_1_2_WIN,
      Back_1_81_1_85_05_07_1_2_WIN,
      Back_1_81_1_85_08_10_1_2_WIN,
      Back_1_81_1_85_11_13_1_2_WIN,
      Back_1_81_1_85_14_17_1_2_WIN,
      Back_1_81_1_85_18_20_1_2_WIN,
      Back_1_81_1_85_21_23_1_2_WIN,
      Back_1_81_1_85_24_26_1_2_WIN,
      Back_1_81_1_85_27_30_1_2_WIN,
      Back_1_81_1_85_31_33_1_2_WIN,
      Back_1_81_1_85_34_37_1_2_WIN,
      Back_1_81_1_85_38_40_1_2_WIN,

      Back_1_86_1_90_01_04_1_2_WIN,
      Back_1_86_1_90_05_07_1_2_WIN,
      Back_1_86_1_90_08_10_1_2_WIN,
      Back_1_86_1_90_11_13_1_2_WIN,
      Back_1_86_1_90_14_17_1_2_WIN,
      Back_1_86_1_90_18_20_1_2_WIN,
      Back_1_86_1_90_21_23_1_2_WIN,
      Back_1_86_1_90_24_26_1_2_WIN,
      Back_1_86_1_90_27_30_1_2_WIN,
      Back_1_86_1_90_31_33_1_2_WIN,
      Back_1_86_1_90_34_37_1_2_WIN,
      Back_1_86_1_90_38_40_1_2_WIN,

      Back_1_91_1_95_01_04_1_2_WIN,
      Back_1_91_1_95_05_07_1_2_WIN,
      Back_1_91_1_95_08_10_1_2_WIN,
      Back_1_91_1_95_11_13_1_2_WIN,
      Back_1_91_1_95_14_17_1_2_WIN,
      Back_1_91_1_95_18_20_1_2_WIN,
      Back_1_91_1_95_21_23_1_2_WIN,
      Back_1_91_1_95_24_26_1_2_WIN,
      Back_1_91_1_95_27_30_1_2_WIN,
      Back_1_91_1_95_31_33_1_2_WIN,
      Back_1_91_1_95_34_37_1_2_WIN,
      Back_1_91_1_95_38_40_1_2_WIN,

      Back_1_96_2_00_01_04_1_2_WIN,
      Back_1_96_2_00_05_07_1_2_WIN,
      Back_1_96_2_00_08_10_1_2_WIN,
      Back_1_96_2_00_11_13_1_2_WIN,
      Back_1_96_2_00_14_17_1_2_WIN,
      Back_1_96_2_00_18_20_1_2_WIN,
      Back_1_96_2_00_21_23_1_2_WIN,
      Back_1_96_2_00_24_26_1_2_WIN,
      Back_1_96_2_00_27_30_1_2_WIN,
      Back_1_96_2_00_31_33_1_2_WIN,
      Back_1_96_2_00_34_37_1_2_WIN,
      Back_1_96_2_00_38_40_1_2_WIN

      );

  -------------------------------------------------------------

  type Allowed_Type is record
    Bet_Name          : Betname_Type := (others => ' ');
    Bet_Size          : Bet_Size_Type := 0.0;
 --   Is_Allowed_To_Bet : Boolean       := False;
    Has_Betted        : Boolean       := False;
    Max_Loss_Per_Day  : Bet_Size_Type := 0.0;
 --   Bet_Size_Portion  : Bet_Size_Portion_Type := 0.0;
  end record;

  Bets_Allowed : array (Bet_Type'range) of Allowed_Type;


  --------------------------------------------------------------



  type Bet_List_Record is record
    Bet           : Table_Abets.Data_Type;
    Price_Finish  : Prices.Price_Type;
    Price_Finish2 : Prices.Price_Type;
  end record;

  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_List_Record);


  Global_Bet_List : Bet_List_Pack.List;


  ----------------------------------------------------------

  procedure Set_Bet_Names is
  begin
    for i in Bet_Type'range loop
      case i is
        when others             => Move(I'Img, Bets_Allowed(i).Bet_Name);
      end case;
    end loop;
  end Set_Bet_Names;
  ----------------------------------------------------------------------------



  --------------------------------------------------------------

  -------------------------------------------------------------------------------------------------------------------

  procedure Try_To_Make_Back_Bet(
   -- Bettype         : in     Bet_Type;
    Best_Runners    : in     Best_Runners_Array_Type;
    Win_Marketid    : in     Marketid_Type;
    Plc_Marketid    : in     Marketid_Type;
  --  Min_Price       : in     String ;
    Bet_List        : in out Bet_List_Pack.List
    --Match_Directly :  in     Boolean := False
    ) is

    Bet : Table_Abets.Data_Type;
  begin
    for i in Bet_Type'range loop
      --          1         2         3
      -- 123456789012345678901234567890
      -- Back_1_46_1_50_11_13_1_2_WIN,

      declare
        Min_1, Max_1 : Fixed_Type := 0.0;
        Min_2, Max_2 : Fixed_Type := 0.0;
        Betname      : String  := i'img;
        Backed_Place    : Integer;
        Next_Place      : Integer;

      begin
        if not Bets_Allowed(i).Has_Betted then

          Min_1 := Fixed_Type'Value(Betname( 6) & "." & Betname(8..9));
          Max_1 := Fixed_Type'Value(Betname(11) & "." & Betname(13..14));
          Min_2 := Fixed_Type'Value(Betname(16..17));
          Max_2 := Fixed_Type'Value(Betname(19..20));
          Backed_Place := Integer'Value(Betname(22..22));
          Next_Place := Integer'Value(Betname(24..24));

          if Best_Runners(Backed_Place).Backprice >= Min_1 and then
             Best_Runners(Backed_Place).Backprice <= Max_1 and then
             Best_Runners(Next_Place).Backprice >= Min_2 and then
             Best_Runners(Next_Place).Backprice <= Max_2 and then
             Best_Runners(Backed_Place).Layprice > Fixed_Type(1.01) and then
             Best_Runners(Backed_Place).Layprice < Fixed_Type(1_000.0) then

             Bet := Table_Abets.Empty_Data;

             Bet.Marketid    := Win_Marketid;
             Bet.Selectionid := Best_Runners(Backed_Place).Selectionid;
             Bet.Side        := "BACK";
             Bet.Size        := Fixed_Type(Bets_Allowed(i).Bet_Size);
             Bet.Price       := Best_Runners(Backed_Place).Backprice;
             Bet.Sizematched := Fixed_Type(Bets_Allowed(i).Bet_Size);
             Bet.Pricematched:= Best_Runners(Backed_Place).Backprice;
             Bet.Betplaced   := Best_Runners(Backed_Place).Pricets;
             Bet.Reference(1) := '.';
             Bet.Inststatus(1) := '.';
             Bet.Insterrcode(1) := '.';
             Bet.Runnername(1) := '.';
             Bet.Fullmarketname(1) := '.';
             Bet.Status(1) := '-';
             Move(Betname, Bet.Betname);
             Bet_List.Append(Bet_List_Record'(
                  Bet          => Bet,
                  Price_Finish => Best_Runners(Backed_Place),
                  Price_Finish2 => Best_Runners(Next_Place))
             );


             -- also bet on place
             Bet := Table_Abets.Empty_Data;
             Bet.Marketid    := Plc_Marketid;
             Bet.Selectionid := Best_Runners(Backed_Place).Selectionid;
             Bet.Side        := "BACK";
             Bet.Size        := Fixed_Type(Bets_Allowed(i).Bet_Size);
             Bet.Price       := Best_Runners(Backed_Place).Backprice;
             Bet.Sizematched := Fixed_Type(Bets_Allowed(i).Bet_Size);
             Bet.Pricematched:= Best_Runners(Backed_Place).Backprice;
             Bet.Betplaced   := Best_Runners(Backed_Place).Pricets;
             Bet.Reference(1) := '.';
             Bet.Inststatus(1) := '.';
             Bet.Insterrcode(1) := '.';
             Bet.Runnername(1) := '.';
             Bet.Fullmarketname(1) := '.';
             Bet.Status(1) := '-';
             Betname(26..28) := "PLC";
             Move(Betname, Bet.Betname);
             Bet_List.Append(Bet_List_Record'(
                  Bet          => Bet,
                  Price_Finish => Best_Runners(Backed_Place),
                  Price_Finish2 => Best_Runners(Next_Place))
             );
             Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);
             Bets_Allowed(i).Has_Betted := True;

          end if;

        end if;
      end;
    end loop;
  end Try_To_Make_Back_Bet;
  -------------------------------------------------------------------------------------------------------------------

  procedure Run(Market_Notification : in Bot_Messages.Market_Notification_Record) is
    Market    : Markets.Market_Type;
    Event     : Events.Event_Type;
    Price_List : Prices.Lists.List;
    --------------------------------------------
    function "<" (Left,Right : Prices.Price_Type) return Boolean is
    begin
      return Left.Backprice < Right.Backprice;
    end "<";
    --------------------------------------------
    package Backprice_Sorter is new Prices.Lists.Generic_Sorting("<");

    Price             : Prices.Price_Type;
    Has_Been_In_Play,
    In_Play           : Boolean := False;
    Best_Runners      : Best_Runners_Array_Type := (others => Prices.Empty_Data);

    Worst_Runner      : Prices.Price_Type := Prices.Empty_Data;

    Eos               : Boolean := False;
    Market_Array      : array (Market_Type'range) of Markets.Market_Type;
    Found_Place       : Boolean := True;
    T                 : Sql.Transaction_Type;
    Current_Turn_Not_Started_Race : Integer_4 := 0;
  begin
    Log(Me & "Run", "Treat market: " &  Market_Notification.Market_Id);
    Market.Marketid := Market_Notification.Market_Id;

    Set_Bet_Names;

    --set values from cfg
    for i in Bets_Allowed'range loop
      Bets_Allowed(i).Bet_Size   := 30.0;
      Bets_Allowed(i).Has_Betted := False;
      Bets_Allowed(i).Max_Loss_Per_Day := 10000.0;
    end loop;

    Global_Bet_List.Clear;

    Market.Read(Eos);
    if not Eos then
      if  Market.Markettype(1..3) /= "WIN"  then
        Log(Me & "Run", "not a WIN market: " &  Market_Notification.Market_Id);
        return;
      else
        Event.Eventid := Market.Eventid;
        Events.Read(Event, Eos);
        if not Eos then
          if Event.Eventtypeid /= Integer_4(7) then
            Log(Me & "Run", "not a HORSE market: " &  Market_Notification.Market_Id);
            return;
--          elsif not Cfg.Country_Is_Ok(Event.Countrycode) then
--            Log(Me & "Run", "not an OK country,  market: " &  Market_Notification.Market_Id);
--            return;
          end if;
        else
          Log(Me & "Run", "no event found");
          return;
        end if;
      end if;
    else
      Log(Me & "Run", "no market found");
      return;
    end if;
    Market_Array(Win):= Market;

    T.Start;
      Find_Plc_Market.Prepare(
        "select MP.* from AMARKETS MW, AMARKETS MP " &
        "where MW.EVENTID = MP.EVENTID " &
        "and MW.STARTTS = MP.STARTTS " &
        "and MW.MARKETID = :WINMARKETID " &
        "and MP.MARKETTYPE = 'PLACE' " &
        "and MP.NUMWINNERS = 3 " &
        "and MW.MARKETTYPE = 'WIN' " &
        "and MP.STATUS = 'OPEN'" ) ;

      Find_Plc_Market.Set("WINMARKETID", Market_Array(Win).Marketid);
      Find_Plc_Market.Open_Cursor;
      Find_Plc_Market.Fetch(Eos);
      if not Eos then
        Market_Array(Place) := Markets.Get(Find_Plc_Market);
        if Market_Array(Win).Startts /= Market_Array(Place).Startts then
           Log(Me & "Make_Bet", "Wrong PLACE market found, give up");
           Found_Place := False;
        end if;
      else
        Log(Me & "Make_Bet", "no PLACE market found");
        Found_Place := False;
      end if;
      Find_Plc_Market.Close_Cursor;
    T.Commit;

    -- do the poll
    Poll_Loop : loop

      if Market_Array(Place).Numwinners < Integer_4(3) then
        exit Poll_Loop;
      end if;

      --Table_Aprices.Aprices_List_Pack.Remove_All(Price_List);
      Price_List.Clear;
      Rpc.Get_Market_Prices(Market_Id  => Market_Notification.Market_Id,
                            Market     => Market,
                            Price_List => Price_List,
                            In_Play    => In_Play);

      exit Poll_Loop when Market.Status(1..4) /= "OPEN" and then Has_Been_In_Play;

      if not Has_Been_In_Play then
        -- toggle the first time we see in-play=true
        -- makes us insensible to Betfair toggling bug
        Has_Been_In_Play := In_Play;
      end if;

      if not Has_Been_In_Play then
        if Current_Turn_Not_Started_Race >= 120 then  -- 120*5-> 10 min
           Log(Me & "Make_Bet", "Market took too long time to start, give up");
           exit Poll_Loop;
        else
          Current_Turn_Not_Started_Race := Current_Turn_Not_Started_Race +1;
          delay 5.0; -- no need for heavy polling before start of race
        end if;
      else
        delay 0.05; -- to avoid more than 20 polls/sec
      end if;

      -- ok find the runner with lowest backprice:
      Backprice_Sorter.Sort(Price_List);

      Price.Backprice := 10_000.0;
      Best_Runners := (others => Price);
      Worst_Runner.Layprice := 10_000.0;

      declare
        Idx : Integer := 0;
      begin
        for Tmp of Price_List loop
          if Tmp.Status(1..6) = "ACTIVE" then
            Idx := Idx +1;
            exit when Idx > Best_Runners'Last;
            Best_Runners(Idx) := Tmp;
          end if;
        end loop;
      end ;

      for Tmp of Price_List loop
        if Tmp.Status(1..6) = "ACTIVE" and then
           Tmp.Backprice > Fixed_Type(1.0) and then
           Tmp.Layprice < Fixed_Type(1_000.0) and then
           Tmp.Selectionid /= Best_Runners(1).Selectionid and then
           Tmp.Selectionid /= Best_Runners(2).Selectionid then

          Worst_Runner := Tmp;
        end if;
      end loop;

      for i in Best_Runners'range loop
        Log("Best_Runners(i)" & i'Img & " " & Best_Runners(i).To_String);
      end loop;
      Log("Worst_Runner " & Worst_Runner.To_String);

      if Best_Runners(1).Backprice >= Fixed_Type(1.01) then
        if Found_Place and then Market_Array(Place).Numwinners >= Integer_4(3) then
          Try_To_Make_Back_Bet (
                Best_Runners => Best_Runners,
                Win_Marketid  => Market_Array(Win).Marketid,
                Plc_Marketid  => Market_Array(Place).Marketid,
                Bet_List  => Global_Bet_List);
        else
          exit Poll_Loop;
        end if;
      end if;
    end loop Poll_Loop;

    begin
      T.Start;
      Update_Betwon_To_Null.Prepare("update ABETS set BETWON = null where BETID = :BETID");

      for b of Global_Bet_List loop
        b.Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
        Log("inserting " &  b.Bet.To_String);
        b.Bet.Insert;
        Update_Betwon_To_Null.Set("BETID", b.Bet.Betid);
        Update_Betwon_To_Null.Execute;
      end loop;
      T.Commit;
--    exception
--      when others =>
--        T.Rollback;
--        Log("exception, rolling back");
    end;



  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
------------------------------ main start -------------------------------------

begin

   Define_Switch
    (Cmd_Line,
     Sa_Par_Bot_User'access,
     Long_Switch => "--user=",
     Help        => "user of bot");

   Define_Switch
     (Cmd_Line,
      Ba_Daemon'access,
      Long_Switch => "--daemon",
      Help        => "become daemon at startup");

   Define_Switch
     (Cmd_Line,
      Sa_Par_Inifile'access,
      Long_Switch => "--inifile=",
      Help        => "use alternative inifile");

  Getopt (Cmd_Line);  -- process the command line

  if Ba_Daemon then
    Posix.Daemonize;
  end if;

   --must take lock AFTER becoming a daemon ...
   --The parent pid dies, and would release the lock...
  My_Lock.Take(EV.Value("BOT_NAME"));

  Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

 -- Cfg := Config.Create(Ev.Value("BOT_HOME") & "/" & Sa_Par_Inifile.all);
 -- Log(Cfg.To_String);
  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
        (Host     => Ini.Get_Value("database", "host", ""),
         Port     => Ini.Get_Value("database", "port", 5432),
         Db_Name  => Ini.Get_Value("database", "name", ""),
         Login    => Ini.Get_Value("database", "username", ""),
         Password =>Ini.Get_Value("database", "password", ""));
  Log(Me, "db Connected");

  Log(Me, "Login betfair");
  Rpc.Init(
            Username   => Ini.Get_Value("betfair","username",""),
            Password   => Ini.Get_Value("betfair","password",""),
            Product_Id => Ini.Get_Value("betfair","product_id",""),
            Vendor_Id  => Ini.Get_Value("betfair","vendor_id",""),
            App_Key    => Ini.Get_Value("betfair","appkey","")
          );
  Rpc.Login;
  Log(Me, "Login betfair done");

  --if Cfg.Enabled then
  --  Cfg.Enabled := Ev.Value("BOT_MACHINE_ROLE") = "PROD";
  --end if;

  Main_Loop : loop

    --notfy markets_fetcher that we are free
    Data := (Free => 1, Name => This_Process.Name , Node => This_Process.Node);
    Bot_Messages.Send(Markets_Fetcher, Data);

    begin
      Log(Me, "Start receive");
      Process_Io.Receive(Msg, Timeout);
      Log(Me, "msg : "& Process_Io.Identity(Msg)'Img & " from " & Trim(Process_Io.Sender(Msg).Name));
      if Sql.Transaction_Status /= Sql.None then
        raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
      end if;
      case Process_Io.Identity(Msg) is
        when Core_Messages.Exit_Message                  =>
          exit Main_Loop;
        when Bot_Messages.Market_Notification_Message    =>
       --   if Cfg.Enabled then
            --notfy markets_fetcher that we are busy
            Data := (Free => 0, Name => Process_Io.This_Process.Name , Node => Process_Io.This_Process.Node);
            Bot_Messages.Send(Markets_Fetcher, Data);
            Run(Bot_Messages.Data(Msg));
       --   else
    --        Log(Me, "Poll is not enabled in poll.ini");
     --     end if;
        when others =>
          Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout =>
        Rpc.Keep_Alive(OK);
        if not OK then
          begin
            Rpc.Login;
          exception
            when Rpc.Login_Failed =>
              Log(Me, "login failed, but will try again");
          end;
        end if;
    end;
    Now := Calendar2.Clock;

    --restart every day
    Is_Time_To_Exit := Now.Hour = 01 and then
                     ( Now.Minute = 00 or Now.Minute = 01) ; -- timeout = 2 min

    exit Main_Loop when Is_Time_To_Exit;

  end loop Main_Loop;

  Log(Me, "Close Db");
  Sql.Close_Session;
  Rpc.Logout;
  Logging.Close;
  Posix.Do_Exit(0); -- terminate

exception
  when Lock.Lock_Error =>
    Log(Me, "lock error, exit");
    Logging.Close;
    Posix.Do_Exit(0); -- terminate
  when E: others =>
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
    begin
      Log(Last_Exception_Name);
      Log("Message : " & Last_Exception_Messsage);
      Log(Last_Exception_Info);
      Log("addr2line" & " --functions --basenames --exe=" &
           Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
    end ;

    Log(Me, "Closed log and die");
    Logging.Close;
    Posix.Do_Exit(0); -- terminate
end Poll_Bounds;
