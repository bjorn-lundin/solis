--with Ada.Strings;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers;
--with Text_Io;
--with Gnat.Command_Line; use Gnat.Command_Line;
---with GNAT.Strings;

with Sim;
--with Utils; use Utils;
with Types ; use Types;
--with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
with Price_Histories;
with Bets;
with Calendar2;  use Calendar2;
with Logging; use Logging;
--with Bot_System_Number;


procedure Check_New_Strategy is

  package EV renames Ada.Environment_Variables;


  Global_Bet_List : Bets.Lists.List;
--  Config           : Command_Line_Configuration;

--  IA_Back_At_Back_Price : aliased Integer := 50;
--  IA_Lay_At_Back_Price  : aliased Integer := 100;
--  IA_Max_Lay_Price      : aliased Integer := 200;

--  Lay_Size  : Fixed_Type := 30.0;
--  Back_Size : Fixed_Type := 1500.0;

  --type Bet_Status_Type is (No_Bet_Laid, Bet_Laid);
  --Bet_Status : Bet_Status_Type := No_Bet_Laid;

--    Global_Min_Backprice1     : constant Fixed_Type := 1.31;
--    Global_Max_Backprice1     : constant Fixed_Type := 1.36;
--    Global_Min_Backprice2     : constant Fixed_Type := 2.5;
--    Global_Max_Backprice2     : constant Fixed_Type := 10.0;
--    Global_Lay_At_Backprice   : constant Fixed_Type := 1.25;
--    Global_Lay_Size           : constant Fixed_Type := 110.0;
--    Global_Back_Size          : constant Fixed_Type := 100.0;

  Start : Calendar2.Time_Type := Calendar2.Clock;

  function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  type Best_Runners_Array_Type is array (1..4) of Price_Histories.Price_History_Type ;
  Best_Runners      : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);

  --------------------------------------------

  Enough_Runners : Boolean := False;
  use type Ada.Containers.Count_Type;
  Price : Price_Histories.Price_History_Type;

  Day : Time_Type := (2016,03,19,00,00,00,000);
  End_Date : Time_Type := Clock;
  One_Day : Interval_Type := (1,0,0,0,0);
begin

  Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

  Log ("Connect db");
  Sql.Connect
    (Host     => "localhost",
     Port     => 5432,
     Db_Name  => "dry",
     Login    => "bnl",
     Password => "bnl");
  Log ("Connected to db");

  Day_Loop : loop

    exit Day_Loop when Day >  End_Date;
    Sim.Fill_Data_Maps(Day, Bot_Types.Horse, Rewards => False, Racetimes => False);

    Log("start process date " & Day.To_String);

    declare
      Cnt : Integer := 0;
      Is_Win : Boolean := True;
    --  Bet : Bets.Bet_Type;
    begin
      Log("num markets " & Day.To_String & " " & Sim.Market_With_Data_List.Length'Img);

      Loop_Market : for Market of Sim.Market_With_Data_List loop
        Is_Win := Market.Markettype(1..3) = "WIN";

        if Is_Win then
          Log("Treat market " & Market.To_String );
          Cnt := Cnt + 1;
          -- list of timestamps in this market
          declare
            Timestamp_To_Apriceshistory_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                          Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);
         --   Bet_Placed : Boolean := False;
            First : Boolean := True;
          begin
            Loop_Timestamp : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              --Log("Treat marketid '" & Market.Marketid & "' pricets " & Timestamp.To_String);
              declare
                List : Price_Histories.Lists.List :=
                          Timestamp_To_Apriceshistory_Map(Timestamp.To_String);
              begin
                if First then
                  Enough_Runners := List.Length >= 8;
                  First := False;
                  if not Enough_Runners then
                    exit Loop_Timestamp;  -- too few runners
                  end if;
                end if;

                --Current_Market_Of_Sample := List.First_Element;
                --if Current_Market_Of_Sample.Marketid /= Old_Market_Of_Sample.Marketid then
                --  Log("Treat marketid '" & Current_Market_Of_Sample.Marketid & "' " &
                --      "pricets " & Current_Market_Of_Sample.Pricets.To_String);
                --  Old_Market_Of_Sample := Current_Market_Of_Sample;
                --end if;

                Backprice_Sorter.Sort(List);
                Price.Backprice := 10_000.0;
                Best_Runners := (others => Price);

                declare
                  Idx : Integer := 0;
                begin
                  for Tmp of List loop
                    Idx := Idx +1;
                    exit when Idx > Best_Runners'Last;
                    Best_Runners(Idx) := Tmp;
                  end loop;
                end ;

                --do something here

              end;
              exit Loop_Market when False;
            end loop Loop_Timestamp; --  Timestamp
          end;
        end if; -- Is_Win
      end loop Loop_Market;  -- marketid

    end;
    Log("num bets laid" & Global_Bet_List.Length'Img);

    declare
--        Profit, Sum, Sum_Winners, Sum_Losers  : array (Side_Type'range) of Fixed_Type   := (others => 0.0);
--        Winners, Losers, Unmatched, Strange   : array (Side_Type'range) of Integer_4 := (others => 0);
      T : Sql.Transaction_Type;
    begin
      T.Start;
      for Bet of Global_Bet_List loop
        --Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
        Bet.Insert;

      end loop;
      T.Commit;

--        for i in Side_Type'range loop
--          Sum(i) := Sum_Winners(i) + Sum_Losers(i) ;
--          Log("RESULT day       : " & Day.To_String & " " & i'Img );
--          Log("RESULT Winners   : " & Winners(i)'Img & " " & Integer_4(Sum_Winners(i))'Img );
--          Log("RESULT Losers    : " & Losers(i)'Img  & " " & Integer_4(Sum_Losers(i))'Img);
--          Log("RESULT Unmatched : " & Unmatched(i)'Img  & " " & Unmatched(i)'Img);
--          Log("RESULT Strange   : " & Strange(i)'Img  & " " & Strange(i)'Img);
--          Log("RESULT Sum       : " & Integer_4(Sum(i))'Img );
--        end loop;
--        Log(" Min_Backprice1:" & Global_Min_Backprice1'Img &
--            " Max_Backprice1:" & Global_Max_Backprice1'Img &
--            " Min_Backprice2:" & Global_Min_Backprice2'Img &
--            " Max_Backprice2:" & Global_Max_Backprice2'Img);
--
--        Log(" GT:" &  Integer(Sum(Back) + Sum(Lay))'Img);
    end ;

    Global_Bet_List.Clear;
    Day := Day + One_Day;

  end loop Day_Loop;
  Sql.Close_Session;
  Log("Started : " & Start.To_String);
  Log("Done : " & Calendar2.Clock.To_String);
  Logging.Close;


  exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Check_New_Strategy ;
