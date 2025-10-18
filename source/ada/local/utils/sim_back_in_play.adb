with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers;
with Gnat.Command_Line; use Gnat.Command_Line;
with GNAT.Strings;

with Sim;
with Utils; use Utils;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Price_Histories;
with Bets;
with Calendar2;  use Calendar2;
with Logging; use Logging;
--with Bot_System_Number;
with Markets;
with Runners;
--with Prices;
with Price_Histories;

procedure Sim_Back_In_Play is

  package EV renames Ada.Environment_Variables;
 -- Betname_Already_Exists : exception;
  Commission : constant Fixed_Type := 0.0 / 100.0;
  Cmd_Line   : Command_Line_Configuration;

  SA_Max_Leader_Back_Price      : aliased Gnat.Strings.String_Access;
  SA_Min_2nd_Back_Price         : aliased Gnat.Strings.String_Access;
  SA_Animal                     : aliased Gnat.Strings.String_Access;
  SA_Log                        : aliased Gnat.Strings.String_Access;

  Global_Logging                 : Boolean := True;
  Global_Max_Leader_Back_Price   : Fixed_Type := 0.0;
  Global_Min_2nd_Back_Price      : Fixed_Type := 0.0;
  Global_Lay_Size                : Bet_Size_Type := 30.0;
--  Global_Back_Size               : Bet_Size_Type := 30.0;
  Global_Animal                  : Animal_Type := Horse; --default
  Global_Bet_List                : Bets.Lists.List;
  --num_bets=${num_bets} \
  --first_bet=${first_bet} \
  --place_num=${place_num} \
  --max_back_price=${max_back} \
  --max_lay_price_delta=${max_lay_delta} > ./${name}.log 2>&1 &
--  Betname_Exists             : Sql.Statement_Type;

  Start             : Calendar2.Time_Type := Calendar2.Clock;
  Global_Bet_Suffix : String_Object;

  --    ----------------------------------------------------------------------------------------
  function "<" (Left, Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting ("<");
  type Best_Runners_Array_Type is array (1 .. 10) of Price_Histories.Price_History_Type ;
  Best_Runners : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);

  -----------------------------------------------------------------------------------------
    --check matched
  procedure Check_Matched is
  begin
    Log ("Check_Matched", "start matching, #bets" & Global_Bet_List.Length'Img );

    Bet_Loop : for Bet of Global_Bet_List loop
      begin
        declare
          Timestamps_For_Market_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                        Sim.Marketid_Timestamp_To_Prices_History_Map (Bet.Marketid);
        begin
          TS_Loop  : for Ts of Sim.Marketid_Pricets_Map (Bet.Marketid) loop
            -- Log ("TS_Loop", "Treat marketid '" & Bet.Marketid & "' pricets " & Ts.To_String );
            declare
              Price_List       : Price_Histories.Lists.List :=
                                   Timestamps_For_Market_Map (TS.To_String);
            begin
              -- Log ("TS_Loop", "found timestamps for marketid '" & Bet.Marketid & "'");
              if Bet.Betplaced + (0, 0, 0, 1, 0) <= Ts and then
                Ts <= Bet.Betplaced + (0, 0, 0, 2, 0) then
                Price_Loop : for Price of Price_List loop
                  if Price.Selectionid = Bet.Selectionid then
                    Log ("Price_Loop", "within time_placed2 +1 and time_placed2 +2 " &
                           "timestamp=" & Ts.To_String );
                    Log ("Price_Loop", "Price: " & Price.To_String);

                    if Bet.Side = "BACK" then
                      if Price.Backprice >= Bet.Price then
                        Bet.Pricematched := Price.Backprice;
                        Move ("MATCHED", Bet.Status);
                        Log ("Price_Loop", "Matched backbet " & Bet.To_String );
                      else
                        Log ("Price_Loop", "UNMatched backbet " & Bet.To_String );
                      end if;
                    elsif Bet.Side = "LAY " then
                      if Fixed_Type(1.01) <= Price.Layprice and then
                         Price.Layprice <= Bet.Price then
                        Bet.Pricematched := Price.Layprice;
                        Move ("MATCHED", Bet.Status);
                        Log ("Price_Loop", "Matched laybet " & Bet.To_String );
                      else
                        Log ("Price_Loop", "UNMatched laybet " & Bet.To_String );
                      end if;
                    end if;
                  end if;
                end loop Price_Loop;
                exit TS_Loop; --only one chance to match a second after ...
              end if;
            end;
          end loop TS_Loop;
        end;

        if Bet.Status (1) = 'M' then
          --check won
          declare
            Winner_List : Runners.Lists.List;
            List_Is_OK  : Boolean := True;
          begin

            Log ("Bet_Loop", "check winner for bet " & Bet.To_String);
            Winner_List.Clear;
            List_Is_OK := True;
            begin
              Winner_List := Sim.Winners_Map (Bet.Marketid);
            exception
              when Constraint_Error =>
                Winner_List.Clear;
                List_Is_OK := False;
            end;
            if List_Is_Ok then
              Winner_Loop : for Winner of Winner_List loop
                if Bet.Side = "BACK" then
                  Bet.Betwon := False;            --assume bet lost
                  Bet.Profit := -Bet.Sizematched;
                  if Winner.Selectionid = Bet.Selectionid then
                    -- is winner
                    Bet.Betwon := True;
                    Bet.Profit := (1.0 - Commission) * Bet.Sizematched * (Bet.Pricematched - 1.0);
                    exit Winner_Loop;
                  end if;
                else
                  Bet.Betwon := True;
                  Bet.Profit := Bet.Sizematched; --assume bet won
                  if Winner.Selectionid = Bet.Selectionid then
                    -- is loser
                    Bet.Betwon := False;
                    Bet.Profit := -(1.0 - Commission) * Bet.Sizematched * (Bet.Pricematched - 1.0);
                    exit Winner_Loop;
                  end if;
                end if;
              end loop Winner_Loop;
              Log ("Bet_Loop", "bet is checked " & Bet.To_String);

            end if; -- List_Is_Ok
          end;
        end if; --bet.Status(1)='M'
        declare
          T : Sql.Transaction_Type;
        begin
          T.Start;
          Bet.Insert;
          T.Commit;
        exception
          when Sql.Duplicate_Index =>
            Log ("Bet_Loop", "Duplicate_index " & Bet.To_String);
            T.Rollback;
        end;
      exception
        when Constraint_Error =>
          Log ("Bet_Loop", "market '" & Bet.Marketid & "' not in map!");
      end;

    end loop Bet_Loop;
  end Check_Matched;

  -----------------------------------------------------------------------------------------
  procedure Treat (BR                     : in     Best_Runners_Array_Type;
                   Win_Market             : in     Markets.Market_Type;
                   Place_Market           : in     Markets.Market_Type;
                   Max_Leader_Back_Price  : in     Fixed_Type;
                   Min_2nd_Back_Price     : in     Fixed_Type;
                   Bet_Suffix             : in     String_Object;
                   Placed_Bet             : in out Boolean;
                   Bet_List               : in out Bets.Lists.List) is
    pragma Unreferenced (Place_Market);
    Betname      : Betname_Type ;
    Runner       : Runners.Runner_Type;
    Bet          : Bets.Bet_Type;
  begin
    Placed_Bet := False;

    if Fixed_Type (1.0) < BR (1).Backprice and then
      BR (1).Backprice < Max_Leader_Back_Price and then
      Min_2nd_Back_Price <= BR (2).Backprice  and then
      BR (2).Backprice  < 10_000.0 and then
      BR (1).Layprice   >  Fixed_Type (1.0) then

      Log ("Treat", "placing bets");
      Log ("Treat", "BR(1) " & BR (1).To_String);
      Log ("Treat", "BR(2) " & BR (2).To_String);
      Log ("Treat", "BR(3) " & BR (3).To_String);

      --        Move ("BACK_PLC" & Bet_Suffix.Fix_String, Betname);
      --        Runner.Selectionid := BR (1).Selectionid;
      --        Bet := Bets.Create (Name   => Betname,
      --                            Side   => Back,
      --                            Size   => Global_Back_Size,
      --                            Price  => Price_Type (1.01), --Price_Type (BR (1).Backprice),
      --                            Placed => BR (1).Pricets,
      --                            Runner => Runner,
      --                            Market => Place_Market);
      --        Bet.Powerdays := 1;
      --        Bet_List.Append (Bet);
      --
      --        Move ("BACK_WIN" & Bet_Suffix.Fix_String, Betname);
      --        Runner.Selectionid := BR (1).Selectionid;
      --        Bet := Bets.Create (Name   => Betname,
      --                            Side   => Back,
      --                            Size   => Global_Back_Size,
      --                            Price  => Price_Type (1.01), --Price_Type (BR (1).Backprice),
      --                            Placed => BR (1).Pricets,
      --                            Runner => Runner,
      --                            Market => Win_Market);
      --        Bet.Powerdays := 1;
      --        Bet_List.Append (Bet);
      declare
        I : Integer := 2;
      begin
        if Fixed_Type (1.0) < BR (I).Layprice and then
          BR (I).Layprice  <  Fixed_Type (1000.0) then
          Bet := Bets.Empty_Data;
          Runner.Selectionid := BR (I).Selectionid;
          Move ("LAY_WIN" & Bet_Suffix.Fix_String & "_2_NO_LIMIT", Betname);
          Bet := Bets.Create (Name   => Betname,
                              Side   => Lay,
                              Size   => Global_Lay_Size,
                              Price  => Price_Type (1000.0),
                              --Price  => Price_Type (BR (i).Layprice + Fixed_Type (5.0)),
                              Placed => BR (I).Pricets,
                              Runner => Runner,
                              Market => Win_Market);
          Bet.Powerdays := Integer_4 (I);
          Bet_List.Append (Bet);
        end if;
      end ;

      --        declare
      --          S_Place : String_Object;
      --        begin
      --          for I in 2 .. 3 loop
      --            if Fixed_Type (1.0) < BR (i).Layprice and then
      --              BR (i).Layprice  <  Fixed_Type (1000.0) then
      --              Bet := Bets.Empty_Data;
      --              S_Place.Set(I'Img);
      --              Runner.Selectionid := BR (i).Selectionid;
      --              Move ("LAY_WIN" & Bet_Suffix.Fix_String & "_" & S_Place.Trim & "_NO_LIMIT", Betname);
      --              Bet := Bets.Create (Name   => Betname,
      --                                  Side   => Lay,
      --                                  Size   => Global_Lay_Size,
      --                                  Price  => Price_Type (1000.0),
      --                                  --Price  => Price_Type (BR (i).Layprice + Fixed_Type (5.0)),
      --                                  Placed => BR (i).Pricets,
      --                                  Runner => Runner,
      --                                  Market => Win_Market);
      --              Bet.Powerdays := Integer_4(I);
      --              Bet_List.Append (Bet);
      --            end if;
      --          end loop;
      --        end ;
      --        declare
      --        -- S_Place : String_Object;
      --        begin
      --          for I in 2 .. 10 loop
      --            if Fixed_Type (1.0) < BR (i).Layprice and then
      --              BR (i).Layprice  <  Fixed_Type (1000.0) then
      --              Bet := Bets.Empty_Data;
      --              --S_Place.Set(I'Img);
      --              Runner.Selectionid := BR (i).Selectionid;
      --              Move ("LAY_WIN" & Bet_Suffix.Fix_String & "_MANY_40_LIMIT", Betname);
      --              Bet := Bets.Create (Name   => Betname,
      --                                  Side   => Lay,
      --                                  Size   => Global_Lay_Size,
      --                                  Price  => Price_Type (40.0),
      --                                  --Price  => Price_Type (BR (i).Layprice + Fixed_Type (5.0)),
      --                                  Placed => BR (i).Pricets,
      --                                  Runner => Runner,
      --                                  Market => Win_Market);
      --              Bet.Powerdays := Integer_4(I);
      --              Bet_List.Append (Bet);
      --            end if;
      --          end loop;
      --        end ;
      --        declare
      --        -- S_Place : String_Object;
      --        begin
      --          for I in 2 .. 10 loop
      --            if Fixed_Type (1.0) < BR (i).Layprice and then
      --              BR (i).Layprice  <  Fixed_Type (1000.0) then
      --              Bet := Bets.Empty_Data;
      --              --S_Place.Set(I'Img);
      --              Runner.Selectionid := BR (i).Selectionid;
      --              Move ("LAY_WIN" & Bet_Suffix.Fix_String & "_MANY_30_LIMIT", Betname);
      --              Bet := Bets.Create (Name   => Betname,
      --                                  Side   => Lay,
      --                                  Size   => Global_Lay_Size,
      --                                  Price  => Price_Type (30.0),
      --                                  --Price  => Price_Type (BR (i).Layprice + Fixed_Type (5.0)),
      --                                  Placed => BR (i).Pricets,
      --                                  Runner => Runner,
      --                                  Market => Win_Market);
      --              Bet.Powerdays := Integer_4(I);
      --              Bet_List.Append (Bet);
      --            end if;
      --          end loop;
      --        end ;
      Placed_Bet := True;
    end if;

  end Treat;
  ---------------------------------------------------------

  use type Ada.Containers.Count_Type;
  Price : Price_Histories.Price_History_Type;

  Day      : Time_Type     := (2016, 3, 31, 00, 00, 00, 000);
  End_Date : Time_Type     := (2017, 3, 31, 00, 00, 00, 000);
  One_Day  : Interval_Type := (1, 0, 0, 0, 0);
begin

  if not EV.Exists ("BOT_NAME") then
    EV.Set (Name => "BOT_NAME", Value => "sim_back_in_play");
  end if;

  -- Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

  Define_Switch
    (Cmd_Line,
     SA_Max_Leader_Back_Price'Access,
     Long_Switch => "--max_leader_back_price=",
     Help        => "max back price for leader");

  Define_Switch
    (Cmd_Line,
     SA_Min_2nd_Back_Price'Access,
     Long_Switch => "--min_2nd_back_price=",
     Help        => "min price for the second runner");

  Define_Switch
    (Cmd_Line,
     SA_Animal'Access,
     Long_Switch => "--animal=",
     Help        => "horse|hound|human");

  Define_Switch
    (Cmd_Line,
     SA_Log'Access,
     Long_Switch => "--log=",
     Help        => "true|false");

  Getopt (Cmd_Line);  -- process the command line

  Global_Logging := Boolean'Value (SA_Log.all);
  Logging.Set_Quiet(not Global_Logging);
  Global_Max_Leader_Back_Price := Fixed_Type'Value (SA_Max_Leader_Back_Price.all);
  Global_Min_2nd_Back_Price    := Fixed_Type'Value (SA_Min_2nd_Back_Price.all);

  if SA_Animal.all = "horse" then
    Global_Animal := Horse;
  elsif SA_Animal.all = "hound" then
    Global_Animal := Hound;
  elsif SA_Animal.all = "human" then
    Global_Animal := Human;
  else
    Log ("bad animal: '" & SA_Animal.all & "'");
    return;
  end if;

  Log ("Connect db");

  case Global_Animal is
    when Horse =>
      Sql.Connect
        (Host     => "localhost",
         Port     => 5432,
         Db_Name  => "bnl",
         Login    => "bnl",
         Password => "bnl");
      Log ("Connected to bnl");
    when Hound =>
      Sql.Connect
        (Host     => "localhost",
         Port     => 5432,
         Db_Name  => "ghd",
         Login    => "bnl",
         Password => "bnl");
      Log ("Connected to ghd");
    when Human =>
      null;
  end case;

  if Global_Min_2nd_Back_Price < 10.0 then
    Global_Bet_Suffix.Append ("_" & F8_Image (Global_Max_Leader_Back_Price) &
                                "_0" & F8_Image (Global_Min_2nd_Back_Price) );
  else
    Global_Bet_Suffix.Append ("_" & F8_Image (Global_Max_Leader_Back_Price) &
                                "_" & F8_Image (Global_Min_2nd_Back_Price) );
  end if;
  Day_Loop : loop
    exit Day_Loop when Day > End_Date;
    Sim.Fill_Data_Maps (Day, Animal => Global_Animal);
    Log ("---+++---+++ start process date " & Day.To_String);

    declare
      Cnt            : Integer := 0;
      Is_Win         : Boolean := True;
      Did_Bet        : Boolean := False;
      Bet_List       : Bets.Lists.List;
      Place_Marketid : Marketid_Type := (others => ' ');
      Place_Market   : Markets.Market_Type;
      Found_Place    : Boolean := False;
    begin
      Log ("Day_Loop","num markets " & Day.To_String & " " & Sim.Market_With_Data_List.Length'Img);

      Loop_Market : for Market of Sim.Market_With_Data_List loop

        Log ("Market_Loop","--++--++--++ start market '" & Market.Marketid & "'");

        Is_Win := Market.Markettype (1 .. 3) = "WIN";

        if Is_Win then
          Is_Win := Market.Numactiverunners >= 8;
          Log ("Market_Loop","Market " & Market.To_String & " had too few runners -> skipping");
        end if;

        Did_Bet     := False;
        Bet_List.Clear;
        Place_Market := Markets.Empty_Data;
        Found_Place  := False;
        if Is_Win then
          begin
            Place_Marketid := Sim.Win_Place_Map (Market.Marketid);
            Log ("Market_Loop","Place market is '" & Place_Marketid & "'");
            Pm_Loop : for Pm of Sim.Market_With_Data_List loop
              if Pm.Marketid = Place_Marketid then
                Place_Market := Pm;
                Found_Place := True;
                exit Pm_Loop;
              end if;
            end loop Pm_Loop;
            Log ("Market_Loop","Place market is found: " & Found_Place'img & "' " & Place_Market.To_String);
          exception
            when others =>
              Is_Win := False;
              Log ("Market_Loop","Market " & Market.To_String & " had no place market -> skipping");
          end;
        end if;

        if Is_Win and Found_Place then
          Log ("Market_Loop","Treat market " & Market.To_String & " num timestamps" &
                 Sim.Marketid_Pricets_Map (Market.Marketid).Length'Img);
          Cnt := Cnt + 1;
          -- list of timestamps in this market
          declare
            Timestamp_To_Apriceshistory_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                Sim.Marketid_Timestamp_To_Prices_History_Map (Market.Marketid);
          begin
            Loop_Timestamp : for Timestamp of Sim.Marketid_Pricets_Map (Market.Marketid) loop
              --Log ("Loop_Timestamp","Treat marketid '" & Market.Marketid & "' pricets " & Timestamp.To_String);
              declare
                List : Price_Histories.Lists.List :=
                         Timestamp_To_Apriceshistory_Map (Timestamp.To_String);
              begin
                Price.Selectionid := 0;
                Price.Backprice := 10_000.0;
                Price.Layprice  := 0.0;
                Best_Runners := (others => Price);

                Backprice_Sorter.Sort (List);
                declare
                  Idx : Integer := 0;
                begin
                  for Tmp of List loop
                    Idx := Idx + 1;
                    exit when Idx > Best_Runners'Last;
                    Best_Runners (Idx) := Tmp;
                  end loop;
                end ;
                if not Did_Bet then
                  Treat (BR                    => Best_Runners,
                         Win_Market            => Market,
                         Place_Market          => Place_Market,
                         Max_Leader_Back_Price => Global_Max_Leader_Back_Price,
                         Min_2nd_Back_Price    => Global_Min_2nd_Back_Price,
                         Bet_Suffix            => Global_Bet_Suffix,
                         Placed_Bet            => Did_Bet,
                         Bet_List              => Bet_List);
                end if;
                if Did_Bet then
                  for Bet of Bet_List loop
                    Global_Bet_List.Append (Bet);
                    Log("Loop_Timestamp", "placed bet" & Bet.To_String);
                  end loop;
                  Bet_List.Clear;
                  exit Loop_Timestamp;
                end if;
              end;
            end loop Loop_Timestamp;
          exception
            when Constraint_Error =>
              Log ("Market_Loop", "Timestamp not in map: Constraint_Error caught "  );
          end;
        end if; -- Is_Win
      end loop Loop_Market;  -- marketid
    end;
    Check_Matched;
    Global_Bet_List.Clear;

    Day := Day + One_Day;
   -- exit Day_Loop when Day.Year = 2016 and Day.Month = 5 and Day.Day = 12; --one day only?
  end loop Day_Loop;


  Sql.Close_Session;
  Log ("Main","Started : " & Start.To_String);
  Log ("Main","Done : " & Calendar2.Clock.To_String);
  Logging.Close;

exception
--    when Betname_Already_Exists  =>
--      Sql.Close_Session;
--      Log ("Betname exists - skip this betname");
--      Log ("Started : " & Start.To_String);
--      Log ("Done : " & Calendar2.Clock.To_String);
--      Logging.Close;
  when E : others =>
    Stacktrace.Tracebackinfo (E);
end Sim_Back_In_Play ;

