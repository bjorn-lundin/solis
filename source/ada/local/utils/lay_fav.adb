with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers;
--with Text_Io;
with Gnat.Command_Line; use Gnat.Command_Line;
--with Gnat.Strings;

with Sim;
--with Utils; use Utils;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
with Price_Histories;
with Bets;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Runners;
with Markets;
--with Bot_System_Number;


procedure Lay_Fav is
    Me : String := "Lay_Fav";

  package Ev renames Ada.Environment_Variables;

  Lay_Size : Bet_Size_Type := 100.0;

  Global_Bet_List  : Bets.Lists.List;
  Cmd_line           : Command_Line_Configuration;

  Ia_Lay_Fav_No : aliased Integer := 0;
  Ia_Filter     : aliased Integer := 0;
  Ia_Max_Layprice     : aliased Integer := 0;
  -- Ia_Delta_Lay_At_Back_Price  : aliased Integer := 100;
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

  Start         : Calendar2.Time_Type := Calendar2.Clock;
  T             : Sql.Transaction_Type;
  function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  type Best_Runners_Array_Type is array (1..16) of Price_Histories.Price_History_Type ;
  Best_Runners      : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);

  --------------------------------------------


  procedure Check_Bet ( R : in Runners.Runner_Type;
                        B : in out Bets.Bet_Type) is
  begin
    if B.Status(1) = 'M' then
      if B.Side(1..4) = "BACK" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := True;
          B.Profit := B.Size * (B.Pricematched - 1.0);
        elsif R.Status(1..5) = "LOSER" then
          B.Betwon := False;
          B.Profit := -B.Size;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
      elsif B.Side(1..3) = "LAY" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := False;
          B.Profit := -B.Size * (B.Pricematched - 1.0);
        elsif R.Status(1..5) = "LOSER" then
          B.Profit := B.Size;
          B.Betwon := True;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
      end if;
      B.Insert;
    end if;
  end Check_Bet;

  -----------------------------------------------------------------

  procedure Run(Br : Best_Runners_Array_Type) is
    Market                 : Markets.Market_Type;
    Eos                    : Boolean := False;
    Runner                 : Runners.Runner_Type;--table_Arunners.Data_Type;
 --   Tic_Lay                : Tics.Tics_Type := 1;
    Bet                    : Bets.bet_Type with Warnings => Off;
    Lay_Bet_Name           : String_Object;
 --   Back_Bet_Name          : String_Object;

 --   Back_Size              : Bet_Size_Type := 0.0;
    Ln                     : Betname_Type := (others => ' ');
  --  Bn                     : Betname_Type := (others => ' ');
  --  Reference              : String(1..30) := (others  => ' ');
  begin
    Log(Me & "Run", "start");

    Market.Marketid := Br(Ia_Lay_Fav_No).Marketid;
    Market.Read(Eos);
    if Eos then
      Log(Me & "Run", "no market found");
      return;
    end if;

    if Ia_Filter = 0 then
      null;

    elsif Ia_Filter = 1 then
      if not Market.Marketname_Ok2 then
        Log(Me & "Run", "bad market found - Name not Ok");
        return;
      end if;

    elsif Ia_Filter = 2 then
      if not Market.Marketname_Ok2 then
        Log(Me & "Run", "bad market (2) found - Name not Ok");
        return;
      end if;

    else
      Log(Me & "Run", "bad filter, 0-2 ok:" & Ia_Filter'Img);
      return;
    end if;

    if  Br(Ia_Lay_Fav_No).Layprice >= Fixed_Type(Ia_Max_Layprice) then
      Log(Me & "Run", "too high price, max is :" & Ia_Max_Layprice'Img);
      return;
    end if;

    Lay_Bet_Name.Set("LAY_FAV_" &
                       Trim(Ia_Lay_Fav_No'Img,Both) & "_" &
                       Trim(Ia_Filter'Img,Both) & "_" &
                       Trim(Ia_Max_Layprice'Img,Both)
                    );

    Runner.Marketid :=  Br(Ia_Lay_Fav_No).Marketid;
    Runner.Selectionid := Br(Ia_Lay_Fav_No).Selectionid;
    Runner.Read(Eos);

    if Eos then
      Log(Me & "Run", "no runner found found  " & Runner.To_String);
      return;
    end if;

    if Runner.Status(1..7) = "REMOVED" then
      Log(Me & "Run", "runner removed " & Runner.To_String);
      return;
    end if;


    Move(Lay_Bet_Name.Fix_String,Ln);
    Sim.Place_Bet(Bet_Name         => Ln,
                  Market_Id        => Market.Marketid,
                  Side             => Lay,
                  Runner_Name      => Runner.Runnernamestripped,
                  Selection_Id     => Br(Ia_Lay_Fav_No).Selectionid,
                  Size             => Lay_Size,
                  Price            => Bet_Price_Type(Br(Ia_Lay_Fav_No).Layprice),
                  Bet_Persistence  => Persist,
                  Bet_Placed       => Br(Ia_Lay_Fav_No).Pricets,
                  Bet              => Bet ) ;

    Move("M",Bet.Status);
    Bet.Pricematched := Br(Ia_Lay_Fav_No).Layprice;

    Check_Bet(Runner, Bet);

--      declare
--        B_Price : Fixed_Type := Tics.Get_Tic_Price(Tic_Lay + Delta_Tics);
--      begin
--        Back_Size := Lay_Size * Bet_Size_Type(Price_Data.Layprice/B_Price);
--      end;
--
--      Move(Back_Bet_Name.Fix_String,Bn);
--
--      Sim.Place_Bet(Bet_Name         => Bn,
--                    Market_Id        => Market.Marketid,
--                    Side             => Back,
--                    Runner_Name      => Runner.Runnernamestripped,
--                    Selection_Id     => Price_Data.Selectionid,
--                    Size             => Back_Size,
--                    Price            => Bet_Price_Type(Tics.Get_Tic_Price(Tic_Lay + Delta_Tics)),
--                    Bet_Persistence  => Persist,
--                    Bet_Placed       => Price_Data.Pricets,
--                    Bet              => Bet.Backbet ) ;
--      Move("U",Bet.Backbet.Status);
--
--      -- see if we meet stop_loss or greenup
--      --there is no delay here since bet is placed in beginning of race
--      for Race_Data of Price_During_Race_List loop
--        if Race_Data.Backprice > Fixed_Type(0.0)
--          -- and then Race_Data.Layprice > Fixed_Type(0.0)
--          and then Race_Data.Backprice < Fixed_Type(1000.0)
--          -- and then Race_Data.Layprice < Fixed_Type(1000.0)
--        then   -- must be valid
--          if Race_Data.Pricets >= Price_Data.Pricets then
--            if Race_Data.Backprice >= Bet.Backbet.Price  -- a match
--              and then Race_Data.Backprice <= Fixed_Type(Global_Overshoot * Bet.Backbet.Price) -- but only if it does not 'overshoot' too much
--            then -- a match
--              Move("M",Bet.Backbet.Status);
--              Bet.Backbet.Pricematched := Race_Data.Backprice;
--              exit;
--              --                elsif Race_Data.Backprice <= Bet.Stop_Loss_Backbet.Price then -- a match
--              --                  Move("M",Bet.Stop_Loss_Backbet.Status);
--              --                  exit;
--            end if;
--          end if;
--        end if;
--      end loop;
--      Check_Bet(Runner, Bet.Backbet);
  end Run;

  Enough_Runners : Boolean := False;
  use type Ada.Containers.Count_Type;
  Price : Price_Histories.Price_History_Type;

  Day      : Time_Type := (2018,10,15,00,00,00,000);
  End_Date : Time_Type := (2020,02,10,00,00,00,000);
  One_Day : Interval_Type := (1,0,0,0,0);
begin

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Ev.Value("BOT_NAME") & ".log");

  Log ("Connect db");
  Sql.Connect
    (Host     => "localhost",
     Port     => 5432,
     Db_Name  => "bnl",
     Login    => "bnl",
     Password => "bnl");
  Log ("Connected to db");

  Define_Switch
    (Cmd_Line,
     Ia_Lay_Fav_No'Access,
     Long_Switch => "--favno=",
     Help        => "lay fav#");

  Define_Switch
    (Cmd_Line,
     Ia_Filter'Access,
     Long_Switch => "--marketnamefilter=",
     Help        => "0,1,2");

  Define_Switch
    (Cmd_Line,
     Ia_Max_Layprice'Access,
     Long_Switch => "--maxlayprice=",
     Help        => "integer");



  Getopt (Cmd_Line);  -- process the command line

  Day_Loop : loop

    exit Day_Loop when Day >  End_Date;
    T.Start;
    Sim.Fill_Data_Maps(Day, Horse,False,False,Race_Prices => True);
    Log("start process date " & Day.To_String);

    declare
      Cnt    : Integer := 0;
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
            First                           : Boolean := True;
          begin
            Loop_Timestamp : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              Log("Treat marketid '" & Market.Marketid & "' pricets " & Timestamp.To_String);
              declare
                List : Price_Histories.Lists.List :=
                         Timestamp_To_Apriceshistory_Map(Timestamp.To_String);
              begin
                Log("list.Length: " & List.Length'Img );
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

                Run(Best_Runners);

              end;
              exit Loop_Market; -- just 1 turn
            end loop Loop_Timestamp; --  Timestamp
          end;
        end if; -- Is_Win
      end loop Loop_Market;  -- marketid
    exception
      when E: others =>
        Stacktrace.Tracebackinfo(E);

    end;
    Log("num bets laid" & Global_Bet_List.Length'Img);

    declare
      --        Profit, Sum, Sum_Winners, Sum_Losers  : array (Side_Type'range) of Fixed_Type   := (others => 0.0);
      --        Winners, Losers, Unmatched, Strange   : array (Side_Type'range) of Integer_4 := (others => 0);
      T : Sql.Transaction_Type with Warnings => Off;
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
    T.Commit;
  end loop Day_Loop;
  Sql.Close_Session;
  Log("Started : " & Start.To_String);
  Log("Done : " & Calendar2.Clock.To_String);
  Logging.Close;


exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Lay_Fav ;
