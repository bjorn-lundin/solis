with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers;
--with Text_Io;
with Gnat.Command_Line; use Gnat.Command_Line;
with GNAT.Strings;

with Sim;
with Utils; use Utils;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
with Price_Histories;
with Bets;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Markets;
with Runners;
--with Bot_System_Number;


procedure Do_Greenup_1 is

  package Ev renames Ada.Environment_Variables;


  Global_Bet_List : Bets.Lists.List;
  Cmd_Line         : Command_Line_Configuration;

  Sa_Backdiff          :  aliased Gnat.Strings.String_Access;
  Sa_Bet_At_Max        :  aliased Gnat.Strings.String_Access;
  Sa_Startdate         : aliased Gnat.Strings.String_Access;
  Sa_Stopdate          : aliased Gnat.Strings.String_Access;
  Ia_Marketnamefilter  : aliased Integer := 0;
  --  IA_Max_Lay_Price      : aliased Integer := 200;

  --  Lay_Size  : Fixed_Type := 30.0;
  Global_Back_Size  : Bet_Size_Type := 100.0;
  Global_Backdiff   : Fixed_Type := 0.0;
  Global_Bet_At_Max : Fixed_Type := 0.0;

  --type Bet_Status_Type is (No_Bet_Laid, Bet_Laid);
  --Bet_Status : Bet_Status_Type := No_Bet_Laid;

  --    Global_Min_Backprice1     : constant Fixed_Type := 1.31;
  --    Global_Max_Backprice1     : constant Fixed_Type := 1.36;
  --    Global_Min_Backprice2     : constant Fixed_Type := 2.5;
  --    Global_Max_Backprice2     : constant Fixed_Type := 10.0;
  --    Global_Lay_At_Backprice   : constant Fixed_Type := 1.25;
  --    Global_Lay_Size           : constant Fixed_Type := 110.0;
  --    Global_Back_Size          : constant Fixed_Type := 100.0;

  Start           : Calendar2.Time_Type := Calendar2.Clock;

  function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  type Ba_Body2_Record is record
    Profit : Fixed_Type := 0.0;
    Marketid : Marketid_Type := (others => ' ');
    Betplaced : Calendar2.Time_Type := Calendar2.Time_Type_First;
    Pricematched : Fixed_Type := 0.0;
  end record;


  type Ba_Body_Record is record
    Ph : Price_Histories.Price_History_Type := Price_Histories.Empty_Data;
    Win : Ba_Body2_Record;
    Plc : Ba_Body2_Record;
  end record;

  type Best_Runners_Array_Type is array (1..16) of Ba_Body_Record; -- Price_Histories.Price_History_Type ;
  Best_Runners      : Best_Runners_Array_Type ;


  ---------------------------------------------
  function Format_Fixed_Number(Num : Fixed_Type) return String is
  begin
    if Num < 10.0 then
        return "00" & F8_Image(Num) ;

    elsif Num < 100.0 then
        return "0" & F8_Image(Num) ;

    elsif Num < 1000.0 then
        return "" & F8_Image(Num) ;
    end if;

    return "WTF-" & F8_Image(Num);
  end Format_Fixed_Number;

  ---------------------------------------------

  procedure To_Array(List : in out Price_Histories.Lists.List ;
                     Bra  : in out Best_Runners_Array_Type ) is
    Price             : Price_Histories.Price_History_Type := Price_Histories.Empty_Data;
  begin
    Price.Backprice := 0.0;
    for I in Bra'Range loop
      Bra(I).Ph := Price;
    end loop;
    declare
      Idx : Integer := 0;
    begin
      for Tmp of List loop
        if Tmp.Status(1..6) = "ACTIVE" then
          Idx := Idx +1;
          exit when Idx > Bra'Last;
          Bra(Idx).Ph := Tmp;
        end if;
      end loop;
    end ;
  end To_Array;

  --------------------------------------------
  function Price_Is_Ok(Price : Price_Histories.Price_History_Type; Side : Bet_Side_Type) return Boolean is
  begin
    case Side is
      when Back =>
        return
          Price.Backprice  > Fixed_Type(1.0)    and then -- must be valid
          Price.Backprice  < Fixed_Type(1000.1); -- must be valid

      when Lay  =>
        return
          Price.Layprice   > Fixed_Type(1.0)    and then -- must be valid
          Price.Layprice   < Fixed_Type(1000.1) ;     -- must be valid
    end case;
  end Price_Is_Ok;


  ---------------------------------------------


  procedure Try_Place_Lay_Bet(M          : in Markets.Market_Type ;
                              Br         : in Best_Runners_Array_Type ;
                              Bl         : in out Bets.Lists.List;
                              Bet_Placed : out Boolean) is
    B_Win       : Bets.Bet_Type;
--    B_Plc       : Bets.Bet_Type;
    Betname : Betname_Type := (others => ' ');
    R       : Runners.Runner_Type;
  begin
    Bet_Placed := False;

    if Fixed_Type(20) <= Br(1).Ph.Layprice
      and then Br(1).Ph.Layprice <= Fixed_Type(40)
      and then Price_Is_Ok(Br(1).Ph,Lay)
    then
      Move ("GRE_40.0_20.0_WIN", Betname);
      R.Selectionid := Br(1).Ph.Selectionid;
      B_Win := Bets.Create(Name   => Betname,
        Side   => Lay,
        Size   => 40.0,
        Price  => Price_Type(Br(1).Ph.Layprice),
        Placed => Br(1).Ph.Pricets,
        Runner => R,
        Market => M);
    end if;

    if Br(1).Ph.Totalmatched = Fixed_Type(0.0) then --no match
      null;
    else
      Bet_Placed := True;
      B_Win.Profit := Br(1).Ph.Totalmatched;
      B_Win.Betwon := B_Win.Profit > Fixed_Type(0.0);
      Bl.Append(B_Win);
    end if;

  end Try_Place_Lay_Bet;
  pragma Unreferenced(Try_Place_Lay_Bet);
  ------------------------------------------------------

  procedure Try_Place_Back_Bet(Win_M      : in     Markets.Market_Type ;
                               Plc_M      : in     Markets.Market_Type ;
                               Backdiff   : in     Fixed_Type;
                               Bet_At_Max : in     Fixed_Type;
                               Br         : in     Best_Runners_Array_Type ;
                               Bl         : in out Bets.Lists.List;
                               Plc_Ok     : in     Boolean;
                               Bet_Placed : in out Boolean) is
    B_Win       : Bets.Bet_Type;
    B_Plc       : Bets.Bet_Type;
    Betname     : Betname_Type := (others => ' ');
    R           : Runners.Runner_Type;
    Plc_Bet_Already_Placed : Boolean := False;
    Win_Bet_Already_Placed : Boolean := False;
  begin

    for B of Bl loop
      if B.Fullmarketname(1) = 'T' then  -- To Be Placed
        if B.Marketid = Plc_M.Marketid  then
          Plc_Bet_Already_Placed := True;
        end if;
      else
        if B.Marketid = Win_M.Marketid  then
          Win_Bet_Already_Placed := True;
        end if;
      end if;

    end loop;

    Bet_Placed := Plc_Bet_Already_Placed and Win_Bet_Already_Placed;

    --winmarket first

    if not Win_Bet_Already_Placed
      and then Br(1).Ph.Backprice < Bet_At_Max
      and then Br(2).Ph.Backprice - Br(1).Ph.Backprice > Backdiff
    then
      Move ("DIFF_R1_R2_" & Format_Fixed_Number(Backdiff) & "_" & Format_Fixed_Number(Bet_At_Max) & "_WIN_" & Ia_Marketnamefilter'Img(2), Betname);
      R.Selectionid := Br(1).Ph.Selectionid;
      B_Win := Bets.Create(Name   => Betname,
                           Side   => Back,
                           Size   => Global_Back_Size,
                           Price  => Price_Type(Br(1).Ph.Backprice),
                           Placed => Br(1).Win.Betplaced,
                           Runner => R,
                           Market => Win_M);

      if Br(1).Win.Profit = Fixed_Type(0.0) then --no match
        Move ("LAPSED", B_Win.Status);
      else
        Move ("MATCHED", B_Win.Status);
        B_Win.Profit := Br(1).Win.Profit;
        B_Win.Betwon := B_Win.Profit > Fixed_Type(0.0);
        B_Win.Pricematched := Br(1).Win.Pricematched;
        if not B_Win.Betwon then
          B_Win.Profit := - Fixed_Type(Global_Back_Size);
        end if;
      end if;
      Bl.Append(B_Win);

      Log("Try_Place_Back_Bet win " & B_Win.To_String);
    end if;

    --plcmarket then
    if Plc_Ok then
      if not Plc_Bet_Already_Placed
        and then Br(1).Ph.Backprice < Bet_At_Max
        and then Br(4).Ph.Backprice - Br(1).Ph.Backprice > Backdiff
      then
        Move ("DIFF_R1_R4_" & Format_Fixed_Number(Backdiff) & "_" & Format_Fixed_Number(Bet_At_Max) & "_PLC_" & Ia_Marketnamefilter'Img(2), Betname);
        R.Selectionid := Br(1).Ph.Selectionid;
        B_Plc := Bets.Create(Name   => Betname,
                             Side   => Back,
                             Size   => Global_Back_Size,
                             Price  => Price_Type(Br(1).Ph.Backprice),
                             Placed => Br(1).Plc.Betplaced,
                             Runner => R,
                             Market => Plc_M);

        if Br(1).Plc.Profit = Fixed_Type(0.0) then --no match
          Move ("LAPSED", B_Plc.Status);
        else
          Move ("MATCHED", B_Plc.Status);
          B_Plc.Profit := Br(1).Plc.Profit;
          B_Plc.Betwon := B_Plc.Profit > Fixed_Type(0.0);
          B_Plc.Pricematched := Br(1).Plc.Pricematched;
          if not B_Plc.Betwon then
            B_Plc.Profit := - Fixed_Type(Global_Back_Size);
          end if;

        end if;
        Bl.Append(B_Plc);

        Log("Try_Place_Back_Bet plc " & B_Plc.To_String);
      end if;
    end if;

  end Try_Place_Back_Bet;


  ---------------------------------------------

  Enough_Runners : Boolean := False;
  use type Ada.Containers.Count_Type;

  Day      : Time_Type := (2016,03,19,00,00,00,000);
  End_Date : Time_Type := (2019,12,31,23,59,59,999);
  One_Day  : Interval_Type := (1,0,0,0,0);
begin

  Define_Switch
    (Cmd_Line,
     Sa_Backdiff'Access,
     Long_Switch => "--diff=",
     Help        => "min(R(4).Backprice - R(1).Backprice)");

  Define_Switch
    (Cmd_Line,
     Sa_Bet_At_Max'Access,
     Long_Switch => "--bet_at_max=",
     Help        => "max(R(1).Backprice) to allow bet to be placed");

  Define_Switch
    (Cmd_Line,
     Ia_Marketnamefilter'Access,
     Long_Switch => "--marketname_filter=",
     Help        => "1=filter1, 2=filter2 else none");

  Define_Switch
    (Cmd_Line,
     Sa_Startdate'Access,
     Long_Switch => "--startdate=",
     Help        => "2018-04-06");

  Define_Switch
    (Cmd_Line,
     Sa_Stopdate'Access,
     Long_Switch => "--stopdate=",
     Help        => "2019-06-12");

  Getopt (Cmd_Line);  -- process the command line


  if Sa_Startdate.all /= "" then
    Day := Calendar2.To_Time_Type(Sa_Startdate.all,"");
  end if;

  if Sa_Stopdate.all /= "" then
    End_Date := Calendar2.To_Time_Type(Sa_Stopdate.all,"");
  end if;

  if Sa_Backdiff.all /= "" then
    Global_Backdiff := Fixed_Type'Value(Sa_Backdiff.all);
  else
    raise Constraint_Error with "no backdiff provided";
  end if;

  if Sa_Bet_At_Max.all /= "" then
    Global_Bet_At_Max := Fixed_Type'Value(Sa_Bet_At_Max.all);
  else
    raise Constraint_Error with "no bet_at_max provided";
  end if;

  if not Ev.Exists ("BOT_NAME") then
    Ev.Set ("BOT_NAME", "do_greenup_1");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Ev.Value("BOT_NAME") & ".log");

  Log ("Connect db");
  Sql.Connect
    (Host     => "localhost",
     Port     => 5432,
     Db_Name  => "bnl",
     Login    => "bnl",
     Password => "bnl",
     SSL_Mode => "prefer"
    );

  Log ("Connected to db");
  Log("start_date '" & Sa_Startdate.all & "'");
  Log("stop_date  '" & Sa_Stopdate.all & "'");
  Log("Backdiff  '" & Sa_Backdiff.all & "'");
  Log("Bet_At_Max  '" & Sa_Bet_At_Max.all & "'");
  Log("Marketnamefilter  '" & Ia_Marketnamefilter'Img & "'");

  Day_Loop : loop
    exit Day_Loop when Day >  End_Date;
    Sim.Fill_Data_Maps(Day,Horse,Racetimes => False);
    Log("start process date " & Day.To_String);

    declare
      Cnt            : Integer := 0;
      Is_Ok         : Boolean := True;
      --Laybet_Placed  : Boolean := False;
      Backbet_Placed : Boolean := False;
    begin
      Log("num markets " & Day.To_String & " " & Sim.Market_With_Data_List.Length'Img);

      Loop_Market    : for Market of Sim.Market_With_Data_List loop
 --       Laybet_Placed  := False;
        Backbet_Placed := False;

        Is_Ok := Market.Markettype(1..3) = "WIN";

        case Ia_Marketnamefilter is
          when 1 => Is_Ok := Is_Ok and Market.Marketname_Ok;
          when 2 => Is_Ok := Is_Ok and Market.Marketname_Ok2;
          when others => null;
        end case;

        if Is_Ok then
          Log("Treat market " & Market.To_String);
          Cnt := Cnt + 1;
          -- list of timestamps in this market
          declare
            Timestamp_To_Apriceshistory_Map  : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                 Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);
            First                                : Boolean := True;
            Plc_Market                           : Markets.Market_Type;
            Plc_Found                            : Boolean := False;
            Dummy_Selid                          : Integer_4 := 0;
            Dummy_Profit                         : Fixed_Type := 0.0;
            Plc_Market_Ok                        : Boolean := False;

          begin
            Log("Treat marketid " & Market.To_String);

            begin
              Plc_Market.Marketid := Sim.Win_Place_Map(Market.Marketid);
              Plc_Found := True;
              Plc_Market.Startts := Market.Startts;
              Move("To Be Placed", Plc_Market.Marketname);
            exception
              when Constraint_Error => Plc_Found := False;
            end ;

            Log("main","Plc_Market Found " & Plc_Found'Img & " " & Plc_Market.To_String  );

            Loop_Timestamp : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              declare
                List : Price_Histories.Lists.List := Timestamp_To_Apriceshistory_Map(Timestamp.To_String);
              begin
                if First then
                  Enough_Runners := List.Length >= 8;
                  First := False;
                  if not Enough_Runners then
                    exit Loop_Timestamp;  -- too few runners
                  end if;
                end if;

                Backprice_Sorter.Sort(List);
                To_Array(List,Best_Runners);

                --check all runners in map and see that profit is set on best_runners. also put comm back on
                for R of Best_Runners loop
                  begin
                    if R.Ph.Selectionid > 0 then
                      R.Win.Profit  := Sim.Rewards_Map(Market.Marketid)(R.Ph.Selectionid)(Timestamp.To_String)/0.95;
                      R.Win.Marketid := Market.Marketid;
                      R.Win.Betplaced := R.Ph.Pricets;
                      R.Win.Pricematched := 1.0 + (R.Win.Profit/Global_Back_Size);
                      Dummy_Selid := R.Ph.Selectionid; -- just get a non-zero selid
                    end if;
                  exception
                    when Constraint_Error => null;
--                      Text_Io.Put_Line("Key was not in map, continue");
                  end;
                end loop;

                if Plc_Found then
                  declare
                    Closest_Timestamp_String : Sim.Timestamp_String_Key_Type := (others => 'x');
                  begin

                    begin
                      Closest_Timestamp_String := Sim.Wints_Placets_Map(Timestamp.To_String);
                      Plc_Market_Ok := Closest_Timestamp_String(1) /= 'x';
                    exception
                      when Constraint_Error =>
                        Plc_Market_Ok := False;
                    end ;

                    if Plc_Market_Ok then
                      for R of Best_Runners loop
                        begin
                          if R.Ph.Selectionid > 0 then
                            R.Plc.Profit := Sim.Rewards_Map(Plc_Market.Marketid)(R.Ph.Selectionid)(Closest_Timestamp_String)/0.95;
                            R.Plc.Marketid := Plc_Market.Marketid;
                            R.Plc.Betplaced := Calendar2.To_Time_Type(Closest_Timestamp_String);
                            R.Plc.Pricematched := 1.0 + (R.Plc.Profit/Global_Back_Size);
                          end if;
                        exception
                          when Constraint_Error => null;
                            --                      Text_Io.Put_Line("Key was not in map, continue");
                        end;
                      end loop;
                    end if;
                  end ;
                end if;

--                    Laybet_Placed := True;
--                    if not Laybet_Placed then
--                      Try_Place_Lay_Bet(Market,Best_Runners,Global_Bet_List,Laybet_Placed);
--                    else
                    Try_Place_Back_Bet(Win_M      => Market,
                                       Plc_M      => Plc_Market,
                                       Backdiff   => Global_Backdiff,
                                       Bet_At_Max => Global_Bet_At_Max,
                                       Br         => Best_Runners,
                                       Bl         => Global_Bet_List,
                                       Plc_Ok     => Plc_Market_Ok,
                                       Bet_Placed => Backbet_Placed);
--                  end if;

              end;
              exit Loop_Market when Backbet_Placed;
            end loop Loop_Timestamp; --  Timestamp
          end;
        end if; -- Is_Win
      end loop Loop_Market;  -- marketid

    end;
    Log("num bets to insert" & Global_Bet_List.Length'Img);

    declare
      --        Profit, Sum, Sum_Winners, Sum_Losers  : array (Side_Type'range) of Fixed_Type   := (others => 0.0);
      --        Winners, Losers, Unmatched, Strange   : array (Side_Type'range) of Integer_4 := (others => 0);
      T : Sql.Transaction_Type with Warnings => Off;
      Eos : Boolean := False;
    begin
      T.Start;
      for Bet of Global_Bet_List loop
        Log("insert " & Bet.To_String);
        --Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
        Bet.Insert;
        if Bet.Status(1..6) = "LAPSED" then
          Bet.Read(Eos);
          Bet.Nullify_Betwon;
        end if;
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
end Do_Greenup_1 ;
