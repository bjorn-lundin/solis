with Ada.Environment_Variables;
--with Ada.Containers;
with Gnat.Command_Line; use Gnat.Command_Line;
with GNAT.Strings;

with Sim;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Price_Histories;
with Bets;
with Calendar2;  use Calendar2;
with Logging; use Logging;
--with Bot_System_Number;
--with Prices;
with Price_Histories;

procedure Check_Matched_Sim_Bets is

  package EV renames Ada.Environment_Variables;
  Cmd_Line   : Command_Line_Configuration;

  Sa_Animal                      : aliased Gnat.Strings.String_Access;
  B_Match_Directly               : aliased Boolean := False;
  Global_Animal                  : Animal_Type := Horse; --default
  Start             : Calendar2.Time_Type := Calendar2.Clock;

  Commission : constant Fixed_Type := 5.0 / 100.0;

  --  Enough_Runners : Boolean := False;
  --use type Ada.Containers.Count_Type;
  --Price   : Prices.Price_Type;

  Day      : Time_Type     := (2018, 10, 31, 00, 00, 00, 000);
  End_Date : Time_Type     := (2020,  3, 31, 00, 00, 00, 000);
  One_Day  : Interval_Type := (1, 0, 0, 0, 0);

begin

  if not EV.Exists ("BOT_NAME") then
    EV.Set (Name => "BOT_NAME", Value => "sim_back_in_play");
  end if;

  -- Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

  Define_Switch
    (Cmd_Line,
     SA_Animal'Access,
     Long_Switch => "--animal=",
     Help        => "horse|hound|human");

Define_Switch
    (Cmd_Line,
     B_Match_Directly'Access,
     Long_Switch => "--matchdirectly",
     Help        => "match the bet with 2 secs of when it was put");



  Getopt (Cmd_Line);  -- process the command line


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

  Day_Loop : loop
    exit Day_Loop when Day > End_Date;
    Sim.Fill_Data_Maps (Day, Animal => Global_Animal);
    Log ("start process date " & Day.To_String);

    declare
      Cnt     : Integer := 0;
      Is_Win  : Boolean := True;
      Betlist : Bets.Lists.List;
      Tmp_Bet : Bets.Bet_Type;
      T       : Sql.Transaction_Type;
    begin
      Log ("num markets " & Day.To_String & " " & Sim.Market_With_Data_List.Length'Img);

      Loop_Market : for Market of Sim.Market_With_Data_List loop
        Is_Win := Market.Markettype (1 .. 3) = "WIN";

        if Is_Win then
          Betlist.Clear;
         -- Log ("Treat market " & Market.To_String );
          Cnt := Cnt + 1;
          -- list of timestamps in this market

          T.Start;
          Tmp_Bet.Marketid := Market.Marketid;
          Bets.Read_Marketid (Data  => Tmp_Bet,
                              List  => Betlist);
          T.Commit;
          Log ("Treat marketid '" & Market.Marketid & " has #bets=" & Betlist.Length'Img );

          declare
            Timestamp_To_Apriceshistory_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                Sim.Marketid_Timestamp_To_Prices_History_Map (Market.Marketid);
          --  First                           : Boolean := True;
          begin
            Loop_Timestamp : for Timestamp of Sim.Marketid_Pricets_Map (Market.Marketid) loop
              --Log ("Treat marketid '" & Market.Marketid & "' pricets " & Timestamp.To_String);
              Betloop: for Bet of Betlist loop
                if Timestamp >= Bet.Betplaced then
                  declare
                    List    : Price_Histories.Lists.List :=
                                Timestamp_To_Apriceshistory_Map (Timestamp.To_String);
                    Do_Update : Boolean := False;
                  begin
                    Item_Loop : for Item of List loop
                      Do_Update := False;
                      if Item.Selectionid = Bet.Selectionid then
                        if Bet.Status(1) = 'U'
                          and then Bet.Betplaced + (0, 0, 0, 1, 0) <= Timestamp
                         -- and then   --matched within 2 secs of palce
                         --   (B_Match_Directly and then Timestamp <= Bet.Betplaced + (0, 0, 0, 2, 0))
                        then
                          Log ("Treat marketid '" & Market.Marketid &
                               " selid" & Bet.Selectionid'Img &
                               " betid: " & Bet.Betid'Img &
                               " betplaced " & Bet.Betplaced.To_String &
                               " "  & Timestamp.To_String);


                          Bet.Betwon:= Sim.Is_Race_Winner(Bet.Selectionid, Bet.Marketid);

                          -- is a match ?
                          if Bet.Side = "BACK" then
                            if Item.Backprice >= Bet.Price then
                              Bet.Pricematched := Item.Backprice;
                              Bet.Status (1) := 'M';
                              if Bet.Betwon then
                                Bet.Profit := (1.0 - Commission) * Bet.Sizematched * (Bet.Pricematched - 1.0);
                              else
                                Bet.Profit := -Bet.Sizematched;
                              end if;
                              Do_Update := True;
                            end if;
                          else
                            if Item.Layprice <= Bet.Price then
                              Bet.Pricematched := Item.Layprice;
                              Bet.Status(1) := 'M';
                              Do_Update := True;
                              if Bet.Betwon then
                                Bet.Profit := (1.0 - Commission) * Bet.Sizematched;
                              else
                                Bet.Profit := - Bet.Sizematched * (Bet.Pricematched - 1.0);
                              end if;
                            end if;
                          end if;

                        end if;
                      end if;
                      if Do_Update then
                        T.Start;
                        Log ("main", "Update bet:" & Bet.To_String);
                        Bet.Update_Withcheck;
                        T.Commit;
                      end if;
                    end loop Item_Loop;
                  end;
                end if;
              end loop Betloop;
            end loop Loop_Timestamp;
          exception
            when Constraint_Error =>
              Log ("main", "Timestamp not in map: Constraint_Error caught "  );
          end;
        end if; -- Is_Win
      end loop Loop_Market;  -- marketid
    end;
    Day := Day + One_Day;
  end loop Day_Loop;
  Sql.Close_Session;
  Log ("Started : " & Start.To_String);
  Log ("Done : " & Calendar2.Clock.To_String);
  Logging.Close;

exception
  when E : others =>
    Stacktrace.Tracebackinfo (E);
end Check_Matched_Sim_Bets ;

