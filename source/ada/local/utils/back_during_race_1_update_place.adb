with Ada.Strings; use Ada.Strings;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;

--with Sim;
--with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Text_Io;
--with Price_Histories; use Price_Histories;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Calendar2;  use Calendar2;
with Logging; use Logging;
--with Markets;
--with Runners;
with Bot_Svn_Info;
with Ini;
--with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Types; use Types;
with Bets;
--with Utils; use Utils;
with Sim;
with Price_Histories;
with Bot_Types;

procedure Back_During_Race_1_Update_Place is
  use type Ada.Containers.Count_Type;

  package Ev renames Ada.Environment_Variables;


  All_Place_Bets : Sql.Statement_Type;
--  package Betname_Pack is new Ada.Containers.Doubly_Linked_Lists(Betname_Type);
  Betlist : Bets.Lists.List;


  subtype Key is String(1..7);

  package Odds_Maps is new Ada.Containers.Hashed_Maps
    (Key,
     Natural,
     Ada.Strings.Hash,
     "=",
     "=");

  ------------------------------
  procedure Treat(Bet : in out Bets.Bet_Type) is
  begin
    declare
      Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                          Sim.Marketid_Timestamp_To_Prices_History_Map(Bet.Marketid);
    begin
      Loop_Ts : for Timestamp of Sim.Marketid_Pricets_Map(Bet.Marketid) loop
        declare
          List           : Price_Histories.Lists.List := Timestamp_To_Prices_History_Map(Timestamp.To_String);
          R              : Price_Histories.Price_History_Type;
          Price_Ok       : Boolean := False;
          Pricematched   : Fixed_Type := 0.0;
          Is_Race_Winner : Boolean := False;
        begin

          if Bet.Status(1) = 'U' then -- found unmatched bet

           -- Log("checking bet ", Bet.To_String);
            for Runner of List loop      --find runner
              if Runner.Selectionid = Bet.Selectionid then
                R := Runner;
                exit;
              end if;
            end loop;


            if R.Selectionid > 0 and then R.Pricets > Bet.Betplaced + (0,0,0,1,0) then -- 1 second later at least, time for BF delay

              if Bet.Side(1..3) = "LAY" then
                Price_Ok := R.Layprice <= Bet.Price and then R.Layprice > Fixed_Type(1.0) ; -- sanity
                Pricematched := R.Layprice;
              elsif Bet.Side(1..4) = "BACK" then
                Price_Ok := R.Backprice >= Bet.Price and then
                R.Backprice > Fixed_Type(1.0) and then R.Backprice < Fixed_Type(1000.0);  -- sanity
                Pricematched := R.Backprice;
              end if;
         --     Log("Price_OK ", Price_Ok'Img);

              if Price_Ok then
                Move("MATCHED",Bet.Status);
                Bet.Pricematched := Pricematched;
                begin
                  Is_Race_Winner := Sim.Is_Race_Winner(Bet.Selectionid, Bet.Marketid);

                  if Bet.Side(1..3) = "LAY" then
                    Bet.Betwon := not Is_Race_Winner;
                    if Bet.Betwon then
                      Bet.Profit :=  Bet.Sizematched; -- commission is calculated/market
                    else
                      Bet.Profit := -Bet.Sizematched * (Bet.Pricematched - 1.0);
                    end if;

                  elsif Bet.Side(1..4) = "BACK" then
                    Bet.Betwon := Is_Race_Winner;
                    if Bet.Betwon then
                      Bet.Profit :=  Bet.Sizematched * (Bet.Pricematched - 1.0);  -- commision is calculated/market
                    else
                      Bet.Profit := -Bet.Sizematched;
                    end if;
                  end if;

                  Bet.Update;
               --   Log("Bet_Updated", Bet.To_String);
                exception
                  when others =>
                    Log("No-race-WInner ", "winner is missing in " & Bet.Marketid);
                end;
              end if;
            end if;
          end if;
        end;
      end loop Loop_Ts; --  Timestamp
      --Bets.Sum_Laybets(Bet_List, -12_000.0);
    end;

  exception
    when Constraint_Error =>
      Bet.Powerdays := 7;
      Bet.Update;
  end Treat;

  ------------------------------

  T                   :          Sql.Transaction_Type;
  Cmd_Line            :          Command_Line_Configuration;
  Sa_Logfilename      : aliased  Gnat.Strings.String_Access;
  Sa_Max_Leader_Price : aliased  Gnat.Strings.String_Access;
  Sa_Back_Price       : aliased  Gnat.Strings.String_Access;
  Sa_Delta_Price      : aliased  Gnat.Strings.String_Access;

  Bet_Date            : Calendar2.Time_Type := Calendar2.Time_Type_First;

begin
  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Define_Switch
    (Cmd_Line,
     Sa_Max_Leader_Price'Access,
     Long_Switch => "--max_leader_price=",
     Help        => "leader's back price must be lower that this");

  Define_Switch
    (Cmd_Line,
     Sa_Delta_Price'Access,
     Long_Switch => "--delta=",
     Help        => "diff between #1 and #2");

  Define_Switch
    (Cmd_Line,
     Sa_Back_Price'Access,
     Long_Switch => "--back_price=",
     Help        => "back the runner at this price");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","lay_during_race3");
  end if;

--  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);
  Log("main", "params start");
  Log("main", "Sa_Max_Leader_Price" & Sa_Max_Leader_Price.all);
  Log("main", "Sa_Lay_Price" & Sa_Back_Price.all);
  Log("main", "Sa_Delta_Price" & Sa_Delta_Price.all);
  Log("main", "params stop");

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");

  Log("main", "Connect Db " &
        Ini.Get_Value("database_home", "host", "")  & " " &
        Ini.Get_Value("database_home", "port", 5432)'Img & " " &
        Ini.Get_Value("database_home", "name", "") & " " &
        Ini.Get_Value("database_home", "username", "") & " " &
        Ini.Get_Value("database_home", "password", "")
     );

  Sql.Connect
    (Host     => Ini.Get_Value("database_home", "host", ""),
     Port     => Ini.Get_Value("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value("database_home", "name", ""),
     Login    => Ini.Get_Value("database_home", "username", ""),
     Password => Ini.Get_Value("database_home", "password", ""));
  Log("main", "db Connected");



  T.Start;
    All_Place_Bets.Prepare("select * from ABETS where BETNAME like 'PLC%' and STATUS <> 'MATCHED' order by BETPLACED");
    Bets.Read_List(All_Place_Bets, Betlist);
  T.Commit;
  Log("main", "#bets" & Betlist.Length'Img);

  for B of Betlist loop

    if Bet_Date.Year /= B.Betplaced.Year or else
      Bet_Date.Month /= B.Betplaced.Month or else
      Bet_Date.Day /= B.Betplaced.Day then
      Bet_Date := B.Betplaced;
      Sim.Fill_Data_Maps(Date => Bet_Date, Animal => Bot_Types.Horse);
    end if;


    T.Start;
    --Log(B);
    Treat(B);
    T.Commit;
  end loop;

  Sql.Close_Session;

  Log("main", "Done");
exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
    Sql.Close_Session;
end Back_During_Race_1_Update_Place;
