--with Ada.Strings;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
--with Ada.Containers;
with Text_Io;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Sim;
--with Utils; use Utils;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Price_Histories;
with Bets;
with Calendar2;  use Calendar2;
with Logging; use Logging;
--with Bot_System_Number;


procedure Check_New_Strategy is

  package Ev renames Ada.Environment_Variables;


  Cmd_Line        :  Command_Line_Configuration;

  Sa_Strategy : aliased Gnat.Strings.String_Access;
  Start       : Calendar2.Time_Type := Calendar2.Clock;

  Betlist     : Bets.Lists.List;
  Select_Bets : Sql.Statement_Type;


  -----------------------------------------


  Start_Day     : Time_Type := (2010,01,18,00,00,00,000);
  Tmp_Day     : Time_Type := (2020,01,31,00,00,00,000);
  Current_Day : Time_Type := (2020,01,31,00,00,00,000);
  Profit      : Fixed_Type := 0.0;
  Total_Profit : Fixed_Type := 0.0;
  Real_Total_Profit : Fixed_Type := 0.0;

  Selmap : Sim.Selectionid_Maps.Map;
  Tsmap  : Sim.Timestamp_To_Reward_Maps.Map;
  T      : Sql.Transaction_Type with Warnings => Off;
begin

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME", "check_new_strategy");
  end if;

  --Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

  Log ("Connect db");
  Sql.Connect
    (Host     => "192.168.1.7",
     Port     => 5432,
     Db_Name  => "bnl",
     Login    => "bnl",
     Password => "ld4BC9Q51FU9CYjC21gp");
  Log ("Connected to db");


  Define_Switch
    (Cmd_Line,
     Sa_Strategy'Access,
     Long_Switch => "--betname=",
     Help        => "Whatever it is called in the db");

  Getopt (Cmd_Line);  -- process the command line


  if Sa_Strategy.all = "" then
    Text_Io.Put_Line("Need strategy name");
    return;
  end if;

  Current_Day := (2020,01,31,23,59,59,999);

  Log("Sa_Strategy.all  " & Sa_Strategy.all);


  T.Start;
  Select_Bets.Prepare("select * from ABETS where BETNAME=:BETNAME and BETPLACED < :DATE and BETPLACED > :START order by BETPLACED");
  Select_Bets.Set("BETNAME", Sa_Strategy.all);
  Select_Bets.Set("DATE", Current_Day);
  Select_Bets.Set("START", Start_Day);
  Bets.Read_List(Stm => Select_Bets, List => Betlist);

  for Bet of Betlist loop

    if Bet.Startts.Year /= Current_Day.Year
      or else Bet.Startts.Month /= Current_Day.Month
      or else Bet.Startts.Day /= Current_Day.Day
    then
      begin
        Tmp_Day := Bet.Startts;
        Tmp_Day.Hour := 1;
        Text_Io.Put_Line("Fill maps " & Tmp_Day.String_Date_Time_Iso( T => " " , Tz => ""));
        Sim.Fill_Data_Maps(Date => Tmp_Day, Animal => Horse, Rewards => True, Racetimes => False, Race_Prices => False);
      exception
        when E: Constraint_Error =>
          Stacktrace.Tracebackinfo(E);
      end;
      Current_Day := Bet.Startts;
    end if;

    begin
      Selmap := Sim.Rewards_Map(Bet.Marketid);
    exception
      when Constraint_Error =>
        Text_Io.Put_Line("key not in map 1 " & Bet.Marketid);
    end;

    declare
      Ts : Time_Type;
      Found : Boolean := True;
    begin
      Tsmap := Selmap(Bet.Selectionid);
      Ts := Bet.Betplaced;
      loop
        Found := Tsmap.Contains(Ts.String_Date_Time_Iso( T => " " , Tz => ""));
        exit when Found or else Bet.Betplaced - Ts > (0,0,0,1,500); -- move max 1.5 secs back
        Ts := Ts - (0,0,0,0,1); -- add a ms
      end loop;

      if Found then
        Profit := Tsmap(Ts.String_Date_Time_Iso( T => " " , Tz => ""));
      else
        Profit := 0.0;
      end if;

    exception
      when Constraint_Error =>
        Text_Io.Put_Line("key not in map 2" & Bet.Selectionid'Img);
    end;

    Total_Profit := Total_Profit + Profit;
    Real_Total_Profit := Real_total_Profit + Bet.Profit;
    Log("profit : " & Profit'Img);
  end loop;
  T.Commit;
  Sql.Close_Session;

  Log("Total_Profit      : " & Total_Profit'Img);
  Log("Real_Total_Profit : " & Real_Total_Profit'Img);
  Log("Started : " & Start.To_String);
  Log("Done : " & Calendar2.Clock.To_String);
  Logging.Close;
exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Check_New_Strategy ;
