--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;

with Sim;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Text_Io;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Bot_Svn_Info;
with Ini;
with Events;
with Markets;

procedure Race_Data is

  package Ev renames Ada.Environment_Variables;

  Start_Date     : constant Calendar2.Time_Type := (2016,03,16,0,0,0,0);
  One_Day        : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date   :          Calendar2.Time_Type := Start_Date;
  Stop_Date      :          Calendar2.Time_Type := (2018,03,01,0,0,0,0);
  T              :          Sql.Transaction_Type;
  Cmd_Line       :          Command_Line_Configuration;
  Sa_Logfilename : aliased  Gnat.Strings.String_Access;

begin
  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","race_data");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log("main", "Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database_home", "host", ""),
     Port     => Ini.Get_Value("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value("database_home", "name", ""),
     Login    => Ini.Get_Value("database_home", "username", ""),
     Password => Ini.Get_Value("database_home", "password", ""));
  Log("main", "db Connected");

  Log("main", "params start");

  Log("main", "params stop");


  Date_Loop : loop
    T.Start;
   -- Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse);
    Sim.Read_All_Markets(Current_Date, Bot_Types.Horse, Sim.Market_With_Data_List);
   -- Sim.Fill_Win_Place_Map(Current_Date, Bot_Types.Horse, Sim.Win_Place_Map);

    Log("start process " & Current_Date.String_Date_ISO);
    begin
      Market_Loop : for Market of Sim.Market_With_Data_List loop
        declare
          Ev : Events.Event_Type;
          Eos,Found : Boolean := False;
          Idx : Natural := 0;
          Market_Win : Markets.Market_Type;
        begin
          Ev.Eventid := Market.Eventid;
          Ev.Read(Eos);
          for I in Ev.Eventname'Range loop
            if Ev.Eventname(I) = ' ' then
              Idx := I;
              exit;
            end if;
          end loop;

          if Market.Markettype(1) = 'P' then
            Market.Corresponding_Win_Market(Market_Win, Found);
          else
            Market_Win.Marketname := Market.Marketname;
          end if;

          Log("|datapoint|" &
                Market.Marketid & "|" &
                Market.Marketname & "|" &
                Market_Win.Marketname & "|" &
                Market.Markettype(1..3) & "|" &
                Ev.Eventname(1..Idx-1) & "|" );
        end;
      end loop Market_Loop;
    end;

    T.Commit;

    Current_Date := Current_Date + One_Day;
    exit Date_Loop when Current_Date = Stop_Date;

  end loop Date_Loop;

  Sql.Close_Session;    -- no need for db anymore

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Race_Data;
