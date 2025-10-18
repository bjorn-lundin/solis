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


procedure Race_Length is

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
    Ev.Set("BOT_NAME","race_length");
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
    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse);
    Log("start process " & Current_Date.String_Date_ISO);
    begin
      Market_Loop : for Market of Sim.Market_With_Data_List loop
        if Market.Markettype(1..3) = "WIN" then
          -- list of timestamps in this market
          declare
            Race_Start_Time    : Time_Type := Time_Type_Last;
            Last_Time          : Time_Type := Time_Type_First;
            Found              : Boolean := False;
          begin
            -- check start of race
            Loop_Ts1 : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              Log("Timestamp: " & Timestamp.To_String(Milliseconds => True) & " Last_time: " & Last_Time.To_String(Milliseconds => True));
              if Last_Time = Time_Type_First then
                null; -- skip first sample
              elsif not Found and then Timestamp - Last_Time < (0,0,0,1,0) then -- race started
                Race_Start_Time := Timestamp;
                Log("Race_Start_Time found " & Race_Start_Time.To_String(Milliseconds => True) & "/" & Timestamp.To_String(Milliseconds => True) );
                Found := True;
              end if;
              Last_Time := Timestamp;
            end loop Loop_Ts1;
            Log("|datapoint|" &
                  String_Interval(Interval => Last_Time - Race_Start_Time, Days => False, Hours => False) & "|" &
                  Market.Marketid & "|" &
                  Market.Marketname & "|" );
          end;
        end if; -- Market_type(1..3) = WIN
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
end Race_Length;
