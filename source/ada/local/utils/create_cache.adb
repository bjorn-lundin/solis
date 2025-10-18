
with Sim;
with Stacktrace;
with Sql;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Ada.Environment_Variables;
with Bot_Types; use Bot_Types;

procedure Create_Cache is

  One_Day      : Interval_Type :=  (1, 0, 0, 0, 0); -- 1 day
  Start        : Time_Type := Clock;
  Date_Start   : Time_Type := (2016, 3, 16, 00, 00, 00, 000);
  Date_Stop    : Time_Type := Start + One_Day;
  Current_Date : Time_Type := Date_Start - One_Day; -- 1 day

  Cmd_Line     : Command_Line_Configuration;
  Sa_Startdate     : aliased Gnat.Strings.String_Access;
  Sa_Stopdate      : aliased Gnat.Strings.String_Access;
  Sa_Animal        : aliased Gnat.Strings.String_Access;
  Ba_Rewards       : aliased Boolean := False;

  Animal           : Animal_Type := Horse;


  package Ev renames Ada.Environment_Variables;
  Db           : String (1..3) := (others => ' ');
begin

  if not Ev.Exists ("BOT_NAME") then
    Ev.Set ("BOT_NAME", "create_cache");
  end if;

  Define_Switch
    (Cmd_Line,
     Ba_Rewards'Access,
     Long_Switch => "--rewards",
     Help        => "create rewards too?");

  Define_Switch
    (Cmd_Line,
     Sa_Animal'Access,
     Long_Switch => "--animal=",
     Help        => "horse|hound|human");

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
    Date_Start := Calendar2.To_Time_Type(Sa_Startdate.all,"");
  end if;

  if Sa_Stopdate.all /= "" then
    Date_Stop := Calendar2.To_Time_Type(Sa_Stopdate.all,"");
  end if;


  Logging.Open (Ev.Value ("BOT_HOME") & "/log/" &  Ev.Value ("BOT_NAME") & ".log");


  Log("Sa_Animal.all  " & Sa_Animal.all);
  Log("start_date '" & Sa_Startdate.all & "'");
  Log("stop_date  '" & Sa_Stopdate.all & "'");
  Log("Ba_Rewards     " & Ba_Rewards'Img);

  Current_Date := Date_Start; -- 1 day

  if Sa_Animal.all = "horse" then
    Animal := Horse;
    Db := "bnl";
  elsif Sa_Animal.all = "hound" then
    Animal := Hound;
    Db := "ghd";
  elsif Sa_Animal.all = "human" then
    Animal := Human;
  end if;
  Log ("animal " & Animal'Img);

  Sql.Connect
    (Host     => "localhost",
     Port     => 5432,
     Db_Name  => Db,
     Login    => "bnl",
     Password => "bnl");
  Log ("Connected to db: " & Db);

  Log ("Current date='" & Current_Date.String_Date_Iso & "' Date_Stop='" & Date_Stop.String_Date_Iso & "'");

  case Animal is
    when Horse | Hound =>
      loop
        exit when Current_Date > Date_Stop;
        Sim.Fill_Win_Place_Map(Current_Date, Animal, Sim.Win_Place_Map);

      ---  Sim.Fill_Data_Maps (Current_Date, Animal => Animal, Rewards => Ba_Rewards, Racetimes => False, Race_Prices => True);
        Current_Date := Current_Date + One_Day;
      end loop;

    when Human => null;
  end case;
  Log ("Started : " & Start.To_String);
  Log ("Done : " & Calendar2.Clock.To_String);
  Sql.Close_Session;


exception
  when Gnat.Command_Line.Exit_From_Command_Line => null;

  when E : others =>
    Stacktrace.Tracebackinfo (E);
end Create_Cache;
