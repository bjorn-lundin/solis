with Ada.Strings; use Ada.Strings;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;

--with Sim;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Text_Io;
--with Price_Histories; use Price_Histories;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
--with Calendar2;  use Calendar2;
with Logging; use Logging;
--with Markets;
--with Runners;
with Bot_Svn_Info;
with Ini;
--with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Containers.Doubly_Linked_Lists;
with Types; use Types;
with Bets;
with Tics;
with Utils; use Utils;
with Bot_System_Number;

procedure Back_During_Race_1_lay_limit is
  use type Ada.Containers.Count_Type;

  package Ev renames Ada.Environment_Variables;


  All_Betnames : Sql.Statement_Type;
  All_Betnames_Matching : Sql.Statement_Type;
  package Betname_Pack is new Ada.Containers.Doubly_Linked_Lists(Betname_Type);
  Betname_List : Betname_Pack.List;


  subtype Key is String(1..7);

  package Odds_Maps is new Ada.Containers.Hashed_Maps
    (Key,
     Natural,
     Ada.Strings.Hash,
     "=",
     "=");



  ------------------------------
  procedure Treat(Name : String) is
    T : Sql.Transaction_Type;
    Price : Fixed_Type;
    Betlist : Bets.Lists.List;
  begin
            -- ca 8 - 100
    for Tic in Tics.Tics_Type(200) .. Tics.Tics_Type(260) loop
      Betlist.Clear;
      Price := Tics.Get_Tic_Price(Tic);
      T.Start;
      All_Betnames_Matching.Prepare("select * from ABETS where BETNAME = :BETNAME and PRICE <= :PRICE");
      All_Betnames_Matching.Set("BETNAME",Name);
      All_Betnames_Matching.Set("PRICE",Price);
      Bets.Read_List(All_Betnames_Matching,Betlist);

      for B of Betlist loop
        Move(Trim(B.Betname,Both) & "_" & F8_Image(Price),B.Betname);
        B.Betid := Integer_8(bot_System_Number.New_Number(System_Number_Type => Bot_System_Number.Betid));
        B.Powerdays := 5;
        B.Insert;
      end loop;

      T.Commit;
    end loop;

  end Treat;

  ------------------------------

  T                   :          Sql.Transaction_Type;
  Cmd_Line            :          Command_Line_Configuration;
  Sa_Logfilename      : aliased  Gnat.Strings.String_Access;
  Sa_Max_Leader_Price : aliased  Gnat.Strings.String_Access;
  Sa_Back_Price       : aliased  Gnat.Strings.String_Access;
  Sa_Delta_Price      : aliased  Gnat.Strings.String_Access;
  Eos                 : Boolean := False;
  Betname             : Betname_Type;

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
    All_Betnames.Prepare("select distinct(BETNAME) from ABETS where BETNAME like 'WIL%'");
    All_Betnames.Open_Cursor;
    loop
      All_Betnames.Fetch(Eos);
      exit when Eos;
      All_Betnames.Get("BETNAME", Betname);
      Betname_List.Append(Betname);
    end loop;
    All_Betnames.Close_Cursor;
  T.Commit;

  for B of Betname_List Loop
    Log(B);
    Treat(B);
  end loop;

  Sql.Close_Session;

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
    Sql.Close_Session;
end Back_During_Race_1_lay_limit;
