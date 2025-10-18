--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Types ; use Types;
with Stacktrace;
with Sql;
--with Text_Io;
with Bets;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Bot_Svn_Info;
with Ini;
with Ada.Environment_Variables;

procedure Delete_Duplicate_Bets is

  package Ev renames Ada.Environment_Variables;

  T        : Sql.Transaction_Type;
  Bet_List : Bets.Lists.List;
  Stm      : Sql.Statement_Type;
  Old_Bet  : Bets.Bet_Type;
  Cnt      : Integer;

begin


  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);
  Log("main", "params start");
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
  Stm.Prepare(
    "select * " &
    "from abets " &
    "where true " &
    "and betname like 'WIN_LAY%TH' " &
    "order by marketid, selectionid, betplaced" );

  T.Commit;
  Log("main", "read list");
  Bets.Read_List(Stm,Bet_List);
  Log("main", "process list");

  Cnt := Integer(Bet_List.Length);

  T.Start;

  for B of Bet_List loop
    if B.Marketid = Old_Bet.Marketid and then
       B.Selectionid = Old_Bet.Selectionid and then
       B.Betplaced = Old_Bet.Betplaced then

       B.Delete_Withcheck;
    end if;
    Old_Bet := B;

    Cnt := Cnt -1;
    if Cnt rem 1_000 = 0 then
      T.Commit;
      T.Start;
      Log("main", "Cnt =" & Cnt'Img);
    end if;

  end loop;
  T.Commit;
  Sql.Close_Session;

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
    Sql.Close_Session;
end Delete_Duplicate_Bets;
