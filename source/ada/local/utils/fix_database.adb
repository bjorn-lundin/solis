
--with Gnat.Command_Line; use Gnat.Command_Line;
--with Gnat.Strings;

with Ada.Exceptions;
with Stacktrace;
with Ada.Command_Line;

with Sql;
with Calendar2; use Calendar2;
--with Logging;               use Logging;
with Text_Io;
with Ini;
with  Ada.Environment_Variables;
--with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
--with Bot_Types;
with Utils; use Utils;
with Price_Histories;
with Markets;
with Runners;
with Types; use Types;

procedure Fix_Database is
  package Ev renames Ada.Environment_Variables;
  -- Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type;

  --Sa_Marketid            : aliased Gnat.Strings.String_Access;
  --  Ba_Print_Bet           : aliased Boolean := False;
  Ml                     : Markets.Lists.List;
  Select_Market          : Sql.Statement_Type;
  Select_Markets         : Sql.Statement_Type;
  Find_Placement         : Sql.Statement_Type;
  Delete_Markets,
  Delete_Runners,
  Delete_Prices,
  Delete_Priceshistory : Sql.Statement_Type;



  Cnt,Tot                : Integer_4 := 0;
  Gdebug                 : Boolean := True;


  -------------------------------
  procedure Debug (What : String) is
  begin
    if Gdebug then
      Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_Iso (Clock, " " , "") & " " & What);
    end if;
  end Debug;
  pragma Warnings(Off, Debug);
  -------------------------------


  function Is_Ok(M : Markets.Market_Type) return Boolean is
    Eos         : Boolean := True;
    Runner      : Runners.Runner_Type;
    Same_Second : Boolean := False;
    Ts          : Calendar2.Time_Type := Calendar2.Clock;
    Ph          : Price_Histories.Price_History_Type;
    Found       : Boolean := False;
  begin

    Find_Placement.Prepare("select * from ARUNNERS where MARKETID=:MARKETID and STATUS =:STATUS");
    Find_Placement.Set("MARKETID", M.Marketid);
    Find_Placement.Set("STATUS", "WINNER");

    Find_Placement.Open_Cursor;
    Find_Placement.Fetch (Eos);
    if not Eos then
      Runner := Runners.Get(Find_Placement);
    else
      Debug("IS_Ok - no winner " & M.To_String);
    end if;
    Find_Placement.Close_Cursor;
    if Eos then
      return False;
    end if;

    Select_Market.Prepare("select * from APRICESHISTORY " &
                            "where MARKETID =:MARKETID " &
                            "and SELECTIONID = :SELECTIONID " &
                            "order by PRICETS");
    Select_Market.Set("MARKETID",M.Marketid);
    Select_Market.Set("SELECTIONID",Runner.Selectionid);

    Select_Market.Open_Cursor;
    loop
      Select_Market.Fetch (Eos);
      exit when Eos;
      Ph := Price_Histories.Get(Select_Market);
      Found := True;
      Same_Second := Ph.Pricets.Second = Ts.Second;
      exit when Same_Second;
      Ts.Second := Ph.Pricets.Second;
    end loop;
    Select_Market.Close_Cursor;
    return Same_Second and Found;
  end Is_Ok;

  ----------------------------------------------

  procedure Delete_Market_Data(M : Markets.Market_Type) is
    Num_Deleted : Natural := 0;
  begin

    Delete_Markets.Prepare("delete from AMARKETS where MARKETID=:MARKETID");
    Delete_Runners.Prepare("delete from ARUNNERS where MARKETID=:MARKETID");
    Delete_Prices.Prepare("delete from APRICES where MARKETID=:MARKETID");
    Delete_Priceshistory.Prepare("delete from APRICESHISTORY where MARKETID=:MARKETID");

    Delete_Markets.Set("MARKETID", M.Marketid);
    Delete_Runners.Set("MARKETID", M.Marketid);
    Delete_Prices.Set("MARKETID", M.Marketid);
    Delete_Priceshistory.Set("MARKETID", M.Marketid);

    Debug ("Delete_Markets_Data: Market" & M.To_String);
    Delete_Markets.Execute(No_Of_Affected_Rows => Num_Deleted);
    Debug ("Delete_Markets_Data-Markets      - deleted=" & Num_Deleted'Img);
    Delete_Runners.Execute(No_Of_Affected_Rows => Num_Deleted);
    Debug ("Delete_Market_Data-Runners       - deleted=" & Num_Deleted'Img);
    Delete_Prices.Execute(No_Of_Affected_Rows => Num_Deleted);
    Debug ("Delete_Market_Data-Prices        - deleted=" & Num_Deleted'Img);
    Delete_Priceshistory.Execute(No_Of_Affected_Rows => Num_Deleted);
    Debug ("Delete_Market_Data-Priceshistory - deleted=" & Num_Deleted'Img);
    --raise Program_Error with "testing and rollback";

  end Delete_Market_Data;




begin


  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");

  Debug("Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database_home","host",""),
     Port     => Ini.Get_Value("database_home","port", 5432),
     Db_Name  => Ini.Get_Value("database_home","name",""),
     Login    => Ini.Get_Value("database_home","username",""),
     Password => Ini.Get_Value("database_home","password",""));
  Debug("db Connected");


  T.Start;
  Select_Markets.Prepare("select * from AMARKETS M, AEVENTS E " &
                           "where M.EVENTID = E.EVENTID " &
                           "and E.EVENTTYPEID = 7 " &
                           "and M.NUMRUNNERS >= 7 " &
                           "and M.MARKETTYPE = 'PLACE' " &
                           "order by M.STARTTS");

  Markets.Read_List(Select_Markets,Ml);
  T.Commit;

  Tot := Integer_4(Ml.Length);
  Debug ("# Markets =" & Tot'Img);

  for M of Ml loop
    Cnt := Cnt +1;
    if Cnt mod 100 = 0 then
      Debug (Cnt'Img & "/" & Tot'Img & " -> " &
               F8_Image(Fixed_Type(100.0 * Float(Cnt) / Float(Tot))) & " %");
    end if;

    T.Start;
    if not Is_Ok(M) then
      Delete_Market_Data(M);
    end if;
    T.Commit;
  end loop;

  Sql.Close_Session;

exception
  when E : others =>

    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name (E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message (E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information (E);
    begin
      Debug (Last_Exception_Name);
      Debug ("Message : " & Last_Exception_Messsage);
      Debug (Last_Exception_Info);
      Debug ("addr2line" & " --functions --basenames --exe=" &
             Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump (Last_Exception_Info));
    end ;
    T.Rollback;
    Sql.Close_Session;

end Fix_Database;
