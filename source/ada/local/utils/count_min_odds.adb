with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Containers;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
with Rpc;
--with Lock ;
with Ini;
with Logging; use Logging;
with Bot_Svn_Info;
with Utils; use Utils;
with Tics;
with Sim;

with Prices;
with Price_Histories;
with Markets;
with Runners;

procedure Count_Min_Odds is

  package Ev renames Ada.Environment_Variables;
  use type Rpc.Result_Type;
  use type Ada.Containers.Count_Type;

  Me              : constant String := "Greenup_Lay_First_All.";

  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Cmd_Line        : Command_Line_Configuration;

  type Struct_Type is record
    Price : Fixed_Type := 0.0;
    Count : Natural    := 0;
  end record;

  type Struct_Array is array (1..350) of Struct_Type;

  Winners : Struct_Array;
  Losers  : Struct_Array;
  -------------------------------------------------------------

  procedure Run(Price_Data : in Prices.Price_Type;
                Winners    : in out Struct_Array;
                Losers     : in out Struct_Array) is

    Market                 : Markets.Market_Type;
    Eos                    : Boolean := False;
    Price_During_Race_List : Price_Histories.Lists.List;
    Runner                 : Runners.Runner_Type;--table_Arunners.Data_Type;
    Tic_Lay                : Integer := 0;
    Min_Backprice          : Fixed_Type := 10000.0;

  begin
    Log(Me & "Run", "start");

    -- Log(Me & "Run", "Treat market: " &  Price_Data.Marketid);
    Market.Marketid := Price_Data.Marketid;
    Market.Read(Eos);
    if Eos then
      Log(Me & "Run", "no market found");
      return;
    end if;

    -- Log(Me & "Run", "Market: " & Market.To_String);
    Sim.Read_Marketid_Selectionid(Marketid    => Market.Marketid,
                                  Selectionid => Price_Data.Selectionid,
                                  Animal      => Horse,
                                  List        => Price_During_Race_List) ;

    Runner.Marketid := Price_Data.Marketid;
    Runner.Selectionid := Price_Data.Selectionid;
    Runner.Read(Eos);

    if Price_During_Race_List.Length > 80 then
      if Eos then
        Log(Me & "Run", "no runner found found  " & Runner.To_String);
        return;
      end if;

      if Runner.Status(1..7) = "REMOVED" then
        Log(Me & "Run", "runner removed " & Runner.To_String);
        return;
      end if;

      Tic_Lay := Tics.Get_Tic_Index(Price_Data.Layprice);
      -- Log(Me & "Run", "tic_lay " & Tic_Lay'img & " " & Price_Data.To_String);

      -- see if we meet stop_loss or greenup
      --there is no delay here since bet is placed in beginning of race
      for Race_Data of Price_During_Race_List loop
        if Race_Data.Backprice > Fixed_Type(0.0) and then Race_Data.Layprice > Fixed_Type(0.0) and then   -- must be valid
           Race_Data.Backprice < Fixed_Type(1000.0) and then Race_Data.Layprice < Fixed_Type(1000.0) then   -- must be valid
          if Race_Data.Backprice < Min_Backprice then
            Min_Backprice := Race_Data.Backprice;
          end if;
        end if;
      end loop;

      if Runner.Status(1..6) = "WINNER" then
        Winners(Tic_Lay).Count := Winners(Tic_Lay).Count + 1;

      elsif Runner.Status(1..5) = "LOSER" then
        Losers(Tic_Lay).Count := Losers(Tic_Lay).Count + 1;

      end if;

    else
      Log(Me & "not enough data for runner" & Price_During_Race_List.Length'Img, Price_Data.To_String);
    end if;
  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
  ------------------------------ main start -------------------------------------
  Current_Date : Calendar2.Time_Type := Calendar2.Clock;

 -- Sa_Min_Layprice : aliased Gnat.Strings.String_Access;
 -- Sa_Max_Layprice : aliased Gnat.Strings.String_Access;
  Sa_Logfilename  : aliased Gnat.Strings.String_Access;
 -- Ia_Min_Tic      : aliased Integer;
 -- Ia_Max_Tic      : aliased Integer;


  Layprice_High   : Fixed_Type := 0.0;
  Layprice_Low    : Fixed_Type := 0.0;

begin

--    Define_Switch
--      (Cmd_Line,
--       Sa_Min_Layprice'Access,
--       Long_Switch => "--min_lay=",
--       Help        => "Min layprice");
--
--    Define_Switch
--      (Cmd_Line,
--       Sa_Max_Layprice'Access,
--       Long_Switch => "--max_lay=",
--       Help        => "Min layprice");
--
--    Define_Switch
--      (Cmd_Line,
--       Ia_Min_Tic'Access,
--       Long_Switch => "--min_tic=",
--       Help        => "min tic");
--
--    Define_Switch
--      (Cmd_Line,
--       Ia_Max_Tic'Access,
--       Long_Switch => "--max_tic=",
--       Help        => "max tic");

  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Define_Switch
    (Cmd_Line,
     Sa_Par_Inifile'Access,
     Long_Switch => "--inifile=",
     Help        => "use alternative inifile");

  Getopt (Cmd_Line);  -- process the command line


  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","cnt_min_odds");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database_home", "host", ""),
     Port     => Ini.Get_Value("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value("database_home", "name", ""),
     Login    => Ini.Get_Value("database_home", "username", ""),
     Password =>Ini.Get_Value("database_home", "password", ""));
  Log(Me, "db Connected");

  Layprice_High := 100.00; --fixed_Type'Value(Sa_Max_Layprice.all);
  Layprice_Low  :=   1.01; --fixed_Type'Value(Sa_Min_Layprice.all);

  --init arrays
  for i in Winners'Range loop
    Winners(I).Price := Tics.Get_Tic_Price(I);
    Losers(I).Price := Winners(I).Price;
  end loop;

  declare
    Stm         : Sql.Statement_Type;
    T           : Sql.Transaction_Type;
    Price_List  : Prices.Lists.List;
  begin
    T.Start;
    Stm.Prepare(
                "select P.* " &
                  "from APRICES P, AMARKETS M, AEVENTS E " &
                  "where E.EVENTID=M.EVENTID " &
                  "and M.MARKETTYPE = 'WIN' " &
                  "and E.COUNTRYCODE in ('GB','IE') " &
                  "and P.MARKETID = M.MARKETID " &
                  "and E.EVENTTYPEID = 7 " &
                  "and P.LAYPRICE <= :MAX_LAYPRICE " &
                  "order by M.STARTTS, P.MARKETID, P.SELECTIONID ");
    Stm.Set("MAX_LAYPRICE",Layprice_High);
    Prices.Read_List(Stm, Price_List);
    T.Commit;
    Log(Layprice_Low'Img & " " & Layprice_High'Img & " " & Price_List.Length'Img);

    begin
      for Price of Price_List loop -- all runners in race
        if Price.Pricets.Day /= Current_Date.Day then
          Log(Me, "start Treat date: " & Current_Date.String_Date_Iso );
          Current_Date := Price.Pricets;
        end if;

        if Layprice_Low <= Price.Layprice and then Price.Layprice <= Layprice_High then
          T.Start;
          --for Dtg in Delta_Tics_Type'Range loop
     --     for Dtg in Ia_Min_Tic .. Ia_Max_Tic loop
            Run(Price_Data => Price,
                Winners => Winners,
                Losers => Losers);
     --     end loop;
          T.Commit;
        end if;
      end loop;
    end;
  end;

  for i in Winners'Range loop
    Log("winners :" & I'Img & " " & F8_Image(Winners(I).Price) & " " & Winners(I).Count'Img);
  end loop;

  for i in Winners'Range loop
    Log("losers  :" & I'Img & " " & F8_Image(Losers(I).Price) & " " & Losers(I).Count'Img);
  end loop;


  Log(Me, "Close Db");
  Sql.Close_Session;
  Logging.Close;

exception
--    when Lock.Lock_Error =>
--      Log(Me, "lock error, exit");
--      Logging.Close;

  when E: others =>
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
    begin
      Log(Last_Exception_Name);
      Log("Message : " & Last_Exception_Messsage);
      Log(Last_Exception_Info);
      Log("addr2line" & " --functions --basenames --exe=" &
            Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
    end ;
    Log(Me, "Closed log and die");
    Logging.Close;
end Count_Min_Odds;
