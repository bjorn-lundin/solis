with Ada.Exceptions;
with Ada.Command_Line;
--with Ada.Strings; use Ada.Strings;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
--with Ada.Containers.Doubly_Linked_Lists;

--with Bot_System_Number;

with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
--with Calendar2; use Calendar2;
with Bot_Messages;
with Rpc;
with Lock ;
with Posix;
with Ini;
with Logging; use Logging;
--with Process_IO;
--with Core_Messages;
with Table_Amarkets;
with Table_Aevents;
with Table_Aprices;
--with Table_Abalances;
with Table_Apriceshistory;
with Bot_Svn_Info;
--with Config;
--with Utils; use Utils;
with Table_Abets;
with Table_Arunners;

with Sim;
with Simulation_Storage;

procedure Create_Graph_Data_Lost_Bets is

--  Bad_Mode : exception;
  package EV renames Ada.Environment_Variables;
  use type Rpc.Result_Type;

  Me              : constant String := "Poll.";
  My_Lock         : Lock.Lock_Type;

  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Ba_Daemon       : aliased Boolean := False;
  Cmd_Line        : Command_Line_Configuration;
  Mode : Bot_Mode_Type := Simulation; -- may look at $BOT_MODE
--  use type Ada.Containers.Count_Type;
    
  Global_Marketid_Map  : Simulation_Storage.Marketid_Map_Pack.Map;
  Global_Winner_Map    : Simulation_Storage.Winner_Map_Pack.Map;
  Global_Win_Place_Map : Simulation_Storage.Win_Place_Map_Pack.Map;
  
  Global_Market_Notification : Bot_Messages.Market_Notification_Record;
  -------------------------------------------------------------

  procedure Run(Market_Notification : in Bot_Messages.Market_Notification_Record) is
    Market    : Markets.Market_Type;
    Event     : Table_AEvents.Event_Type;
    Price_List  : Table_Aprices.Aprices_List_Pack2.List;
    --------------------------------------------
    
    Eos               : Boolean := False;

    
    Runner_List : Table_Arunners.Arunners_List_Pack2.List;
    Tmp_Runner  : Table_Arunners.Data_Type;
    
    Is_Winner_Win : Boolean := False;
    Is_Winner_Place : Boolean := False;
    Price_Data : Table_Aprices.Data_Type;
    
    Price_During_Race_List : Table_Apriceshistory.Apriceshistory_List_Pack2.List;
    
  begin
    Log(Me & "Run", "Treat market: " &  Market_Notification.Market_Id);
    Market.Marketid := Market_Notification.Market_Id;


    Market.Read(Eos);
    if not Eos then
      if Market.Markettype(1..3) /= "WIN"  then
        Log(Me & "Run", "not a WIN market: " &  Market_Notification.Market_Id);
        return;
      else
        Event.Eventid := Market.Eventid;
        Event.Read(Eos);
        if not Eos then
          if Event.Eventtypeid /= Integer_4(7) then
            Log(Me & "Run", "not a HORSE market: " &  Market_Notification.Market_Id);
            return;
          end if;
        else
          Log(Me & "Run", "no event found");
          return;
        end if;
      end if;
      
      if Market.Numrunners < Integer_4(6) then
        Log(Me & "Run", "less than 6 runners");
        return;
      end if;
      
    else
      Log(Me & "Run", "no market found");
      return;
    end if;
    
    if not Table_Apriceshistory.Is_Existing_I1(Marketid => Market.Marketid) then
      Log(Me & "Run", "no Apriceshistory found");
      return;
    end if;

    Log(Me & "Run", "market found  " & Market.To_String);

    
    Tmp_Runner.Marketid := Market.Marketid;
    Tmp_Runner.Read_I1_Marketid(Runner_List);
    
    
    Sim.Read_Marketid(Marketid => Market_Notification.Market_Id, List => Price_During_Race_List) ;
    
    -- do the poll
    Log(Me & "Make_Bet", "start poll_loop for market '" & Market.To_String & "'");
    
    for Runner of Runner_List loop
      Price_List.Clear;
    
      for Race_Data of Price_During_Race_List loop

        if Runner.Selectionid =  Race_Data.Selectionid then
          Price_Data := (
             Marketid     => Race_Data.Marketid,
             Selectionid  => Race_Data.Selectionid,
             Pricets      => Race_Data.Pricets,
             Status       => Race_Data.Status,
             Totalmatched => Race_Data.Totalmatched,
             Backprice    => Race_Data.Backprice,
             Layprice     => Race_Data.Layprice,
             Ixxlupd      => Race_Data.Ixxlupd,
             Ixxluts      => Race_Data.Ixxluts
          );
          Price_List.Append(Price_Data);
        end if;    
      end loop ;
      
      -- is winner win?
      begin
        Is_Winner_Win := False;
        for WW of Global_Winner_Map(Market_Notification.Market_Id) loop
          if WW.Selectionid = Runner.Selectionid then
            Is_Winner_Win := True;
            exit;
          end if;  
        end loop;  
      end;
     
      -- is winner place?
      declare
        Place_Market_Id : Market_Id_Type := Global_Win_Place_Map(Market_Notification.Market_Id);
      begin
        Is_Winner_Place := False;
        for WP of Global_Winner_Map(Place_Market_Id) loop
          if WP.Selectionid = Runner.Selectionid and then not Is_Winner_Win then
            Is_Winner_Place := True;
            exit;
          end if;  
        end loop;  
      end;
      Sim.Create_Runner_Data(Price_List => Price_List, 
                             Alg        => Sim.None,
                             Is_Winner  => Is_Winner_Win,
                             Is_Place   => Is_Winner_Place ) ;

    end loop ;
    Log(Me & "Make_Bet", "exited poll_loop for market '" & Market.To_String & "'");

  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
------------------------------ main start -------------------------------------

begin

   Define_Switch
    (Cmd_Line,
     Sa_Par_Bot_User'access,
     Long_Switch => "--user=",
     Help        => "user of bot");

   Define_Switch
     (Cmd_Line,
      Ba_Daemon'access,
      Long_Switch => "--daemon",
      Help        => "become daemon at startup");

   Define_Switch
     (Cmd_Line,
      Sa_Par_Inifile'access,
      Long_Switch => "--inifile=",
      Help        => "use alternative inifile");

  Getopt (Cmd_Line);  -- process the command line

  if Ba_Daemon then
    Posix.Daemonize;
  end if;

   --must take lock AFTER becoming a daemon ...
   --The parent pid dies, and would release the lock...
  My_Lock.Take(EV.Value("BOT_NAME"));
  Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
        (Host     => Ini.Get_Value("database", "host", ""),
         Port     => Ini.Get_Value("database", "port", 5432),
         Db_Name  => Ini.Get_Value("database", "name", ""),
         Login    => Ini.Get_Value("database", "username", ""),
         Password =>Ini.Get_Value("database", "password", ""));
  Log(Me, "db Connected");
  Log(Me, "Login betfair");  
  case Mode is 
    when Real =>
      Rpc.Init(
                Username   => Ini.Get_Value("betfair","username",""),
                Password   => Ini.Get_Value("betfair","password",""),
                Product_Id => Ini.Get_Value("betfair","product_id",""),
                Vendor_Id  => Ini.Get_Value("betfair","vendor_id",""),
                App_Key    => Ini.Get_Value("betfair","appkey","")
              );
      Rpc.Login;
    when Simulation =>
      Log(Me, "no Login to Betfair in simulation mode");
  end case;                      
  Log(Me, "Login betfair done");
  
  

  declare
    Stm : Sql.Statement_Type;
    T   : Sql.Transaction_Type;
    Eos : Boolean := False;
    B   : Table_Abets.Data_Type;
  begin
    T.Start;
    Stm.Prepare(
      "select * " &
      "from ABETS " &
      "where STATUS = 'SETTLED' and STARTTS::date = '2014-11-09' " );
    Stm.Open_Cursor;
    loop
      Stm.Fetch(Eos);
      exit when Eos;
       B := Table_Abets.Get(Stm); 
       Sim.Create_Bet_Data(B);
    end loop;
           
    Stm.Close_Cursor;
    T.Commit;
  end;           
  
  
  Log(Me, "Close Db");
  Sql.Close_Session;
  return;
  
  Log(Me, "start read maps");
  Simulation_Storage.Fill_Maps(Marketid_Map  => Global_Marketid_Map,
                               Winner_Map    => Global_Winner_Map,  
                               Win_Place_Map => Global_Win_Place_Map) ;
  Log(Me, "done read maps");

  
  declare
    Stm : Sql.Statement_Type;
    T   : Sql.Transaction_Type;
    Eos : Boolean := False;
    Place_Market,Win_Market : Markets.Market_Type;
  begin
    T.Start;
    Stm.Prepare(
      "select distinct(MARKETID) " &
      "from ABETS " & 
      "where BETNAME = 'HORSES_PLC_BACK_FINISH_1.10_7.0_1' " & 
      "and STARTTS::date = '2014-11-09' ");
--      "and not BETWON" );
    Stm.Open_Cursor;
    loop
      Stm.Fetch(Eos);
      exit when Eos;
      Stm.Get("MARKETID",Place_Market.Marketid); 
      Win_Market := Sim.Get_Win_Market(Place_Market.Marketid)  ;
      Global_Market_Notification.Market_Id := Win_Market.Marketid;
      Log(Me & "Run", "Treat market: " & Global_Market_Notification.Market_Id );
      Run(Global_Market_Notification);
    end loop;
           
    Stm.Close_Cursor;
    T.Commit;
  end;           

  Log(Me, "Close Db");
  Sql.Close_Session;
  case Mode is 
    when Real       => Rpc.Logout;
    when Simulation => null;
  end case;  
  Logging.Close;
  Posix.Do_Exit(0); -- terminate

exception
  when Lock.Lock_Error =>
    Log(Me, "lock error, exit");
    Logging.Close;
    Posix.Do_Exit(0); -- terminate
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
    Posix.Do_Exit(0); -- terminate
end Create_Graph_Data_Lost_Bets;

