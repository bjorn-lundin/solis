with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Environment_Variables;

with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
with Bot_Messages;
with Rpc;
with Lock ;
with Posix;
with Ini;
with Logging; use Logging;
with Process_IO;
with Core_Messages;
with Table_Amarkets;
with Table_Aevents;
with Table_Aprices;
with Table_Apriceshistory;
with Bot_Svn_Info;
with Utils; use Utils;

procedure Long_Poll_Market is
  package EV renames Ada.Environment_Variables;
  use type Rpc.Result_Type;

  Me              : constant String := "Poll_Market.";
  Timeout         : Duration := 1.0; --first poll only 55 later on
  My_Lock         : Lock.Lock_Type;
  Msg             : Process_Io.Message_Type;

  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Ba_Daemon       : aliased Boolean := False;
  Cmd_Line        : Command_Line_Configuration;
  Now             : Calendar2.Time_Type;
  Ok,
  Is_Time_To_Exit : Boolean := False;
  Select_Open_Markets : Sql.Statement_Type;

  -------------------------------------------------------------
  procedure Run(Market_Notification : in Bot_Messages.Market_Notification_Record) is
    Market    : Markets.Market_Type;
    Event     : Table_AEvents.Event_Type;
    Price_List : Table_Aprices.Aprices_List_Pack2.List;
    --------------------------------------------

    Priceshistory_Data : Table_Apriceshistory.Data_Type;
    In_Play           : Boolean := False;

    Eos               : Boolean := False;
    T                 : Sql.Transaction_Type;
  begin
    Log(Me & "Run", "Treat market: " &  Market_Notification.Market_Id);
    Market.Marketid := Market_Notification.Market_Id;

    Market.Read(Eos);
    if not Eos then
      if Market.Markettype(1..3) = "WIN" or
         Market.Markettype(1..5) = "PLACE"  then
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
      else
        Log(Me & "Run", "not a WIN nor PLACE market: " &  Market_Notification.Market_Id);
        return;
      end if;
    else
      Log(Me & "Run", "no market found");
      return;
    end if;

    -- do the poll
    Price_List.Clear;
    Rpc.Get_Market_Prices(Market_Id  => Market_Notification.Market_Id,
                          Market     => Market,
                          Price_List => Price_List,
                          In_Play    => In_Play);

    begin
      T.Start;
      Now := Calendar2.Clock;
      for Price of Price_List loop
        Priceshistory_Data := (
          Marketid     => Price.Marketid,
          Selectionid  => Price.Selectionid,
          Pricets      => Price.Pricets,
          Status       => Price.Status,
          Totalmatched => Price.Totalmatched,
          Backprice    => Price.Backprice,
          Layprice     => Price.Layprice,
          Ixxlupd      => Price.Ixxlupd,
          Ixxluts      => Now
        );
        Priceshistory_Data.Insert(Keep_Timestamp => True);
      end loop;
      T.Commit;
    exception
      when Sql.Duplicate_Index =>
       T.Rollback;
       Log("Duplicate_Index on Priceshistory " & Priceshistory_Data.To_String );
    end;
    Log("stop insert record into Priceshistory");
  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;


  procedure Collect_Data is
    Market_List : Table_Amarkets.Amarkets_List_Pack2.List;
    T : Sql.Transaction_Type;
    Market_Notification : Bot_Messages.Market_Notification_Record;
  begin
    T.Start;
    Select_Open_Markets.Prepare(
      "select * from AMARKETS where STATUS = 'OPEN'");
    Table_Amarkets.Read_List(Select_Open_Markets, Market_List);
    T.Commit;

    for m of Market_List loop
      Market_Notification.Market_Id := m.Marketid;
      Run(Market_Notification);
    end loop;
  end Collect_Data;


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
    -- Ask a pythonscript to login for us, returning a token
  Log(Me, "Login betfair");
  Rpc.Init(
            Username   => Ini.Get_Value("betfair","username",""),
            Password   => Ini.Get_Value("betfair","password",""),
            Product_Id => Ini.Get_Value("betfair","product_id",""),
            Vendor_Id  => Ini.Get_Value("betfair","vendor_id",""),
            App_Key    => Ini.Get_Value("betfair","appkey","")
          );
  Rpc.Login;
  Log(Me, "Login betfair done");


  Main_Loop : loop
--    --notfy markets_fetcher that we are free
--      Data := (Free => 1, Name => This_Process.Name , Node => This_Process.Node);
--      Bot_Messages.Send(Markets_Fetcher, Data);

    begin
      Log(Me, "Start receive");
      Process_Io.Receive(Msg, Timeout);
      Log(Me, "msg : "& Process_Io.Identity(Msg)'Img & " from " & Trim(Process_Io.Sender(Msg).Name));
      if Sql.Transaction_Status /= Sql.None then
        raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
      end if;
      case Process_Io.Identity(Msg) is
        when Core_Messages.Exit_Message                  =>
          exit Main_Loop;
        when Bot_Messages.Market_Notification_Message    =>
          ----notfy markets_fetcher that we are busy
          --Data := (Free => 0, Name => This_Process.Name , Node => This_Process.Node);
          --Bot_Messages.Send(Markets_Fetcher, Data);
          Run(Bot_Messages.Data(Msg));
        when others =>
          Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout =>
        Timeout := 55.0;
        Rpc.Keep_Alive(OK);
        if not OK then
          begin
            Rpc.Login;
          exception
            when Rpc.Login_Failed =>
              Log(Me, "login failed, but will try again");
          end;
        end if;
        Collect_Data;
    end;
    Now := Calendar2.Clock;

    --restart every day
    Is_Time_To_Exit := Now.Hour = 01 and then
                     ( Now.Minute = 00 or Now.Minute = 01) ; -- timeout = 2 min

    exit Main_Loop when Is_Time_To_Exit;

  end loop Main_Loop;

  Log(Me, "Close Db");
  Sql.Close_Session;
  Rpc.Logout;
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
end Long_Poll_Market;

