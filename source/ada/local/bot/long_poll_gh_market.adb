
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

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
--with Table_Apriceshistory;
with Table_Arunners;
with Table_Abets;
with Bot_Svn_Info;
with Utils; use Utils;
with Sim;

procedure Long_Poll_GH_Market is
  package EV renames Ada.Environment_Variables;
  use type Rpc.Result_Type;

  Me              : constant String := "Poll_Market.";
  Timeout         : Duration := 1.0; --first poll only, 30 later on
  My_Lock         : Lock.Lock_Type;
  Msg             : Process_Io.Message_Type;

  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Ba_Daemon       : aliased Boolean := False;
  Cmd_Line        : Command_Line_Configuration;
  Now             : Calendar2.Time_Type;
  Ok,
  Is_Time_To_Exit : Boolean := False;
  --Select_Open_Markets : Sql.Statement_Type;
  Data : Bot_Messages.Poll_State_Record ;
  Process : Process_Io.Process_Type     := Process_Io.This_Process;
  Markets_Fetcher : Process_Io.Process_Type := (("gh_mark_fetcher"),(others => ' '));
  type Best_Runners_Array_Type is array (1..6) of Table_Aprices.Data_Type ;
  Update_Betwon_To_Null : Sql.Statement_Type;

  -------------------------------------------------------------
  procedure Run(Market_Notification : in Bot_Messages.Market_Notification_Record) is
    Market    : Markets.Market_Type;
    Event     : Table_AEvents.Event_Type;
    Price_List : Table_Aprices.Aprices_List_Pack2.List;
    --------------------------------------------
    function "<" (Left,Right : Table_Aprices.Data_Type) return Boolean is
    begin
      return Left.Backprice < Right.Backprice;
    end "<";
    --------------------------------------------
    Price             : Table_Aprices.Data_Type;
    package Backprice_Sorter is new  Table_Aprices.Aprices_List_Pack2.Generic_Sorting("<");
    Best_Runners      : Best_Runners_Array_Type := (others => Table_Aprices.Empty_Data);
    In_Play           : Boolean := False;
    Eos               : Boolean := False;
    T                 : Sql.Transaction_Type;
    ----------------------------------------------------------------
    procedure Lay_The_Bet(Name          : in    String ;
                          BR            : in    Best_Runners_Array_Type;
                          Max_Lay_Price : in    Fixed_Type) is
      Runner_Data : Table_Arunners.Data_Type;
      Eos : Boolean := False;
      Lay_Bet_Name : Bot_Types.Bet_Name_Type := (others => ' ');
      Bet : Table_Abets.Data_Type;
    begin
     if Fixed_Type(1.01) <= BR(1).Layprice and then BR(1).Layprice <= Max_Lay_Price then
        Runner_Data.Marketid := BR(1).Marketid;
        Runner_Data.Selectionid := BR(1).Selectionid;
        Runner_Data.Read(Eos);
        if not Eos then
          Move (Name,Lay_Bet_Name);
          Sim.Place_Bet(Bet_Name         => Lay_Bet_Name,
                        Market_Id        => Market.Marketid,
                        Side             => Lay,
                        Runner_Name      => Runner_Data.Runnernamestripped,
                        Selection_Id     => BR(1).Selectionid,
                        Size             => Bet_Size_Type(40.0),
                        Price            => Bet_Price_Type(BR(1).Layprice),
                        Bet_Persistence  => Persist,
                        Bet_Placed       => BR(1).Pricets,
                        Bet              => Bet ) ;
          Update_Betwon_To_Null.Prepare("update ABETS set BETWON = null where BETID = :BETID");
          Log("inserting " &  Bet.To_String);
          Bet.Insert;
          Update_Betwon_To_Null.Set("BETID", Bet.Betid);
          Update_Betwon_To_Null.Execute;
        end if;
      end if;
    end Lay_The_Bet;
    --------------------------------------------
    procedure Lay_The_Bet_Between(Name          : in    String ;
                          BR            : in    Best_Runners_Array_Type;
                          Min_Lay_Price : in    Fixed_Type;
                          Max_Lay_Price : in    Fixed_Type) is
      Runner_Data : Table_Arunners.Data_Type;
      Eos : Boolean := False;
      Lay_Bet_Name : Bot_Types.Bet_Name_Type := (others => ' ');
      Bet : Table_Abets.Data_Type;
    begin
     if Min_Lay_Price <= BR(1).Layprice and then BR(1).Layprice <= Max_Lay_Price then
        Runner_Data.Marketid := BR(1).Marketid;
        Runner_Data.Selectionid := BR(1).Selectionid;
        Runner_Data.Read(Eos);
        if not Eos then
          Move (Name,Lay_Bet_Name);
          Sim.Place_Bet(Bet_Name         => Lay_Bet_Name,
                        Market_Id        => Market.Marketid,
                        Side             => Lay,
                        Runner_Name      => Runner_Data.Runnernamestripped,
                        Selection_Id     => BR(1).Selectionid,
                        Size             => Bet_Size_Type(40.0),
                        Price            => Bet_Price_Type(BR(1).Layprice),
                        Bet_Persistence  => Persist,
                        Bet_Placed       => BR(1).Pricets,
                        Bet              => Bet ) ;
          Update_Betwon_To_Null.Prepare("update ABETS set BETWON = null where BETID = :BETID");
          Log("inserting " &  Bet.To_String);
          Bet.Insert;
          Update_Betwon_To_Null.Set("BETID", Bet.Betid);
          Update_Betwon_To_Null.Execute;
        end if;
      end if;
    end Lay_The_Bet_Between;
    --------------------------------------------

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
          if Event.Eventtypeid /= Integer_4(4339) then
            Log(Me & "Run", "not a GREYHOUND market: " &  Market_Notification.Market_Id);
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
      -- ok find the runner with lowest backprice:
      Backprice_Sorter.Sort(Price_List);

      Price.Backprice := 10_000.0;
      Best_Runners := (others => Price);

      declare
        Idx : Integer := 0;
      begin
        for Tmp of Price_List loop
          if Tmp.Status(1..6) = "ACTIVE" then
            Idx := Idx +1;
            exit when Idx > Best_Runners'Last;
            Best_Runners(Idx) := Tmp;
          end if;
        end loop;
      end ;

      for i in Best_Runners'range loop
        Log("Best_Runners(i)" & i'Img & " " & Best_Runners(i).To_String);
      end loop;

      Lay_The_Bet(Name          => "LAY_FAVORITE_1_01_2_90",
                  BR            => Best_Runners,
                  Max_Lay_Price => 2.9);

      Lay_The_Bet(Name          => "LAY_FAVORITE_1_01_3_50",
                  BR            => Best_Runners,
                  Max_Lay_Price => 3.5);

      Lay_The_Bet(Name          => "LAY_FAVORITE_1_01_6_00",
                  BR            => Best_Runners,
                  Max_Lay_Price => 6.0);

      Lay_The_Bet(Name          => "LAY_FAVORITE_1_01_10_00",
                  BR            => Best_Runners,
                  Max_Lay_Price => 10.0);

      Lay_The_Bet(Name          => "LAY_FAVORITE_1_01_15_00",
                  BR            => Best_Runners,
                  Max_Lay_Price => 15.0);

      Lay_The_Bet(Name          => "LAY_FAVORITE_1_01_20_00",
                  BR            => Best_Runners,
                  Max_Lay_Price => 20.0);

      Lay_The_Bet_Between(Name   => "LAY_BETWEEN_6_00_8_00",
                  BR             => Best_Runners,
                  Min_Lay_Price  =>  6.0,
                  Max_Lay_Price  =>  8.0);

      Lay_The_Bet_Between(Name   => "LAY_BETWEEN_6_00_10_00",
                  BR             => Best_Runners,
                  Min_Lay_Price  =>  6.0,
                  Max_Lay_Price  => 10.0);

      Lay_The_Bet_Between(Name   => "LAY_BETWEEN_6_00_12_00",
                  BR             => Best_Runners,
                  Min_Lay_Price  =>  6.0,
                  Max_Lay_Price  => 12.0);

      Lay_The_Bet_Between(Name   => "LAY_BETWEEN_8_00_10_00",
                  BR             => Best_Runners,
                  Min_Lay_Price  =>  8.0,
                  Max_Lay_Price  => 10.0);

      Lay_The_Bet_Between(Name   => "LAY_BETWEEN_8_00_12_00",
                  BR             => Best_Runners,
                  Min_Lay_Price  =>  8.0,
                  Max_Lay_Price  => 12.0);

      Lay_The_Bet_Between(Name   => "LAY_BETWEEN_10_00_12_00",
                  BR             => Best_Runners,
                  Min_Lay_Price  => 10.0,
                  Max_Lay_Price  => 12.0);

      T.Commit;
    end;
  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;


  --procedure Collect_Data is
  --  Market_List : Table_Amarkets.Amarkets_List_Pack2.List;
  --  T : Sql.Transaction_Type;
  --  Market_Notification : Bot_Messages.Market_Notification_Record;
  --begin
  --  T.Start;
  --  Select_Open_Markets.Prepare(
  --    "select * from AMARKETS where STATUS = 'OPEN'");
  --  Table_Amarkets.Read_List(Select_Open_Markets, Market_List);
  --  T.Commit;
  --
  --  for m of Market_List loop
  --    Market_Notification.Market_Id := m.Marketid;
  --    Run(Market_Notification);
  --  end loop;
  --end Collect_Data;


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
    --notfy markets_fetcher that we are free
      Data := (Free => 1, Name => Process.Name , Node => Process.Node);
      Bot_Messages.Send(Markets_Fetcher, Data);

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
          --notfy markets_fetcher that we are busy
          Data := (Free => 0, Name => Process.Name , Node => Process.Node);
          Bot_Messages.Send(Markets_Fetcher, Data);
          Run(Bot_Messages.Data(Msg));
          --null;
        when others =>
          Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout =>
        Timeout := 30.0;
        Rpc.Keep_Alive(OK);
        if not OK then
          begin
            Rpc.Login;
          exception
            when Rpc.Login_Failed =>
              Log(Me, "login failed, but will try again");
          end;
        end if;
       -- Collect_Data;
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
end Long_Poll_GH_Market;

