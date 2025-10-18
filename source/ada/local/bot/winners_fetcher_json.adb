with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Types; use Types;
with Sql;
with Stacktrace;
with Lock ;
with Rpc;
with Posix;
with Logging; use Logging;
with Bot_Messages;
with Process_Io;
with Ini;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Core_Messages;
with Markets;
with Calendar2;

procedure Winners_Fetcher_Json is
  package EV renames Ada.Environment_Variables;
  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Cmd_Line : Command_Line_Configuration;

  Me : constant String := "Main.";
  --------------------------
  OK : Boolean := False;
  Has_Inserted_Winner : Boolean := False;
  My_Lock  : Lock.Lock_Type;
  Msg      : Process_Io.Message_Type;
  Long_Timeout  : Duration := 47.0;
  Timeout  : Duration := 1.0;
  Ba_Daemon : aliased Boolean := False;
  Now : Calendar2.Time_Type := Calendar2.Time_Type_First;
  ----------------------------------------------
  use type Sql.Transaction_Status_Type;
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
    Getopt (Cmd_Line);  -- process the command line

    Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");
    Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

    if Ba_Daemon then
      Posix.Daemonize;
    end if;
    My_Lock.Take(EV.Value("BOT_NAME"));

    Log(Me, "Login betfair");
    Rpc.Init(
            Username   => Ini.Get_Value("betfair","username",""),
            Password   => Ini.Get_Value("betfair","password",""),
            Product_Id => Ini.Get_Value("betfair","product_id",""),
            Vendor_Id  => Ini.Get_Value("betfair","vendor_id",""),
            App_Key    => Ini.Get_Value("betfair","appkey","")
    );
    Rpc.Login;

    Log (Me, "connect db");
    Sql.Connect
        (Host     => Ini.Get_Value("database","host",""),
         Port     => Ini.Get_Value("database","port",5432),
         Db_Name  => Ini.Get_Value("database","name",""),
         Login    => Ini.Get_Value("database","username",""),
         Password => Ini.Get_Value("database","password",""));
    Log (Me, "connected to db");

    Main_Loop : loop
      begin
        Log(Me, "Start receive");
        Process_Io.Receive(Msg, Timeout);
        if Sql.Transaction_Status /= Sql.None then
          raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
        end if;
        case Process_Io.Identity(Msg) is
          when Core_Messages.Exit_Message                  =>
            exit Main_Loop;
          when others =>
            Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);
        end case;
      exception
        when Process_Io.Timeout =>
          if Sql.Transaction_Status /= Sql.None then
            raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
          end if;
          Timeout := Long_Timeout; -- only first time fast ...
          Log(Me, "Timeout");
          Rpc.Keep_Alive(OK);
        if not Ok then
          begin
            Rpc.Login;
          exception
            when Rpc.Login_Failed =>
              Log(Me, "login failed, but will try again");
          end;
        end if;
        Markets.Check_Market_Status;                          -- updates markets status
        Markets.Check_Unsettled_Markets(Has_Inserted_Winner); -- updates runner status

        if Has_Inserted_Winner then
          declare
            Nwanr    : Bot_Messages.New_Winners_Arrived_Notification_Record;
            Receiver : Process_Io.Process_Type := ((others => ' '), (others => ' '));
          begin
            Move("bet_checker", Receiver.Name);
            Log(Me, "Notifying 'bet_checker' of that new winners are arrived");
            Bot_Messages.Send(Receiver, Nwanr);
          end;
        end if;
      end;
      Now := Calendar2.Clock;
      --restart every day
      exit Main_Loop when Now.Hour = 01 and then Now.Minute <= 02;

    end loop Main_Loop;

    Sql.Close_Session;
    Rpc.Logout;
    Logging.Close;
    Posix.Do_Exit(0); -- terminate
exception
  when Lock.Lock_Error =>
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
    Logging.Close;
    Posix.Do_Exit(0); -- terminate
end Winners_Fetcher_Json;
