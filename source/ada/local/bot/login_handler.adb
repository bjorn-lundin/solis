with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Environment_Variables;
--with Ada.Strings ; use Ada.Strings;
--with Ada.Strings.Fixed ; use Ada.Strings.Fixed;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Stacktrace;
with Lock;
--with Text_io;
with Posix;
with Logging; use Logging;
with Process_Io;
with Core_Messages;

with Ini;
with Rpc;
with Calendar2;
with Types; use Types;
with Utils;
with Token;


procedure Login_Handler is
  package EV renames Ada.Environment_Variables;
  Timeout         : Duration := 25.0;
  My_Lock         : Lock.Lock_Type;
  Msg             : Process_Io.Message_Type;
  Me              : constant String := "Main.";
  Ba_Daemon       : aliased Boolean := False;
  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Config          : Command_Line_Configuration;
  OK              : Boolean := False;
  Is_Time_To_Exit : Boolean := False;
  Now             : Calendar2.Time_Type := Calendar2.Clock;


  ------------------------------------------------------

begin
  Define_Switch
     (Config,
      Sa_Par_Bot_User'access,
      Long_Switch => "--user=",
      Help        => "user of bot");

  Define_Switch
     (Config,
      Ba_Daemon'access,
      "-d",
      Long_Switch => "--daemon",
      Help        => "become daemon at startup");
  Getopt (Config);  -- process the command line

  if Ba_Daemon then
    Posix.Daemonize;
  end if;

  Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");
   --must take lock AFTER becoming a daemon ...
   --The parent pid dies, and would release the lock...
  My_Lock.Take(EV.Value("BOT_NAME"));

  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");


  Log(Me, "Login betfair");
  Rpc.Init(
            Username   => Ini.Get_Value("betfair","username",""),
            Password   => Ini.Get_Value("betfair","password",""),
            Product_Id => Ini.Get_Value("betfair","product_id",""),
            Vendor_Id  => Ini.Get_Value("betfair","vendor_id",""),
            App_Key    => Ini.Get_Value("betfair","appkey","")
          );
  begin
    Log(Me, "start logout first");
    begin
      Rpc.Logout;
      Log(Me, "logout ok");
    exception
      when Token.NOT_VALID_TOKEN =>
        Log(Me, "logout failed");
    end;

    Log(Me, "start login");
    Rpc.Login;
    Log(Me, "stop login");
  exception
    when Rpc.Login_Failed =>
      Log(Me, "Not allowed to login yet");
  end;
  Log(Me, "Login betfair done");
  Log(Me, "Start main loop");
  Main_Loop : loop
    begin
      Log(Me, "Start receive");
      Process_Io.Receive(Msg, Timeout);

      Log(Me, "msg : "& Process_Io.Identity(Msg)'Img & " from " & Utils.Trim(Process_Io.Sender(Msg).Name));

      case Process_Io.Identity(Msg) is
        when Core_Messages.Exit_Message                            => exit Main_Loop;
        when others => Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout =>

       begin
          Log(Me, "Timeout start");
          Rpc.Keep_Alive(OK);
          if not OK then
            Rpc.Login;
          end if;
        Log(Me, "Timeout stop");
       exception
         when RPC.Login_Failed =>
           Log(Me, "Not allowed to login yet");
       end;
    end;

    Now := Calendar2.Clock;
    --restart every day
    Is_Time_To_Exit := Now.Hour = 01 and then
                       Now.Minute = 00 and then
                       Now.Second >= 50 ;

    exit Main_Loop when Is_Time_To_Exit;

  end loop Main_Loop;
  Logging.Close;

  Posix.Do_Exit(0); -- terminate

exception
  when Lock.Lock_Error =>
    Log(Me, "Lock_Error - Close log and die");
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

    Log(Me, "Close log and die");
    Logging.Close;
    Posix.Do_Exit(0); -- terminate
end Login_Handler;

