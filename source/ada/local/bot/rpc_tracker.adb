--with Text_Io;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings.Fixed ; use Ada.Strings.Fixed;
with Ada.Strings ; use Ada.Strings;
with Ada.Environment_Variables;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Types; use Types;
with Calendar2; use Calendar2;
with Stacktrace;
with Lock;
with Bot_Messages;
with Posix;
with Logging; use Logging;
with Process_Io;
with Core_Messages;
with Bot_Svn_Info;
with Rpc;
with Ini;

procedure Rpc_Tracker is
  package Ev renames Ada.Environment_Variables;
  Timeout  : Duration := 30.0;
  My_Lock  : Lock.Lock_Type;
  Msg      : Process_Io.Message_Type;
  Me       : constant String := "Main.";
  Now : Calendar2.Time_Type := Calendar2.Time_Type_First;

  Is_Time_To_Exit : Boolean := False;

  Sa_Par_Bot_User                     : aliased Gnat.Strings.String_Access;
  Ba_Daemon                           : aliased Boolean := False;
  Cmd_Line                            : Command_Line_Configuration;
  ------------------------------------------------------


begin
  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Ev.Value("BOT_NAME") & ".log");

  Define_Switch
    (Cmd_Line,
     Sa_Par_Bot_User'Access,
     Long_Switch => "--user=",
     Help        => "user of bot");

  Define_Switch
    (Cmd_Line,
     Ba_Daemon'Access,
     Long_Switch => "--daemon",
     Help        => "become daemon at startup");

  Getopt (Cmd_Line);  -- process the command line

  if Ba_Daemon then
    Posix.Daemonize;
  end if;

  --must take lock AFTER becoming a daemon ...
  --The parent pid dies, and would release the lock...
  My_Lock.Take(Ev.Value("BOT_NAME"));
  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");

  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Log(Me, "Start main loop");

  Main_Loop : loop
    begin
    --  Log(Me, "Start receive");
      Process_Io.Receive(Msg, Timeout);
    --  Log(Me, "msg : "& Process_Io.Identity(Msg)'Img & " from " & Utils.Trim(Process_Io.Sender(Msg).Name));
      case Process_Io.Identity(Msg) is
        when Core_Messages.Exit_Message                  =>
          exit Main_Loop;
          -- when Core_Messages.Enter_Console_Mode_Message    => Enter_Console;
        when Bot_Messages.Rpc_Called_Message =>
          declare
            Rcm : Bot_Messages.Rpc_Called_Record := Bot_Messages.Data(Msg);
          begin   -- or do dbcall?
           Log(Me, Rcm.Name & "|" & Rcm.Typ & "|" & Trim(Rcm.Data,right) & "|DATAPOINT" );
          end;
        when others =>
          Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout => null;
        --Log(Me, "Timeout");
    end;
    Now := Calendar2.Clock;

    --restart every day
    Is_Time_To_Exit := Now.Hour = 01 and then
      ( Now.Minute = 00 or Now.Minute = 01) ; -- timeout = 2 min

    exit Main_Loop when Is_Time_To_Exit;

  end loop Main_Loop;

  Log (Me, "db closed, Is_Time_To_Exit " & Is_Time_To_Exit'Img);
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
end Rpc_Tracker;
