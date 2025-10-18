with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Strings ; use Ada.Strings;
with Ada.Strings.Fixed ; use Ada.Strings.Fixed;
with Gnatcoll.Json;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Bets;
with Bot_System_Number;
with Stacktrace;
with Lock;
--with Text_io;
with Sql;
with Bot_Messages;
with Posix;
with Logging; use Logging;
with Process_Io;
with Core_Messages;

with Ini;
with Rpc;
with Calendar2;
with Types; use Types;
with Utils;

with Markets;
with Table_Arunners;


procedure Bet_Checker is
  package Ev renames Ada.Environment_Variables;
  Timeout         : Duration := 25.0;
  My_Lock         : Lock.Lock_Type;
  Msg             : Process_Io.Message_Type;
  Me              : constant String := "Main.";
  Ba_Daemon       : aliased Boolean := False;
  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Config          : Command_Line_Configuration;
  Ok              : Boolean := False;
  Is_Time_To_Exit : Boolean := False;
  Now             : Calendar2.Time_Type := Calendar2.Clock;

  --------------------------------------------
  procedure Treat_Pending_Bets_In_Json_File is
    Service     : String := "Treat_Pending_Bets_In_Json_File";
    use Gnatcoll.Json;
    use Ada.Directories;
    Dir         : String := Ev.Value("BOT_HOME") & "/pending";
    Dir_Ent     : Directory_Entry_Type;
    The_Search  : Search_Type with Warnings => Off;
    Json_Data   : Json_Value;
    T           : Sql.Transaction_Type;
  begin

    if not Exists(Dir) then
      Log(Me & Service , "creating directory:" & Dir);
      Create_Directory(Dir);
    end if;

    Start_Search(Search    => The_Search,
                 Directory => Dir,
                 Pattern   => "*.json");

    loop
      exit when not More_Entries(Search => The_Search);
      Log("----------------------");
      Get_Next_Entry(Search          => The_Search,
                     Directory_Entry => Dir_Ent);
      declare
        Filename : String := Full_Name(Dir_Ent);
        Content  : String := Lock.Read_File(Filename);
        Bet      : Bets.Bet_Type;
        Market   : Markets.Market_Type;
        Runner   : Table_Arunners.Data_Type;
        type Eos_Type is (Amarkets , Arunners, Abets);
        Eos      : array (Eos_Type'Range) of Boolean := (others => False);
      begin
        Log(Filename & " has content length" & Content'Length'Img);
        if Content'Length > 0 then
          Json_Data := Read(Content,"");

          Bet := Bets.From_Json(Json_Data);

          if Bet.Betid = 0 then
            Log(Me & Service, "bad bet, get fake betid");
            Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
          end if;

          begin
            T.Start;
            Market.Marketid := Bet.Marketid;
            Market.Read(Eos(Amarkets) );

            Runner.Marketid := Bet.Marketid;
            Runner.Selectionid := Bet.Selectionid;
            Runner.Read(Eos(Arunners) );

            Bet.Startts       := Market.Startts;
            Bet.Fullmarketname:= Market.Marketname;
            Bet.Runnername    := Runner.Runnername;

            Bet.Insert_And_Nullify_Betwon;
            Log(Me & "Place_Bet", Utils.Trim(Bet.Betname) & " inserted bet: " & Bet.To_String);
            T.Commit;
          exception
            when Sql.Duplicate_Index =>
              T.Rollback;
              Log(Me & Service, "duplicate index " & Bet.To_String);
          end ;

          if Bet.Powerdays > 0 then
            -- bet must be at least partially matched immediately or we try to cancel it
            if Integer(Bet.Sizematched) = 0 then
              Log(Me & Service, "try to cancel bet, since Powerdays > 0 and sizematched = 0");
              declare
                Cancel_Succeeded : Boolean := False;
              begin
                Cancel_Succeeded := Rpc.Cancel_Bet(Bet => Bet);
                Log(Me & Service, "Cancel bet" & Bet.Betid'Img & " succeeded: " & Cancel_Succeeded'Img);
                if Cancel_Succeeded then
                  loop
                    begin
                      T.Start;
                      Bet.Read(Eos(Abets));
                      if not Eos(Abets) then
                        Move("CANCELLED", Bet.Status);
                        Bet.Update_Withcheck;
                      end if;
                      T.Commit;
                      exit;
                    exception
                      when Sql.No_Such_Row =>
                        T.Rollback;
                        Log(Me & Service, "Cancel bet" & Bet.Betid'Img & " Trf_conflict : " & Bet.To_String);
                    end;
                  end loop;
                end if;
              end;
            end if;
          end if;
          Log(Me & Service, "delete file " & Filename);
          Delete_File(Filename);
        else
          Log(Me & Service, Filename & " was locked or empty. Retry next time");
        end if;
      end;
    end loop;
    End_Search (Search => The_Search);
  end Treat_Pending_Bets_In_Json_File ;

  ------------------------------------------------------

begin
  Define_Switch
    (Config,
     Sa_Par_Bot_User'Access,
     Long_Switch => "--user=",
     Help        => "user of bot");

  Define_Switch
    (Config,
     Ba_Daemon'Access,
     "-d",
     Long_Switch => "--daemon",
     Help        => "become daemon at startup");
  Getopt (Config);  -- process the command line

  if Ba_Daemon then
    Posix.Daemonize;
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Ev.Value("BOT_NAME") & ".log");
  --must take lock AFTER becoming a daemon ...
  --The parent pid dies, and would release the lock...
  My_Lock.Take(Ev.Value("BOT_NAME"));

  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");

  Log(Me, "Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database","host",""),
     Port     => Ini.Get_Value("database","port", 5432),
     Db_Name  => Ini.Get_Value("database","name",""),
     Login    => Ini.Get_Value("database","username",""),
     Password => Ini.Get_Value("database","password",""));
  Log(Me, "db Connected");

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

  Bets.Check_Bets;
  Log(Me, "Start main loop");
  Main_Loop : loop
    begin
      Log(Me, "Start receive");
      Process_Io.Receive(Msg, Timeout);

      Log(Me, "msg : "& Process_Io.Identity(Msg)'Img & " from " & Utils.Trim(Process_Io.Sender(Msg).Name));

      case Process_Io.Identity(Msg) is
        when Core_Messages.Exit_Message                            => exit Main_Loop;
        when Bot_Messages.New_Bet_Placed_Notification_Message =>
          Treat_Pending_Bets_In_Json_File;
        when Bot_Messages.New_Winners_Arrived_Notification_Message =>
          Treat_Pending_Bets_In_Json_File;
          Bets.Check_If_Bet_Accepted;
          Bets.Check_Bets;
        when others => Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout =>
        Log(Me, "Timeout start");
        Rpc.Keep_Alive(Ok);
        if not Ok then
          begin
            Rpc.Login;
          exception
            when Rpc.Login_Failed =>
              Log(Me, "login failed, but will try again");
          end;
        end if;
        Treat_Pending_Bets_In_Json_File;
        Bets.Check_If_Bet_Accepted;
        Bets.Check_Bets;
        Log(Me, "Timeout stop");
    end;

    Now := Calendar2.Clock;
    --restart every day
    Is_Time_To_Exit := Now.Hour = 01 and then
      Now.Minute = 00 and then
      Now.Second >= 50 ;

    exit Main_Loop when Is_Time_To_Exit;

  end loop Main_Loop;
  Log(Me, "Close Db");
  Sql.Close_Session;
  Log(Me, "Db Closed");
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
end Bet_Checker;
