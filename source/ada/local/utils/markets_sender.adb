with Stacktrace;
with Sql;
with Calendar2;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Types ; use Types;
with Lock ;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Posix;
with Markets;
with Events;
with Table_Arunners;
with Ini;
with Logging; use Logging;
with Utils; use Utils;

with Ada.Environment_Variables;
with Process_IO;
with Bot_Messages;

procedure Markets_Sender is
  package EV renames Ada.Environment_Variables;

  Me : constant String := "Main.";
  Ba_Daemon       : aliased Boolean := False;
  Ba_Horse        : aliased Boolean := False;
  Ba_Hound        : aliased Boolean := False;
  Ba_Football     : aliased Boolean := False;
  Ba_Log          : aliased Boolean := False;
  Sa_Par_Marketid : aliased Gnat.Strings.String_Access;
  Sa_Par_Countrycodes  : aliased Gnat.Strings.String_Access;
  
  Config : Command_Line_Configuration;
  My_Lock  : Lock.Lock_Type;
  T : Sql.Transaction_Type;
  The_Markets : Sql.Statement_Type;
  Do_Send : Boolean := True;
------------------------------ main start -------------------------------------
  Amarkets_List : Markets.Lists.List;
  Amarket :  Markets.Market_Type;
  Aevent :  Events.Event_Type;
  Arunner : Table_Arunners.Data_Type;
  MNR      : Bot_Messages.Market_Notification_Record;
  Receiver : Process_IO.Process_Type := ((others => ' '), (others => ' '));
  Tot,Cur : Natural := 0;
  type Eos_Type is (Event,Winner);
  Eos : array (Eos_Type'range) of Boolean := (others => False);
  Ts : Calendar2.Time_Type;
begin
  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");
  Define_Switch
     (Config,
      Ba_Log'access,
      Long_Switch => "--log",
      Help        => "open logfile ");

  Define_Switch
     (Config,
      Sa_Par_Marketid'access,
      Long_Switch => "--marketid=",
      Help        => "read markets with MARKETID > marketid ");
      
  Define_Switch
     (Config,
      Ba_Daemon'access,
      Long_Switch => "--daemon",
      Help        => "become daemon at startup");
      
  Define_Switch
     (Config,
      Sa_Par_Countrycodes'access,
      Long_Switch => "--countrycodes=",
      Help        => "countrycodes commaseparated. Applies only to horses");

  Define_Switch
     (Config,
      Ba_Horse'access,
      Long_Switch => "--horses",
      Help        => "send horse markets");

  Define_Switch
     (Config,
      Ba_Hound'access,
      Long_Switch => "--hounds",
      Help        => "send hound markets");
      
  Define_Switch
     (Config,
      Ba_Football'access,
      Long_Switch => "--football",
      Help        => "send football markets");
      
  Getopt (Config);  -- process the command line

  if Ba_Log then
    Logging.Open(EV.Value("BOT_HOME") & "/log/markets_sender.log");
  end if;

  if Ba_Daemon then
     Posix.Daemonize;
  end if;

   --must take lock AFTER becoming a daemon ...
   --The parent pid dies, and would release the lock...
  My_Lock.Take("markets_sender");

  Sql.Connect
        (Host     => Ini.Get_Value("database","host",""),
         Port     => Ini.Get_Value("database","port",5432),
         Db_Name  => Ini.Get_Value("database","name",""),
         Login    => Ini.Get_Value("database","username",""),
         Password => Ini.Get_Value("database","password",""));

  T.Start;
  
--  if Sa_Par_Marketid.all /= "" then
--    Markets.Prepare("select * from AMARKETS where MARKETID >= :MARKETID order by STARTTS");
--    Markets.Set("MARKETID", Sa_Par_Marketid.all);
--  else
    The_Markets.Prepare("select * from AMARKETS where STARTTS > :TS order by STARTTS ");
--    Ts := (2013,12,30,0,0,0,0);
    Ts := (2014,07,09,0,0,0,0);
    The_Markets.Set_Timestamp("TS", Ts);
    
--  end if;
  Markets.Read_List(Stm => The_Markets, List  => Amarkets_List);
--  Table_Amarkets.Read_All(List  => Amarkets_List, Order=> True);
  T.Commit;
  Tot := Integer(Amarkets_List.Length);
  Log(Me, "found # markets:" & Tot'Img );
--  while not Table_Amarkets.Amarkets_List_Pack.Is_Empty(Amarkets_List) loop
--     Table_Amarkets.Amarkets_List_Pack.Remove_From_Head(Amarkets_List, Amarket);
  for m of Amarkets_List loop
     Amarket := m;
     Cur := Cur +1;
     Arunner.Marketid := Amarket.Marketid;
     Move("WINNER",Arunner.Status);
     Table_Arunners.Read_One_Marketid_status(Arunner,
                                             Order      => False,
                                             End_Of_Set => Eos(Winner));
     
     if not Eos(Winner) then -- need to have a winner
       MNR.Market_Id := (others => ' ');
       Receiver.Name := (others => ' ');
       Move(Amarket.Marketid, MNR.Market_Id);
       Move("bot", Receiver.Name);
       
       Aevent.Eventid := Amarket.Eventid;
       Events.Read(Aevent,Eos(Event));
       Do_Send := False;
       if not Eos(Event) then
        if Aevent.Eventtypeid = 7 and then Ba_Horse then       -- horse
        
          if Trim(Amarket.Markettype) = "PLACE" then
            if    Aevent.Countrycode = "US" and then Position(Sa_Par_Countrycodes.all, "US") > 0 then
              Move("horses_plc_us", Receiver.Name);
              Do_Send := False;
            elsif Aevent.Countrycode = "GB" and then Position(Sa_Par_Countrycodes.all, "GB") > 0 then
              Move("horses_plc_gb", Receiver.Name);
              Do_Send := False;
            elsif Aevent.Countrycode = "IE" and then Position(Sa_Par_Countrycodes.all, "IE") > 0 then
              Move("horses_plc_ie", Receiver.Name);
              Do_Send := False;
            elsif Aevent.Countrycode = "ZA" and then Position(Sa_Par_Countrycodes.all, "ZA") > 0 then
              Move("horses_plc_za", Receiver.Name);
              Do_Send := False;
            elsif Aevent.Countrycode = "SG" and then Position(Sa_Par_Countrycodes.all, "SG") > 0 then
              Move("horses_plc_sg", Receiver.Name);
              Do_Send := False;
            elsif Aevent.Countrycode = "FR" and then Position(Sa_Par_Countrycodes.all, "FR") > 0 then
              Move("horses_plc_fr", Receiver.Name);
              Do_Send := False;
            end if;            

          elsif Trim(Amarket.Markettype) = "WIN" then     

            if    Aevent.Countrycode = "US" and then Position(Sa_Par_Countrycodes.all, "US") > 0 then
              Move("horses_win_us", Receiver.Name);
              Do_Send := False;
            elsif Aevent.Countrycode = "GB" and then Position(Sa_Par_Countrycodes.all, "GB") > 0 then
              Move("lay_in_play_gb", Receiver.Name);
              Do_Send := True;
            elsif Aevent.Countrycode = "IE" and then Position(Sa_Par_Countrycodes.all, "IE") > 0 then
              Move("lay_in_play_ie", Receiver.Name);
              Do_Send := True;
            elsif Aevent.Countrycode = "ZA" and then Position(Sa_Par_Countrycodes.all, "ZA") > 0 then
              Move("horses_win_za", Receiver.Name);
              Do_Send := False;
            elsif Aevent.Countrycode = "SG" and then Position(Sa_Par_Countrycodes.all, "SG") > 0 then
              Move("horses_win_sg", Receiver.Name);
              Do_Send := False;
            elsif Aevent.Countrycode = "FR" and then Position(Sa_Par_Countrycodes.all, "FR") > 0 then
              Move("horses_win_fr", Receiver.Name);
              Do_Send := False;
            end if;                 
          end if;
        
        elsif Aevent.Eventtypeid = 4339 and then Ba_Hound then -- hound
          if Trim(Amarket.Markettype) = "PLACE" then
            if    Aevent.Countrycode = "GB" then
              Move("hounds_plc_gb", Receiver.Name);
              Do_Send := True;
            else
              Move("hounds_plc_xx", Receiver.Name);
              Do_Send := False;
            end if;              
            
          elsif Trim(Amarket.Markettype) = "WIN" then     
            if    Aevent.Countrycode = "GB" then
              Move("hounds_win_gb", Receiver.Name);
              Do_Send := True;
            else
              Move("hounds_win_xx", Receiver.Name);
              Do_Send := False;
            end if;              
          end if;  
        elsif Aevent.Eventtypeid = 1 and then Ba_Football then -- football

          if Trim(Amarket.Markettype) = "MATCH_ODDS" then
              Move("football", Receiver.Name);
              Do_Send := True;
          elsif Trim(Amarket.Markettype) = "CORRECT_SCORE" then
              Move("football", Receiver.Name);
              Do_Send := True;
          elsif Trim(Amarket.Markettype) = "HALF_TIME_SCORE" then
              Move("football_2", Receiver.Name);
              Do_Send := True;
          end if;        
          
        end if;
        if Do_Send then       
          Log(Me, "Notifying " & Trim(Receiver.Name) & " with marketid: '" & MNR.Market_Id   & "' Startts = " &
                  Calendar2.String_Date_And_Time(Amarket.Startts, Milliseconds => true)
                  & Cur'Img & "/" & Tot'Img);
          Bot_Messages.Send(Receiver, MNR);
        end if;  
      end if;        
    end if;
  end loop;

  Log(Me, "shutting down, close db");
  Sql.Close_Session;
  Log(Me, "do_exit");
  Posix.Do_Exit(0); -- terminate
  Log(Me, "after do_exit");

exception
  when Lock.Lock_Error =>
      Posix.Do_Exit(0); -- terminate

  when E: others =>
    Stacktrace.Tracebackinfo(E);
    Posix.Do_Exit(0); -- terminate
end Markets_Sender;
