with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers.Doubly_Linked_Lists;

with Bot_System_Number;

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
with Table_Abalances;
with Table_Apricesfinish;
with Bot_Svn_Info;
with Config;
with Utils; use Utils;
with Table_Abets;
with Table_Arunners;

with Sim;


procedure Lay_In_Play_Greenup is

  Bad_Mode : exception;
  package EV renames Ada.Environment_Variables;
  use type Rpc.Result_Type;

  Me              : constant String := "Poll.";
  Timeout         : Duration := 120.0;
  My_Lock         : Lock.Lock_Type;
  Msg             : Process_Io.Message_Type;

  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Ba_Daemon       : aliased Boolean := False;
  Cmd_Line        : Command_Line_Configuration;
  Now             : Calendar2.Time_Type;
  Ok,
  Is_Time_To_Exit : Boolean := False;
  Cfg : Config.Config_Type;
  Mode : Bot_Mode_Type := Simulation; -- may look at $BOT_MODE
  use type Ada.Containers.Count_Type;
    
  -------------------------------------------------------------

  procedure Run(Market_Notification : in Bot_Messages.Market_Notification_Record) is
    Market    : Markets.Market_Type;
    Event     : Table_AEvents.Event_Type;
    Price_List,
    Tmp_Price_List   : Table_Aprices.Aprices_List_Pack2.List;
    --------------------------------------------
    function "<" (Left,Right : Table_Aprices.Data_Type) return Boolean is
    begin
      -- sort in reversed order , highest odds first
      return Left.Backprice > Right.Backprice;
    end "<";
    --------------------------------------------
    package Backprice_Sorter is new  Table_Aprices.Aprices_List_Pack2.Generic_Sorting("<");
    
    Has_Been_In_Play,
    In_Play           : Boolean := False;
    Eos               : Boolean := False;
    T                 : Sql.Transaction_Type;
    Current_Turn_Not_Started_Race : Integer_4 := 0;
    Betfair_Result    : Rpc.Result_Type := Rpc.Result_Type'first;
    Saldo             : Table_Abalances.Data_Type;
    
    type Bet_Type is record
      Lay_Bet  : Table_Abets.Data_Type := Table_Abets.Empty_Data;
      Back_Bet : Table_Abets.Data_Type := Table_Abets.Empty_Data;
    end record;  
    use type Table_Abets.Data_Type;
    package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_Type);
    Bet_List : Bet_List_Pack.List;
    Bet : Bet_Type;
    
    Lay_Bet_Name  : constant Bet_Name_Type := "HORSES_WIN_50@30_GREENUP_LAY                                                                        ";
    Back_Bet_Name : constant Bet_Name_Type := "HORSES_WIN_35@40_GREENUP_BACK                                                                       ";
    
    Runner_List : Table_Arunners.Arunners_List_Pack2.List;
    Tmp_Runner  : Table_Arunners.Data_Type;
    Laybet_Is_Placed : Boolean := False;
    
    
    First_Time : Boolean := True;
  begin
    Log(Me & "Run", "Treat market: " &  Market_Notification.Market_Id);
    Market.Marketid := Market_Notification.Market_Id;

    -- check if ok to bet and set bet size
    case Mode is 
      when Real       => 
        Rpc.Get_Balance(Betfair_Result => Betfair_Result, Saldo => Saldo);
        if abs(Saldo.Exposure) > 0.3 * Saldo.Balance then
          Log(Me & "Run", "Too much exposure - > 30% - skip this race " & Saldo.To_String);
          return;
        end if;
      when Simulation => null;
    end case;                      

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
    
    if not Table_Apricesfinish.Is_Existing_I1(Marketid => Market.Marketid) then
      Log(Me & "Run", "no Apricesfinish found");
      return;
    end if;

    Log(Me & "Run", "market found  " & Market.To_String);

    
    Tmp_Runner.Marketid := Market.Marketid;
    Tmp_Runner.Read_I1_Marketid(Runner_List);
    
    --lay all runners with backodds 25-35  
    declare
      Aprices_Data : Table_Aprices.Data_Type;
      Aprices_List : Table_Aprices.Aprices_List_Pack2.List;
      Tmp_Arunner_Data : Table_Arunners.Data_Type;
      Eos : Boolean := False;
      
    begin
      Aprices_Data.Marketid := Market.Marketid;
      Aprices_Data.Read_I1_Marketid(Aprices_List);
      
      for p of Aprices_List loop
        Log("checking prices: " & " " & p.To_String);
        Tmp_Arunner_Data.Marketid := P.Marketid;
        Tmp_Arunner_Data.Selectionid := P.Selectionid;
        Tmp_Arunner_Data.Read(Eos);
        if Eos then
          Move("MISSING",Tmp_Arunner_Data.Status);
        end if;
            if Tmp_Arunner_Data.Status(1..7) /= "REMOVED" and then
               p.Backprice >= Fixed_Type(10.0) and then
               p.Backprice <= Fixed_Type(550.0) and then
               p.Layprice  <= Fixed_Type(35.0) and then
               p.Layprice  >= Fixed_Type(25.0) then
               
               Log("lay_bet " & " " & p.To_String);
               case Mode is 
                 when Real =>
                   Rpc.Place_Bet(Bet_Name         => Lay_Bet_Name,
                                 Market_Id        => Market.Marketid,
                                 Side             => Lay,
                                 Runner_Name      => Tmp_Arunner_Data.Runnernamestripped,
                                 Selection_Id     => p.Selectionid,
                                 Size             => Bet_Size_Type(40.0),
                                 Price            => Bet_Price_Type(200.0),
                                 Bet_Persistence  => Persist,
                                 Bet              => Bet.Lay_Bet ) ;
                 when Simulation =>
                   Sim.Place_Bet(Bet_Name         => Lay_Bet_Name,
                                 Market_Id        => Market.Marketid,
                                 Side             => Lay,
                                 Runner_Name      => Tmp_Arunner_Data.Runnernamestripped,
                                 Selection_Id     => p.Selectionid,
                                 Size             => Bet_Size_Type(40.0),
                                 Price            => Bet_Price_Type(p.Layprice),
                                 Bet_Persistence  => Persist,
                                 Bet_Placed       => p.Pricets,
                                 Bet              => Bet.Lay_Bet ) ;
               end case;                      
               Laybet_Is_Placed := True;
              
              begin
                T.Start;
                  if Bet.Lay_Bet.Betid = 0 then
                    Bet.Lay_Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
                  end if;
                  Bet.Lay_Bet.Insert; -- save to db
                  Log("Lay_Bet inserted " & " " &  Bet.Lay_Bet.To_String);
                T.Commit;
              exception
                when Sql.Duplicate_Index =>
                  T.Rollback;
                  Log("Duplicate_Index on laybet " & " " &  Bet.Lay_Bet.To_String);
              end ;             
              
              if Trim(Bet.Lay_Bet.Status) = "EXECUTION_COMPLETE" then
                 Bet_List.Append(Bet);
              else
                case Mode is 
                  when Real =>
                    if Rpc.Cancel_Bet(Bet.Lay_Bet) then
                      -- bet cancelled 
                      Log("cancelled bet on runner " & " " & p.To_String);
                    else
                      -- bet was matched just now
                      Move( "EXECUTION_COMPLETE", Bet.Lay_Bet.Status);
                      Bet_List.Append(Bet);
                    end if;               
                  when Simulation => 
                    raise Bad_Mode with Bet.Lay_Bet.To_String;                
                end case;                      
              end if;
            end if;  
      end loop;
    end;
    
    
    
    -- do the poll
    Log(Me & "Make_Bet", "start poll_loop for market '" & Market.To_String & "'");
    Poll_Loop : loop


      Price_List.Clear;
      Tmp_Price_List.Clear;
      case Mode is 
        when Real =>
          Rpc.Get_Market_Prices(Market_Id  => Market_Notification.Market_Id,
                                Market     => Market,
                                Price_List => Tmp_Price_List,
                                In_Play    => In_Play);
        when Simulation =>
     --   Log(Me & "Make_Bet", " enter Get_Market_Prices Market.Status(1..4) " &  Market.Status(1..4));
          Sim.Get_Market_Prices(Market_Id  => Market_Notification.Market_Id,
                                Market     => Market,
                                Price_List => Tmp_Price_List,
                                In_Play    => In_Play);
       -- Log(Me & "Make_Bet", " exit Get_Market_Prices Market.Status(1..4) " &  Market.Status(1..4));
      end case;                      
     -- Log(Me & "Make_Bet", " Market.Status(1..4) " &  Market.Status(1..4));
      exit Poll_Loop when Market.Status(1..4) /= "OPEN";

      if First_Time then
        declare
          i : Integer := 0;
        begin
           -- get a free slot in the array
          for s of Tmp_Price_List loop
            i := i +1;
            Sim.Fifo(i).In_Use := True;
            Sim.Fifo(i).Selectionid := S.Selectionid;
            Sim.Fifo(i).Index := i;
          end loop;
        end;
        First_Time := False;
      end if;        

      Sim.Filter_List(Tmp_Price_List, Price_List);
      
     -- Sim.Create_Runner_Data(Price_List => Tmp_Price_List, Is_Average => False) ;
     -- Sim.Create_Runner_Data(Price_List => Price_List,     Is_Average => True) ;
      
      
     -- Log(Me & "Make_Bet", "Tmp_Price_List.Length/Price_List.Length " & Tmp_Price_List.Length'img & "/" & Price_List.Length'img);
      
      -- price list may be empty efter Filter -- buffering up
      if Price_List.Length > 0 then

        
        if not Has_Been_In_Play then
          -- toggle the first time we see in-play=true
          -- makes us insensible to Betfair toggling bug
          Has_Been_In_Play := In_Play;
        end if;
        
        if not Has_Been_In_Play then
          if Current_Turn_Not_Started_Race >= Cfg.Max_Turns_Not_Started_Race then
             Log(Me & "Make_Bet", "Market took too long time to start, give up");
             exit Poll_Loop;
          else
            Current_Turn_Not_Started_Race := Current_Turn_Not_Started_Race +1;
            delay 30.0; -- no need for heavy polling before start of race
          end if;
        else
          case Mode is 
            when Real       => delay 0.05; -- to avoid more than 20 polls/sec
            when Simulation => null;
          end case;        
        end if;
        
        if Price_List.Length < 8 then
          exit Poll_Loop;
        end if;
        
        -- ok sort the runners with HIGHEST backprice first:
        Backprice_Sorter.Sort(Price_List);
        
        declare
          Stale_Data : Boolean := False;
          Runner_Has_Bet_Already : Boolean := False;
          DB_Runner : Table_Arunners.Data_Type;
        begin
          Laybet_Is_Placed := True; -- make lay bets only at start, not during race
          if not Laybet_Is_Placed then
            for Runner of Price_List loop        
              -- check if this runner already has a bet
              for b of Bet_List loop
                if b.Lay_Bet.Selectionid = Runner.Selectionid then
                  Runner_Has_Bet_Already := True;
                  exit;
                end if;               
              end loop;
              
              -- find the db-record for this runner. Contains the runner names
              for r of Runner_List loop
                if r.Selectionid = Runner.Selectionid then
                  DB_Runner := r;
                  exit;
                end if;               
              end loop;
              
              if not Runner_Has_Bet_Already and then
                 Runner.Status(1..6) = "ACTIVE" and then
                 Runner.Backprice <= Fixed_Type(500.0) and then -- proof of real value
                 Runner.Layprice  >= Fixed_Type(10.0) and then -- proof of real value
                 Runner.Layprice  <= Fixed_Type(400.0) and then
                 Runner.Backprice >= Fixed_Type(10.0) then
                 
                 Log("lay_bet " & " " & Runner.To_String);
                 case Mode is 
                   when Real =>
                     Rpc.Place_Bet(Bet_Name         => Lay_Bet_Name,
                                   Market_Id        => Market.Marketid,
                                   Side             => Lay,
                                   Runner_Name      => DB_Runner.Runnernamestripped,
                                   Selection_Id     => Runner.Selectionid,
                                   Size             => Bet_Size_Type(30.0),
                                   Price            => Bet_Price_Type(200.0),
                                   Bet_Persistence  => Persist,
                                   Bet              => Bet.Lay_Bet ) ;
                   when Simulation =>
                     Sim.Place_Bet(Bet_Name         => Lay_Bet_Name,
                                   Market_Id        => Market.Marketid,
                                   Side             => Lay,
                                   Runner_Name      => DB_Runner.Runnernamestripped,
                                   Selection_Id     => Runner.Selectionid,
                                   Size             => Bet_Size_Type(30.0),
                                   Price            => Bet_Price_Type(Runner.Layprice +Fixed_Type(10.0)),
                                   Bet_Persistence  => Persist,
                                   Bet_Placed       => Runner.Pricets,
                                   Bet              => Bet.Lay_Bet ) ;
                 end case;                      
                 Laybet_Is_Placed := True;
                
                begin
                  T.Start;
                    if Bet.Lay_Bet.Betid = 0 then
                      Bet.Lay_Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
                    end if;
                    Bet.Lay_Bet.Insert; -- save to db
                    Log("Lay_Bet inserted " & " " &  Bet.Lay_Bet.To_String);
                  T.Commit;
                exception
                  when Sql.Duplicate_Index =>
                    T.Rollback;
                    Log("Duplicate_Index on laybet " & " " &  Bet.Lay_Bet.To_String);
                end ;             
                
                if Trim(Bet.Lay_Bet.Status) = "EXECUTION_COMPLETE" then
                   Bet_List.Append(Bet);
                   Stale_Data := True;
                   exit; -- data is stale now > 1 s old, poll again for other runners to lay
                else
                  case Mode is 
                    when Real =>
                      if Rpc.Cancel_Bet(Bet.Lay_Bet) then
                        -- bet cancelled 
                        Log("cancelled bet on runner " & " " & Runner.To_String);
                      else
                        -- bet was matched just now
                        Move( "EXECUTION_COMPLETE", Bet.Lay_Bet.Status);
                        Bet_List.Append(Bet);
                      end if;               
                      Stale_Data := True;
                      exit; -- data is stale now > 1 s old, poll again for other runners to lay 
                    when Simulation => 
                      raise Bad_Mode with Bet.Lay_Bet.To_String;                
                  end case;                      
                end if;
              end if;
            end loop;
          end if; --Laybet_Is_Placed
          
          
          --check for lay bets in danger of winning 
          if not Stale_Data then 
            for b of Bet_List loop
              for o of Price_List loop
                if b.Lay_Bet.Selectionid = o.Selectionid and then
                   o.Backprice >= b.Lay_Bet.Price + Fixed_Type(10.00) and then
                   b.Back_Bet = Table_Abets.Empty_Data then   -- no previous backbet for this laybet
                  
                  Log("safety backbet " & " " & o.To_String);
                  case Mode is 
                    when Real =>
                      -- make safety backbet, and updates list
                      Rpc.Place_Bet(Bet_Name         => Back_Bet_Name,
                                    Market_Id        => Market.Marketid,
                                    Side             => Back,
                                    Runner_Name      => b.Lay_Bet.Runnername,
                                    Selection_Id     => b.Lay_Bet.Selectionid,
                                    Size             => Bet_Size_Type(150.0),
                                    Price            => Bet_Price_Type(1.01), -- we want this badly
                                    Bet_Persistence  => Persist,
                                    Bet              => b.Back_Bet ) ;
                   when Simulation => 
                      -- make safety backbet, and updates list
                      Sim.Place_Bet(Bet_Name         => Back_Bet_Name,
                                    Market_Id        => Market.Marketid,
                                    Side             => Back,
                                    Runner_Name      => b.Lay_Bet.Runnername,
                                    Selection_Id     => b.Lay_Bet.Selectionid,
                                    Size             => Bet_Size_Type(30.0),
                                    Price            => Bet_Price_Type(o.Backprice), --
                                    Bet_Persistence  => Persist,
                                    Bet_Placed       => o.Pricets,
                                    Bet              => b.Back_Bet ) ;
                  end case;                      
                                            
                  begin
                    T.Start;
                      if b.Back_Bet.Betid = 0 then
                        b.Back_Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
                        Log("Betid was 0, is now" &  b.Back_Bet.Betid'Img);
                      end if;
                      Log("will insert Back_Bet" & " " &  b.Back_Bet.To_String);
                      b.Back_Bet.Insert; -- save to db
                      Log("Back_Bet inserted " & " " &  b.Back_Bet.To_String);
                    T.Commit;
                  exception
                    when Sql.Duplicate_Index =>
                      T.Rollback;
                      Log("Duplicate_Index on backbet " & " " &  b.Back_Bet.To_String);
                  end ;             
                  
                end if;
              end loop;
            end loop;
          end if;      
        end ;
      end if ; --len price_list

    end loop Poll_Loop;
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

  Main_Loop : loop
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
           --send busy
           Run(Bot_Messages.Data(Msg));
           --send free
        when others =>
          Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout => 
        case Mode is 
          when Real       =>
            Rpc.Keep_Alive(OK);
            if not OK then
              Rpc.Login;
            end if;
          when Simulation => null;
        end case;  
    end;
    Now := Calendar2.Clock;

    --restart every day
    Is_Time_To_Exit := Now.Hour = 01 and then
                     ( Now.Minute = 00 or Now.Minute = 01) ; -- timeout = 2 min

    exit Main_Loop when Is_Time_To_Exit;

  end loop Main_Loop;

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
end Lay_In_Play_Greenup;

