with Ada.Exceptions;
with Ada.Command_Line;
with Stacktrace;
with Types; use Types;
with Sql;
with Ada.Calendar.Time_Zones;
with Calendar2; use Calendar2;
with Gnatcoll.Json; use Gnatcoll.Json;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Token ;
with Lock ;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Posix;
with Events;
with Markets;
with Runners;
with Prices;
with Ini;
with Logging; use Logging;
with Ada.Environment_Variables;
with Process_IO;
with Bot_Messages;
with Core_Messages;
with Utils; use Utils;
with RPC ;

procedure Markets_Fetcher_Greyhounds is
  package EV renames Ada.Environment_Variables;
  use type Sql.Transaction_Status_Type;

  Me : constant String := "Main.";

  Msg      : Process_Io.Message_Type;

  No_Such_UTC_Offset,
  No_Such_Field  : exception;

  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Ba_Daemon    : aliased Boolean := False;
  Cmd_Line : Command_Line_Configuration;
  Query_List_Market_Catalogue : JSON_Value := Create_Object;
  Query_List_Market_Book      : JSON_Value := Create_Object;

  Reply_List_Market_Catalogue : JSON_Value := Create_Object;
  Reply_List_Market_Book      : JSON_Value := Create_Object;

  Result_List_Market_Catalogue : JSON_Array := Empty_Array;
  Result_List_Market_Book     : JSON_Array := Empty_Array;


  Params                      : JSON_Value := Create_Object;
  Filter                      : JSON_Value := Create_Object;

  Event,
  Event_Type,
  Market                      : JSON_Value := Create_Object;

  Market_Start_Time           : JSON_Value := Create_Object;
  Market_Projection,
  Market_Countries,
  Market_Type_Codes,
  Market_Betting_Types,
  Exchange_Ids,
  Event_Type_Ids              : JSON_Array := Empty_Array;
  UTC_Offset_Minutes          : Ada.Calendar.Time_Zones.Time_Offset;

  Is_Time_To_Exit : Boolean := False;
  My_Lock         : Lock.Lock_Type;
  UTC_Time_Start,
  UTC_Time_Stop   : Calendar2.Time_Type ;
 -- Three_Minutes   : Calendar2.Interval_Type := (0,0,3,0,0);
  Eleven_Seconds  : Calendar2.Interval_Type := (0,0,0,11,0);
 -- One_Minute      : Calendar2.Interval_Type := (0,0,1,0,0);
  One_Hour        : Calendar2.Interval_Type := (0,1,0,0,0);
  Two_Hours       : Calendar2.Interval_Type := (0,2,0,0,0);
 -- Three_Days      : Calendar2.Interval_Type := (3,0,0,0,0);
 -- One_Day         : Calendar2.Interval_Type := (1,0,0,0,0);
  T               : Sql.Transaction_Type;
  Turns           : Integer := 0;

--  Is_Data_Collector : Boolean := EV.Value("BOT_USER") = "ghd" ;


  type Poll_Process is record
    Free     : Boolean := True;
    Process  : Process_IO.Process_Type := ((others => ' '),(others => ' '));
  end record;

  Data_Pollers : array (1..3) of Poll_Process := (
    1 => (True, (("poll_market_1  "), (others => ' '))),
    2 => (True, (("poll_market_2  "), (others => ' '))),
    3 => (True, (("poll_market_3  "), (others => ' ')))
--    4 => (True, (("poll_market_4  "), (others => ' '))),
--    5 => (True, (("poll_market_5  "), (others => ' '))),
--    6 => (True, (("poll_market_6  "), (others => ' '))),
--    7 => (True, (("poll_market_7  "), (others => ' '))),
--    8 => (True, (("poll_market_8  "), (others => ' ')))
  );

  Race_Pollers_1 : array (1..3) of Poll_Process := (
    1 => (True, (("play_market_1  "), (others => ' '))),
    2 => (True, (("play_market_2  "), (others => ' '))),
    3 => (True, (("play_market_3  "), (others => ' ')))
  );
  Race_Pollers_2 : array (1..3) of Poll_Process := (
    1 => (True, (("gh_poll_1      "), (others => ' '))),
    2 => (True, (("gh_poll_2      "), (others => ' '))),
    3 => (True, (("gh_poll_3      "), (others => ' ')))
  );

  Test_Pollers : array (1..4) of Poll_Process := (
    1 => (True, (("poll_bounds_1  "), (others => ' '))),
    2 => (True, (("poll_bounds_2  "), (others => ' '))),
    3 => (True, (("poll_bounds_3  "), (others => ' '))),
    4 => (True, (("poll_bounds_4  "), (others => ' ')))
  );
---------------------------------------------------------------


  procedure Insert_Event(Event, Event_Type : JSON_Value) is
    DB_Event : Events.Event_Type := Events.Empty_Data;
    Eos : Boolean := False;
  begin
    Log(Me, "Insert_Event start");
    Rpc.Parse_Event(Event, Event_Type, DB_Event);
    DB_Event.Read(Eos);
    if Eos then
      DB_Event.Insert;
      Log(Me, "insert " & DB_Event.To_String);
    end if;
    Log(Me, "Insert_Event stop");
  end Insert_Event;
  ------------------------------------------------------------
  procedure Insert_Market(Market : JSON_Value) is
    Service : constant String := "Insert_Market";
    DB_Market : Markets.Market_Type := Markets.Empty_Data;
    Eos, In_Play    : Boolean    := False;
  begin
    Rpc.Parse_Market(Market, DB_Market, In_Play);
    DB_Market.Read(Eos);
    if Eos then
      DB_Market.Insert;
      Log(Me & Service, "inserted " & DB_Market.To_String);
    end if;
  end Insert_Market;
  ----------------------------------------------------------------
  procedure Update_Market(Market : JSON_Value) is
    Service : constant String := "Update_Market";
    DB_Market : Markets.Market_Type := Markets.Empty_Data;
    Eos, In_Play : Boolean := False;
  begin
    Log(Me & Service, "start");
    if Market.Has_Field("marketId") then
      Log(Me, "marketId - '" & Market.Get("marketId") & "'");
      Move(Market.Get("marketId"), DB_Market.Marketid);
    else
      raise No_Such_Field with "Object 'Market' - Field 'marketId'";
    end if;

    Log(Me & Service, "will update " & DB_Market.Marketid);
    DB_Market.Read(Eos);
    if not Eos then
      Rpc.Parse_Market(Market, DB_Market, In_Play);
      DB_Market.Update_Withcheck;
    end if;

     Log(Me & Service, DB_Market.To_String);
    Log(Me & Service, "stop");
  end Update_Market;

  -------------------------------------------------------------

  procedure Insert_Runners(Market : JSON_Value) is
    Runner_List : Runners.Lists.List;
    Service : constant String := "Insert_Runners";
    Eos : Boolean := False;
  begin
    Log(Me & Service, "start");
    Rpc.Parse_Runners(Market, Runner_List);
    for DB_Runner of Runner_List loop
      DB_Runner.Read( Eos);
      if Eos then
        DB_Runner.Insert;
      end if;
    end loop;
    Log(Me & Service, "stop");
  end Insert_Runners;
  -------------------------------------------------------------

  procedure Insert_Prices(Market : JSON_Value) is
    Eos : Boolean := False;
    Price_List : Prices.Lists.List;
    Service : constant String := "Insert_Runners";
  begin
    Log(Me & Service, "start");
    Rpc.Parse_Prices(Market, Price_List);
    for DB_Runner_Price of Price_List loop
      Log(Me, DB_Runner_Price.To_String);
      DB_Runner_Price.Read(Eos);
      if Eos then
        DB_Runner_Price.Insert;
      end if;
    end loop;
    Log(Me & Service, "stop");
  end Insert_Prices;
  ---------------------------------------------------------------------

  procedure Set_Poller_State(Msg : in Process_Io.Message_Type) is
    Data : Bot_Messages.Poll_State_Record := Bot_Messages.Data(Msg);
    --use type Process_Io.Name_Type;
  begin
    Log(Me, "setting " & Trim(Data.Name) & " to state: " & Data.Free'Img );
    for i in Data_Pollers'range loop
      if Data_Pollers(i).Process.Name = Data.Name then
        Data_Pollers(i).Free := Data.Free = 1; --1 is used as free - 0 as not free
        return;
      end if;
    end loop;
    for i in Race_Pollers_1'range loop
      if Race_Pollers_1(i).Process.Name = Data.Name then
        Race_Pollers_1(i).Free := Data.Free = 1; --1 is used as free - 0 as not free
        return;
      end if;
    end loop;
    for i in Race_Pollers_2'range loop
      if Race_Pollers_2(i).Process.Name = Data.Name then
        Race_Pollers_2(i).Free := Data.Free = 1; --1 is used as free - 0 as not free
        return;
      end if;
    end loop;

    for i in Test_Pollers'range loop
      if Test_Pollers(i).Process.Name = Data.Name then
        Test_Pollers(i).Free := Data.Free = 1; --1 is used as free - 0 as not free
        return;
      end if;
    end loop;
  end Set_Poller_State;

------------------------------ main start -------------------------------------
  Is_Time_To_Check_Markets : Boolean               := True;
  Market_Found             : Boolean               := True;
  pragma Warnings(Off, Market_Found);

  Market_Ids               : JSON_Array            := Empty_Array;
  Minute_Last_Check        : Calendar2.Minute_Type := 0;
  Now                      : Calendar2.Time_Type   := Calendar2.Clock;
  OK                       : Boolean               := True;

begin
  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");

  Logging.Open(EV.Value("BOT_HOME") & "/log/markets_fetcher.log");

  Define_Switch
   (Cmd_Line,
    Sa_Par_Bot_User'access,
    Long_Switch => "--user=",
    Help        => "user of bot");

  Define_Switch
     (Cmd_Line,
      Ba_Daemon'access,
      "-d",
      Long_Switch => "--daemon",
      Help        => "become daemon at startup");
  Getopt (Cmd_Line);  -- process the command line

  if Ba_Daemon then
     Posix.Daemonize;
  end if;
   --must take lock AFTER becoming a daemon ...
   --The parent pid dies, and would release the lock...
  My_Lock.Take(EV.Value("BOT_NAME"));

  Log(Me, "Login");

  Rpc.Init(
            Username   => Ini.Get_Value("betfair","username",""),
            Password   => Ini.Get_Value("betfair","password",""),
            Product_Id => Ini.Get_Value("betfair","product_id",""),
            Vendor_Id  => Ini.Get_Value("betfair","vendor_id",""),
            App_Key    => Ini.Get_Value("betfair","appkey","")
          );
  Rpc.Login;

  Sql.Connect
        (Host     => Ini.Get_Value("database","host",""),
         Port     => Ini.Get_Value("database","port",5432),
         Db_Name  => Ini.Get_Value("database","name",""),
         Login    => Ini.Get_Value("database","username",""),
         Password => Ini.Get_Value("database","password",""));

   -- json stuff

   -- Create JSON arrays
  Append(Exchange_Ids , Create("1"));      -- Not Australia

  Append(Event_Type_Ids , Create("4339"));    -- greyhounds
--   none for all countries
  Append(Market_Countries , Create("GB"));
  --Append(Market_Countries , Create("IE"));
  Append(Market_Type_Codes , Create("WIN"));                 -- for horses/hounds
  Append(Market_Type_Codes , Create("PLACE"));               -- for horses/hounds
  Append(Market_Projection , Create("MARKET_DESCRIPTION"));
  Append(Market_Projection , Create("RUNNER_DESCRIPTION"));
  Append(Market_Projection , Create("EVENT"));
  Append(Market_Projection , Create("EVENT_TYPE"));
  Append(Market_Projection , Create("MARKET_START_TIME"));

  Main_Loop : loop
    Market_Found := False;
    Turns := Turns + 1;
    Log(Me, "Turns:" & Turns'Img);

    loop
      begin
        Process_Io.Receive(Msg, 5.0);
        if Sql.Transaction_Status /= Sql.None then
          raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
        end if;

        Log(Me, "msg : "& Process_Io.Identity(Msg)'Img & " from " & Utils.Trim(Process_Io.Sender(Msg).Name));
        case Process_Io.Identity(Msg) is
          when Core_Messages.Exit_Message      => exit Main_Loop;
          when Bot_Messages.Poll_State_Message => Set_Poller_State(Msg);
          when others => Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
        end case;
      exception
          when Process_io.Timeout =>
          if Sql.Transaction_Status /= Sql.None then
            raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
          end if;
      end;
      Now := Calendar2.Clock;
      Is_Time_To_Check_Markets := Now.Second >= 50 and then Minute_Last_Check /= Now.Minute;

      Log(Me, "Is_Time_To_Check_Markets: " & Is_Time_To_Check_Markets'Img);
      exit when Is_Time_To_Check_Markets;

      --restart every day
      Is_Time_To_Exit := Now.Hour = 01 and then
                       Now.Minute = 02 ;

      exit Main_Loop when Is_Time_To_Exit;
    end loop;
    Minute_Last_Check := Now.Minute;

    UTC_Offset_Minutes := Ada.Calendar.Time_Zones.UTC_Time_Offset;
    case UTC_Offset_Minutes is
      when 60     => UTC_Time_Start := Now - One_Hour;
      when 120    => UTC_Time_Start := Now - Two_Hours;
      when others => raise No_Such_UTC_Offset with UTC_Offset_Minutes'Img;
    end case;

    --check for stale token - send keepAlive, and re-login if bad
    Rpc.Keep_Alive(Ok);
    if not Ok then
      begin
        Rpc.Login;
      exception
        when Rpc.Login_Failed =>
          Log(Me, "login failed, but will try again");
      end;
    end if;

    UTC_Time_Stop  := UTC_Time_Start + Eleven_Seconds;
    --UTC_Time_Start := UTC_Time_Start ;

    T.Start;

    Market_Start_Time.Set_Field(Field_Name => "from", Field => Calendar2.String_Date_Time_ISO(UTC_Time_Start));
    Market_Start_Time.Set_Field(Field_Name => "to",   Field => Calendar2.String_Date_Time_ISO(UTC_Time_Stop));

    Filter.Set_Field (Field_Name => "exchangeIds",        Field => Exchange_Ids);
    Filter.Set_Field (Field_Name => "eventTypeIds",       Field => Event_Type_Ids);
    Filter.Set_Field (Field_Name => "marketCountries",    Field => Market_Countries);
    Filter.Set_Field (Field_Name => "marketTypeCodes",    Field => Market_Type_Codes);
    Filter.Set_Field (Field_Name => "marketBettingTypes", Field => Market_Betting_Types);
--    Filter.Set_Field (Field_Name => "inPlayOnly",         Field => False);
--    Filter.Set_Field (Field_Name => "turnInPlayEnabled",  Field => True);
    Filter.Set_Field (Field_Name => "marketStartTime",    Field => Market_Start_Time);

    Params.Set_Field (Field_Name => "filter",           Field => Filter);
    Params.Set_Field (Field_Name => "marketProjection", Field => Market_Projection);
    Params.Set_Field (Field_Name => "locale",           Field => "en"); -- to get 'the draw' instead of 'Oavgjort'
    Params.Set_Field (Field_Name => "sort",             Field => "FIRST_TO_START");
    Params.Set_Field (Field_Name => "maxResults",       Field => "999");

    Query_List_Market_Catalogue.Set_Field (Field_Name => "params",  Field => Params);
    Query_List_Market_Catalogue.Set_Field (Field_Name => "id",      Field => 15);          -- ???
    Query_List_Market_Catalogue.Set_Field (Field_Name => "method",  Field => "SportsAPING/v1.0/listMarketCatalogue");
    Query_List_Market_Catalogue.Set_Field (Field_Name => "jsonrpc", Field => "2.0");

    Rpc.Get_JSON_Reply(Query => Query_List_Market_Catalogue,
                       Reply => Reply_List_Market_Catalogue,
                       URL   => Token.URL_BETTING);

    if Rpc.API_Exceptions_Are_Present(Reply_List_Market_Catalogue) then
      exit Main_loop;  --  exit main loop, let cron restart program
    end if;

    if Reply_List_Market_Catalogue.Has_Field("result") then
       Result_List_Market_Catalogue := Reply_List_Market_Catalogue.Get("result");
       for i in 1 .. Length (Result_List_Market_Catalogue) loop
         Log(Me, "we have result #:" & i'img);
         Market := Get(Result_List_Market_Catalogue, i);

         if Market.Has_Field("marketId") then
           Market_Found := True;
           Insert_Market(Market);
           Event := Market.Get("event");
           if not Event.Has_Field("id") then
             Log(Me, "we no event:" & i'img & " event:" & Event.Write );
           end if;
         end if;

         if Market.Has_Field("eventType") then
           Event_Type := Market.Get("eventType");
           Insert_Event(Event, Event_Type);
         else
            Log(Me, "we no eventType:" & i'img & " eventType:" & Event_Type.Write );
         end if;

         if Market.Has_Field("runners") then
            Insert_Runners(Market);
         end if;
       end loop;
    end if;
      -- now get the prices
    T.Commit;

    declare
       Params                      : JSON_Value := Create_Object;
       Price_Data                  : JSON_Array := Empty_Array;
       Price_Projection            : JSON_Value := Create_Object;
       Has_Id                      : Boolean  := False;
       One_Market_Id               : JSON_Array := Empty_Array;
    begin
      Market_Ids    := Empty_Array;

      Log(Me, "Found" & Length (Result_List_Market_Catalogue)'Img & " markets");

      for i in 1 .. Length (Result_List_Market_Catalogue) loop
        Log(Me, "process market" & i'img & " of" & Length (Result_List_Market_Catalogue)'Img & " markets");
        Market := Get(Result_List_Market_Catalogue, i);
        Has_Id := False;
        if Market.Has_Field("marketId") then
          Has_Id := True;
          Log(Me, "appending Marketid: '" & Market.Get("marketId") & "'" );
          Append(Market_Ids, Create(string'(Market.Get("marketId")))); --used further down
          One_Market_Id := Empty_Array; --empty it here, to avoid TOO_MUCH_DATA replies
          Append(One_Market_Id, Create(string'(Market.Get("marketId"))));
        end if;

        if Has_Id then
          Append (Price_Data , Create("EX_BEST_OFFERS"));
          Price_Projection.Set_Field (Field_Name => "priceData", Field => Price_Data);
          Params.Set_Field (Field_Name => "priceProjection", Field => Price_Projection);
          Params.Set_Field (Field_Name => "currencyCode",    Field => "SEK");
          Params.Set_Field (Field_Name => "locale",          Field => "sv");
          Params.Set_Field (Field_Name => "marketIds",       Field => One_Market_Id);

          Query_List_Market_Book.Set_Field (Field_Name => "params",  Field => Params);
          Query_List_Market_Book.Set_Field (Field_Name => "id",      Field => 15);   --?
          Query_List_Market_Book.Set_Field (Field_Name => "method",  Field => "SportsAPING/v1.0/listMarketBook");
          Query_List_Market_Book.Set_Field (Field_Name => "jsonrpc", Field => "2.0");

          Rpc.Get_JSON_Reply(Query => Query_List_Market_Book,
                             Reply => Reply_List_Market_Book,
                             URL   => Token.URL_BETTING);

             --  Iterate the Reply_List_Market_Book object.
          if Reply_List_Market_Book.Has_Field("result") then
            Log(Me, "we have result ");
            Result_List_Market_Book := Reply_List_Market_Book.Get("result");
            for i in 1 .. Length (Result_List_Market_Book) loop
              Log(Me, "we have result #:" & i'img);
              Market := Get(Result_List_Market_Book, i);

              if Market.Has_Field("marketId") then

                Trf_Loop : loop
                  begin
                    T.Start;
                    Update_Market(Market);
                    if Market.Has_Field("runners") then
                       Insert_Prices(Market);
                    end if;
                    T.Commit;
                    exit Trf_Loop;
                  exception
                    when Sql.No_Such_Row =>
                      T.Rollback;
                      Log(Me, "Trf conflict on update of marketid " & Market.Get("marketId"));
                      delay 0.1;
                  end ;
                end loop Trf_Loop;
              end if;
            end loop;
          end if;
        end if; --has id
      end loop; --for loop
    end;

  --just get prices at race start for now

  --  Log(Me, "Market_Found: " & Market_Found'Img);
  --  if Market_Found then
  --    declare
  --      Market   : JSON_Value := Create_Object;
  --      MNR      : Bot_Messages.Market_Notification_Record;
  --      --Receiver : Process_IO.Process_Type := ((others => ' '), (others => ' '));
  --      type Eos_Type is (Amarket, Aevent);
  --      Eos       : array (Eos_Type'range) of Boolean := (others => False);
  --      Db_Market : Markets.Market_Type;
  --      Db_Event  : Table_AEvents.Event_Type;
  --      --------------------------------------------------------------------
  --
  --    begin
  --      for i in 1 .. Length (Market_Ids) loop
  --        Market := Get(Market_Ids, i);
  --        MNR.Market_Id := (others => ' ');
  --        Move(String'(Market.Get),MNR.Market_Id);
  --        --some more detailed dispatching is needed now
  --        -- what kind of event is it.
  --        T.Start;
  --          Db_Market.Marketid := MNR.Market_Id;
  --          Table_Amarkets.Read(DB_Market, Eos(Amarket));
  --          if not Eos(Amarket) then
  --            Db_Event.Eventid := Db_Market.Eventid;
  --            Table_Aevents.Read(Db_Event, Eos(Aevent));
  --            if not Eos(Aevent) then
  --              Log(Me, "DB_Event.Eventtypeid: " & DB_Event.Eventtypeid'Img);
  --
  --              case DB_Event.Eventtypeid is
  --                ------------------------------------------------------------------
  --                when 4339      => -- greyhounds
  --                  if Is_Data_Collector then
  --                    for i in Data_Pollers'range loop
  --                      Log(Me, "Data_Pollers(i).Free: " & Data_Pollers(i).Free'Img);
  --                      if Data_Pollers(i).Free then
  --                        Log(Me, "Notifying " & Trim(Data_Pollers(i).Process.Name) & " with marketid: '" & MNR.Market_Id & "'");
  --                        Bot_Messages.Send(Process_IO.To_Process_Type(Trim(Data_Pollers(i).Process.Name)), MNR);
  --                        Data_Pollers(i).Free := False;
  --                        exit;
  --                      end if;
  --                    end loop;
  --
  --                  --elsif Is_Better then
  --                    for i in Race_Pollers_1'range loop
  --                      Log(Me, "Race_Pollers_1(i).Free: " & Race_Pollers_1(i).Free'Img);
  --                      if Race_Pollers_1(i).Free then
  --                        Log(Me, "Notifying " & Trim(Race_Pollers_1(i).Process.Name) & " with marketid: '" & MNR.Market_Id & "'");
  --                        Bot_Messages.Send(Process_IO.To_Process_Type(Trim(Race_Pollers_1(i).Process.Name)), MNR);
  --                        Race_Pollers_1(i).Free := False;
  --                        exit;
  --                      end if;
  --                    end loop;
  --
  --                    for i in Race_Pollers_2'range loop
  --                      Log(Me, "Race_Pollers_2(i).Free: " & Race_Pollers_2(i).Free'Img);
  --                      if Race_Pollers_2(i).Free then
  --                        Log(Me, "Notifying " & Trim(Race_Pollers_2(i).Process.Name) & " with marketid: '" & MNR.Market_Id & "'");
  --                        Bot_Messages.Send(Process_IO.To_Process_Type(Trim(Race_Pollers_2(i).Process.Name)), MNR);
  --                        Race_Pollers_2(i).Free := False;
  --                        exit;
  --                      end if;
  --                    end loop;
  --                  --
  --                  --elsif Is_Tester then
  --                  --  for i in Test_Pollers'range loop
  --                  --    if Test_Pollers(i).Free then
  --                  --      Log(Me, "Notifying " & Trim(Test_Pollers(i).Process.Name) & " with marketid: '" & MNR.Market_Id & "'");
  --                  --      Bot_Messages.Send(Process_IO.To_Process_Type(Trim(Test_Pollers(i).Process.Name)), MNR);
  --                  --      Test_Pollers(i).Free := False;
  --                  --      exit;
  --                  --    end if;
  --                  --  end loop;
  --                  end if;
  --
  --                ------------------------------------------------------------------
  --                when others => null;
  --                ------------------------------------------------------------------
  --              end case;
  --            end if;
  --          end if;
  --        T.Commit;
  --      end loop;
  --    end;
  --  end if;
  end loop Main_Loop;

  Log(Me, "shutting down, close db");
  Sql.Close_Session;
  Log (Me, "db closed, Is_Time_To_Exit " & Is_Time_To_Exit'Img);
  Rpc.Logout;
  Log(Me, "do_exit");
  Posix.Do_Exit(0); -- terminate
  Log(Me, "after do_exit");

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
    Posix.Do_Exit(0); -- terminate
end Markets_Fetcher_Greyhounds;
