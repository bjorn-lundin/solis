with Ada.Exceptions;
with Ada.Command_Line;
with Stacktrace;
with Types; use Types;
with Sql;
with Ada.Calendar.Time_Zones;
with Calendar2; use Calendar2;
with Json; use Json;
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
with Core_Messages;
with Utils; use Utils;
with Table_Aokmarkets;
with RPC ;
with Aliases;
with Unknowns;

procedure Markets_Fetcher_Soccer is
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
  --Market_Countries,
  Competition_Ids,
  Market_Type_Codes,
  Market_Betting_Types,
  Exchange_Ids,
  Event_Type_Ids              : JSON_Array := Empty_Array;
  UTC_Offset_Minutes          : Ada.Calendar.Time_Zones.Time_Offset;

  Is_Time_To_Exit : Boolean := False;
  My_Lock         : Lock.Lock_Type;
  UTC_Time_Start,
  UTC_Time_Stop   : Calendar2.Time_Type ;
  One_Hour        : Calendar2.Interval_Type := (0,1,0,0,0);
  Two_Hours       : Calendar2.Interval_Type := (0,2,0,0,0);
  Twelve_Hours    : Calendar2.Interval_Type := (0,12,0,0,0);
  --One_Day         : Calendar2.Interval_Type := (1,0,0,0,0);
  T               : Sql.Transaction_Type;
  Turns           : Integer := 0;

  --Eos_Okmarket,
  Market_Is_Ok : Boolean := False;
  Okmarket : Table_Aokmarkets.Data_Type;
  -----------------------------------------------

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
    Alias : Aliases.Alias_Type;
    Unknown : Unknowns.Unknown_Type;
  begin
    Log(Me & Service, "start");
    Rpc.Parse_Runners(Market, Runner_List);
    for DB_Runner of Runner_List loop
      DB_Runner.Read( Eos);
      if Eos then
        DB_Runner.Insert;
      end if;
      Alias.Teamname := Db_Runner.Runnername;
      Alias.Read_Teamname(Eos);
      if Eos then
        if (Db_Runner.Selectionid < 20) or         -- 0-0, 1-0 ..
           (Db_Runner.Selectionid = 58805) or      --the draw
           (Db_Runner.Selectionid = 9063254) or    --Any Other Home Win
           (Db_Runner.Selectionid = 9063255) or    --Any Other Away Win
           (Db_Runner.Selectionid = 9063256) then  -- Any Other Draw

          null;
        else
          Log(Me & Service, "runner not in alias " & Db_Runner.To_String);
          Unknown.Teamname := Db_Runner.Runnername;
          Unknown.Read(Eos);
          if Eos then
            Unknown.Countrycode:= "XX";
            Unknown.Insert;
          end if;
        end if;
      end if;
    end loop;
    Log(Me & Service, "stop");
  end Insert_Runners;
  -------------------------------------------------------------

  procedure Insert_Prices(Market : JSON_Value) is
    Eos : Boolean := False;
    Price_List : Prices.Lists.List;
    Service : constant String := "Insert_Prices";
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

------------------------------ main start -------------------------------------
  Market_Ids               : JSON_Array            := Empty_Array;
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

  Append(Event_Type_Ids , Create("1"));    -- soccer
--   none for all countries
--    Append(Market_Countries  , Create("GB"));
--    Append(Market_Countries  , Create("ES"));
--    Append(Market_Countries  , Create("IT"));
--    Append(Market_Countries  , Create("PT"));
--    Append(Market_Countries  , Create("FR"));
--    Append(Market_Countries  , Create("NL"));
--    Append(Market_Countries  , Create("DE"));
--    Append(Market_Countries  , Create("SE"));
--    Append(Market_Countries  , Create("DK"));
--    Append(Market_Countries  , Create("BE"));


  Append(Competition_Ids , Create("228"));    --Champions league
  Append(Competition_Ids , Create("31"));     --English Premier League
  Append(Competition_Ids , Create("129"));    --Swedish Allsvenskan
  Append(Competition_Ids , Create("23"));     --Danish Superliga
  Append(Competition_Ids , Create("59"));     --German Bundesliga 1
  Append(Competition_Ids , Create("89979"));  --Belgian Jupiler League
  Append(Competition_Ids , Create("9404054"));--Dutch Eredivisie
  Append(Competition_Ids , Create("81"));     --Italian Serie A
  Append(Competition_Ids , Create("55"));     --French Ligue 1
  Append(Competition_Ids , Create("117"));    --Spanish  Primera Division
  Append(Competition_Ids , Create("99"));     --Portoguise Primeira Liga


  Append(Market_Type_Codes , Create("MATCH_ODDS"));
  Append(Market_Type_Codes , Create("CORRECT_SCORE"));
  Append(Market_Projection , Create("MARKET_DESCRIPTION"));
  Append(Market_Projection , Create("RUNNER_DESCRIPTION"));
  Append(Market_Projection , Create("EVENT"));
  Append(Market_Projection , Create("EVENT_TYPE"));
  Append(Market_Projection , Create("MARKET_START_TIME"));

  Main_Loop : loop
    Turns := Turns + 1;
    Log(Me, "Turns:" & Turns'Img);

    declare
      Timeout : Duration := 10.0;
    begin
      Now := Calendar2.Clock;
      case Now.Hour is
        when  0 .. 12 => Timeout := 60.0;
        when 13 .. 23 => Timeout := 10.0;
      end case;
      Process_Io.Receive(Msg, Timeout);
      if Sql.Transaction_Status /= Sql.None then
        raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
      end if;

      Log(Me, "msg : "& Process_Io.Identity(Msg)'Img & " from " & Utils.Trim(Process_Io.Sender(Msg).Name));

      case Process_Io.Identity(Msg) is
        when Core_Messages.Exit_Message =>
          exit Main_Loop;
        when others =>
          Log(Me, "Unhandled message identity: " &
                   Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout =>
        if Sql.Transaction_Status /= Sql.None then
          raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
        end if;
    end;
    Now := Calendar2.Clock;
    --restart every day
    Is_Time_To_Exit := Now.Hour = 01 and then
      Now.Minute <= 02 ;

    exit Main_Loop when Is_Time_To_Exit;

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

--    UTC_Time_Stop  := UTC_Time_Start + One_Hour;
    UTC_Time_Stop  := UTC_Time_Start + Twelve_Hours;

    T.Start;

    Market_Start_Time.Set_Field(Field_Name => "from", Field => Calendar2.String_Date_Time_ISO(UTC_Time_Start));
    Market_Start_Time.Set_Field(Field_Name => "to",   Field => Calendar2.String_Date_Time_ISO(UTC_Time_Stop));

    Filter.Set_Field (Field_Name => "exchangeIds",        Field => Exchange_Ids);
    Filter.Set_Field (Field_Name => "eventTypeIds",       Field => Event_Type_Ids);
    Filter.Set_Field (Field_Name => "competitionIds",       Field => Competition_Ids);
 --   Filter.Set_Field (Field_Name => "marketCountries",    Field => Market_Countries);
    Filter.Set_Field (Field_Name => "marketTypeCodes",    Field => Market_Type_Codes);
    Filter.Set_Field (Field_Name => "marketBettingTypes", Field => Market_Betting_Types);
--    Filter.Set_Field (Field_Name => "inPlayOnly",         Field => False);
    Filter.Set_Field (Field_Name => "turnInPlayEnabled",  Field => True);
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
           -- menuparser adds those markets that are ok wrp country/league into AOKMARKETS
           Move(Market.Get("marketId"),Okmarket.Marketid);
          -- Okmarket.Read(Eos_Okmarket);
          -- Market_Is_Ok := not Eos_Okmarket;
           --pragma Compile_Time_Warning(True,"OKmarket is overidden - always true");
           Market_Is_Ok := True;

           if Market_Is_Ok then
             Insert_Market(Market);
             Event := Market.Get("event");
             if not Event.Has_Field("id") then
               Log(Me, "we have no event:" & i'img & " event:" & Event.Write );
             end if;
           else
             Log(Me, "Market:'" & Okmarket.Marketid & "' was NOT found in AOKMARKETS - skipping" );
           end if;
         end if;

         if Market_Is_Ok then
           if Market.Has_Field("eventType") then
             Event_Type := Market.Get("eventType");
             Insert_Event(Event, Event_Type);
           else
              Log(Me, "we no eventType:" & i'img & " eventType:" & Event_Type.Write );
           end if;

           if Market.Has_Field("runners") then
              Insert_Runners(Market);
           end if;
         end if;
       end loop;
    end if;
      -- now get the prices
    T.Commit;


    if Market_Is_Ok then
      declare
         Params                      : JSON_Value := Create_Object;
         Price_Data                  : JSON_Array := Empty_Array;
         Price_Projection            : JSON_Value := Create_Object;
         Has_Id                      : Boolean  := False;
         One_Market_Id               : JSON_Array := Empty_Array;
      begin
        Market_Ids := Empty_Array;

        Log(Me, "Found" & Length (Result_List_Market_Catalogue)'Img & " markets");

        for i in 1 .. Length (Result_List_Market_Catalogue) loop
           Params           := Create_Object;
           Price_Data       := Empty_Array;
           Price_Projection := Create_Object;
           One_Market_Id    := Empty_Array;
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
    end if;

    Log(Me, "Market_Is_Ok: " & Market_Is_Ok'Img);
--      if Market_Is_Ok then
--        declare
--          Market   : JSON_Value := Create_Object;
--          MNR      : Bot_Messages.Market_Notification_Record;
--          --Receiver : Process_IO.Process_Type := ((others => ' '), (others => ' '));
--          type Eos_Type is (Amarket, Aevent);
--          Eos       : array (Eos_Type'range) of Boolean := (others => False);
--          Db_Market : Markets.Market_Type;
--          Db_Event  : Events.Event_Type;
--          --------------------------------------------------------------------
--
--        begin
--          for i in 1 .. Length (Market_Ids) loop
--            Market := Get(Market_Ids, i);
--            MNR.Market_Id := (others => ' ');
--            Move(String'(Market.Get),MNR.Market_Id);
--            --some more detailed dispatching is needed now
--            -- what kind of event is it.
--            T.Start;
--              Db_Market.Marketid := MNR.Market_Id;
--              DB_Market.Read(Eos(Amarket));
--              if not Eos(Amarket) then
--                Db_Event.Eventid := Db_Market.Eventid;
--                Db_Event.Read(Eos(Aevent));
--                if not Eos(Aevent) then
--                  case DB_Event.Eventtypeid is
--                    ------------------------------------------------------------------
--                    when 1      =>
--                      for I in Data_Pollers'Range loop
--                        if Data_Pollers(I).Free then
--                          Log(Me, "Notifying " & Trim(Data_Pollers(I).Process.Name) & " with marketid: '" & Mnr.Market_Id & "'");
--                          Bot_Messages.Send(Process_Io.To_Process_Type(Trim(Data_Pollers(I).Process.Name)), Mnr);
--                          Data_Pollers(I).Free := False;
--                          exit;
--                        end if;
--                      end loop;
--                    ------------------------------------------------------------------
--                    when others => null;
--                    ------------------------------------------------------------------
--                  end case;
--                end if;
--              end if;
--            T.Commit;
--          end loop;
--        end;
--      end if;
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
end Markets_Fetcher_Soccer;
