with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
with Rpc;
--with Lock ;
with Ini;
with Logging; use Logging;
with Bot_Svn_Info;
with Utils; use Utils;
with Table_Abets;
with Tics;
with Sim;

with Prices;
with Price_Histories;
with Markets;
with Runners;
with Bets;

procedure Lay_All_Watch_Back is

  package Ev renames Ada.Environment_Variables;
  use type Rpc.Result_Type;
  use type Ada.Containers.Count_Type;

  Me              : constant String := "Lay_All_Watch_Back.";

  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Cmd_Line        : Command_Line_Configuration;
  -------------------------------------------------------------
  type Bet_Type is record
    Laybet    : Bets.Bet_Type;
    Backbet   : Bets.Bet_Type;
  end record;
  use type Table_Abets.Data_Type;
  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_Type);
  subtype Delta_Tics_Type is Integer range 0 .. 350;

  B_Place : aliased Boolean := False;
  B_Nolay : aliased Boolean := False;


  -----------------------------------------------------------------

  procedure Run(Price_Data : in Prices.Price_Type;
                Delta_Tics : in Delta_Tics_Type;
                Lay_Size   : in Bet_Size_Type) is

    Market, Place_Market   : Markets.Market_Type;
    Eos                    : Boolean := False;
    Place_Price_During_Race_List,
    Price_During_Race_List : Price_Histories.Lists.List;

    Win_Runner,Place_Runner: Runners.Runner_Type;--table_Arunners.Data_Type;
    Tic_Lay                : Integer := 0;
    Bet                    : Bet_Type;
    Lay_Bet_Name           : String_Object;
    Back_Bet_Name          : String_Object;

    Back_Size              : Bet_Size_Type := 100.0;
    Ln                     : Betname_Type := (others => ' ');
    Bn                     : Betname_Type := (others => ' ');
    B_Price                : Fixed_Type := 0.0;
    Found_Place_Market     : Boolean := False;

    Win_History_Exists,
    Place_History_Exists   : Boolean := False;


  begin
    Log(Me & "Run", "start");

    if Delta_Tics < Delta_Tics_Type(10) then
      if Price_Data.Layprice < 10.0 then
        Lay_Bet_Name.Set("GREENUP_LAY_FIXED_LOSS_TICS_00" & Trim(Delta_Tics'Img,Both) & "_0" & F8_Image(Price_Data.Layprice));
      else
        Lay_Bet_Name.Set("GREENUP_LAY_FIXED_LOSS_TICS_00" & Trim(Delta_Tics'Img,Both) & "_" & F8_Image(Price_Data.Layprice));
      end if;

    elsif Delta_Tics < Delta_Tics_Type(100) then
      if Price_Data.Layprice < 10.0 then
        Lay_Bet_Name.Set("GREENUP_LAY_FIXED_LOSS_TICS_0" & Trim(Delta_Tics'Img,Both) & "_0" & F8_Image(Price_Data.Layprice));
      else
        Lay_Bet_Name.Set("GREENUP_LAY_FIXED_LOSS_TICS_0" & Trim(Delta_Tics'Img,Both) & "_" & F8_Image(Price_Data.Layprice));
      end if;

    else
      if Price_Data.Layprice < 10.0 then
        Lay_Bet_Name.Set("GREENUP_LAY_FIXED_LOSS_TICS_" & Trim(Delta_Tics'Img,Both) & "_0" & F8_Image(Price_Data.Layprice));
      else
        Lay_Bet_Name.Set("GREENUP_LAY_FIXED_LOSS_TICS_" & Trim(Delta_Tics'Img,Both) & "_" & F8_Image(Price_Data.Layprice));
      end if;
    end if;

    Back_Bet_Name.Set(Lay_Bet_Name.Fix_String);

    -- Log(Me & "Run", "Treat market: " &  Price_Data.Marketid);
    Market.Marketid := Price_Data.Marketid;
    Market.Read(Eos);
    if Eos then
      Log(Me & "Run", "no market found");
      return;
    end if;


    if B_Place then
      Market.Corresponding_Place_Market(Place_Market => Place_Market,Found => Found_Place_Market);

      if Found_Place_Market then
        Sim.Read_Marketid_Selectionid(Marketid    => Place_Market.Marketid,
                                      Selectionid => Price_Data.Selectionid,
                                      Animal      => Horse,
                                      List        => Place_Price_During_Race_List) ;
      else
        Log(Me & "Run", "no place market found");
        -- return;
      end if;
    end if;

    -- Log(Me & "Run", "Market: " & Market.To_String);
    Sim.Read_Marketid_Selectionid(Marketid    => Market.Marketid,
                                  Selectionid => Price_Data.Selectionid,
                                  Animal      => Horse,
                                  List        => Price_During_Race_List) ;

    if B_Place then
      Place_Runner.Marketid := Place_Market.Marketid;
      Place_Runner.Selectionid := Price_Data.Selectionid;
      Place_Runner.Read(Eos);
    end if;
    Win_Runner.Marketid := Market.Marketid;
    Win_Runner.Selectionid := Price_Data.Selectionid;
    Win_Runner.Read(Eos);


    Win_History_Exists := Price_During_Race_List.Length > 80;
    if B_Place and Win_History_Exists then
      Place_History_Exists := Place_Price_During_Race_List.Length > 80;
    end if;


    if Win_History_Exists then
      if Eos then
        Log(Me & "Run", "no runner found found  " & Win_Runner.To_String);
        return;
      end if;

      if Win_Runner.Status(1..7) = "REMOVED" then
        Log(Me & "Run", "runner removed " & Win_Runner.To_String);
        return;
      end if;

      Tic_Lay := Tics.Get_Tic_Index(Price_Data.Layprice);

      -- Log(Me & "Run", "tic_lay " & Tic_Lay'img & " " & Price_Data.To_String);

      if not B_Nolay then
        Move("WIN" & Lay_Bet_Name.Fix_String,Ln);
        Sim.Place_Bet(Bet_Name         => Ln,
                      Market_Id        => Market.Marketid,
                      Side             => Lay,
                      Runner_Name      => Win_Runner.Runnernamestripped,
                      Selection_Id     => Price_Data.Selectionid,
                      Size             => Lay_Size,
                      Price            => Bet_Price_Type(Price_Data.Layprice),
                      Bet_Persistence  => Persist,
                      Bet_Placed       => Price_Data.Pricets,
                      Bet              => Bet.Laybet ) ;
        Move("M",Bet.Laybet.Status);
        Bet.Laybet.Check_Outcome(Win_Runner);
        Bet.Laybet.Insert;
      end if;

      if Tic_Lay - Delta_Tics < Integer(1) then
        Log(Me & "Run", "Tic_Lay - Delta_Tics < 1 - can't back  " & Win_Runner.To_String);
        return;
      end if;


      B_Price := Tics.Get_Tic_Price(Tic_Lay - Delta_Tics);
      --  Back_Size := Lay_Size * Bet_Size_Type(Price_Data.Layprice/B_Price);

      -- see if we meet stop_loss or greenup
      --there is no delay here since bet is placed in beginning of race
      declare
        Not_Set    : Calendar2.Time_Type := Calendar2.Time_Type_Last - (1,1,1,1,1);
        Match_Time : Calendar2.Time_Type := Not_Set;
      begin
        Race : for Race_Data of Price_During_Race_List loop
          if Race_Data.Backprice > Fixed_Type(1.0)   and then -- must be valid
            Race_Data.Layprice  > Fixed_Type(1.0)    and then -- must be valid
            Race_Data.Backprice < Fixed_Type(1000.1) and then -- must be valid
            Race_Data.Layprice  < Fixed_Type(1000.1) then     -- must be valid

            if Race_Data.Pricets   >= Price_Data.Pricets and then
              Race_Data.Backprice <= B_Price             and then
              Match_Time           = Not_Set             then -- first time condition true

              Match_Time := Race_Data.Pricets;
              --if B_Place then
              --  exit Race;
              --end if;
            end if;

            if Race_Data.Pricets >= Match_Time + (0,0,0,1,0) then

              Move("WIN" & Back_Bet_Name.Fix_String,Bn);
              Sim.Place_Bet(Bet_Name         => Bn,
                            Market_Id        => Market.Marketid,
                            Side             => Back,
                            Runner_Name      => Win_Runner.Runnernamestripped,
                            Selection_Id     => Price_Data.Selectionid,
                            Size             => Back_Size,
                            Price            => Bet_Price_Type(Race_Data.Backprice),
                            Bet_Persistence  => Persist,
                            Bet_Placed       => Match_Time,
                            Bet              => Bet.Backbet ) ;
              Move("M",Bet.Backbet.Status);
              Bet.Backbet.Check_Outcome(Win_Runner);
              Bet.Backbet.Insert;
              exit Race;

            end if;
          end if;
        end loop Race;

        Race_Plc : for Race_Data of Place_Price_During_Race_List loop
          exit Race_Plc when not B_Place;
          exit Race_Plc when not Place_History_Exists;

          if Race_Data.Backprice > Fixed_Type(1.0)   and then -- must be valid
            Race_Data.Layprice  > Fixed_Type(1.0)    and then -- must be valid
            Race_Data.Backprice < Fixed_Type(1000.1) and then -- must be valid
            Race_Data.Layprice  < Fixed_Type(1000.1) then     -- must be valid

            if Race_Data.Pricets >= Match_Time + (0,0,0,1,0) then

              -- take first bet we can get
              Move("PLC" & Back_Bet_Name.Fix_String,Bn);
              Sim.Place_Bet(Bet_Name         => Bn,
                            Market_Id        => Place_Market.Marketid,
                            Side             => Back,
                            Runner_Name      => Place_Runner.Runnernamestripped,
                            Selection_Id     => Price_Data.Selectionid,
                            Size             => Back_Size,
                            Price            => Bet_Price_Type(Race_Data.Backprice),
                            Bet_Persistence  => Persist,
                            Bet_Placed       => Match_Time,
                            Bet              => Bet.Backbet ) ;
              Move("M",Bet.Backbet.Status);
              Bet.Backbet.Check_Outcome(Place_Runner);
              Bet.Backbet.Insert;
              exit Race_Plc;

            end if;
          end if;
        end loop Race_Plc;

      end;

    else
      Log(Me & "not enough data for runner" & Price_During_Race_List.Length'Img, Price_Data.To_String);
    end if;
  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
  ------------------------------ main start -------------------------------------
  Current_Date : Calendar2.Time_Type := Calendar2.Clock;

  Sa_Min_Layprice : aliased Gnat.Strings.String_Access;
  Sa_Max_Layprice : aliased Gnat.Strings.String_Access;
  Sa_Logfilename  : aliased Gnat.Strings.String_Access;
  Ia_Min_Tic      : aliased Integer;
  Ia_Max_Tic      : aliased Integer;


  Lay_Size        : constant Bet_Size_Type := 100.0;
  Layprice_High   : Fixed_Type := 0.0;
  Layprice_Low    : Fixed_Type := 0.0;



begin

  Define_Switch
    (Cmd_Line,
     Sa_Min_Layprice'Access,
     Long_Switch => "--min_lay=",
     Help        => "Min layprice");

  Define_Switch
    (Cmd_Line,
     Sa_Max_Layprice'Access,
     Long_Switch => "--max_lay=",
     Help        => "Min layprice");

  Define_Switch
    (Cmd_Line,
     Ia_Min_Tic'Access,
     Long_Switch => "--min_tic=",
     Help        => "min tic");

  Define_Switch
    (Cmd_Line,
     Ia_Max_Tic'Access,
     Long_Switch => "--max_tic=",
     Help        => "max tic");

  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Define_Switch
    (Cmd_Line,
     Sa_Par_Inifile'Access,
     Long_Switch => "--inifile=",
     Help        => "use alternative inifile");

  Define_Switch
    (Cmd_Line,
     B_Place'Access,
     Long_Switch => "--place",
     Help        => "back place market instead");

  Define_Switch
    (Cmd_Line,
     B_Nolay'Access,
     Long_Switch => "--nolay",
     Help        => "do not actually make a laybet");

  Getopt (Cmd_Line);  -- process the command line

  if Sa_Min_Layprice.all = "" then
    raise Constraint_Error with "no min-lay-price set";
  elsif Sa_Max_Layprice.all = "" then
    raise Constraint_Error with "no max-lay-price set";
  elsif Sa_Logfilename.all = "" then
    raise Constraint_Error with "no log file name set";
  elsif Ia_Min_Tic = 0 then
    raise Constraint_Error with "no min-tic set";
  elsif Ia_Max_Tic = 0  then
    raise Constraint_Error with "no max-tic set";
  end if;
  Log("9");

  Log(Ia_Min_Tic'Img & Ia_Max_Tic'Img & " '" & Sa_Min_Layprice.all & "' '" & Sa_Max_Layprice.all & "'");



  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","lay_all_w_b");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database_home", "host", ""),
     Port     => Ini.Get_Value("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value("database_home", "name", ""),
     Login    => Ini.Get_Value("database_home", "username", ""),
     Password =>Ini.Get_Value("database_home", "password", ""));
  Log(Me, "db Connected");

  Layprice_High := Fixed_Type'Value(Sa_Max_Layprice.all);
  Layprice_Low  := Fixed_Type'Value(Sa_Min_Layprice.all);

  declare
    Stm         : Sql.Statement_Type;
    T           : Sql.Transaction_Type;
    Price_List  : Prices.Lists.List;
  begin
    T.Start;
    Stm.Prepare(
                "select P.* " &
                  "from APRICES P, AMARKETS M, AEVENTS E " &
                  "where E.EVENTID=M.EVENTID " &
                  "and M.MARKETTYPE = 'WIN' " &
                  "and E.COUNTRYCODE in ('GB','IE') " &
                  "and P.MARKETID = M.MARKETID " &
                  "and E.EVENTTYPEID = 7 " &
                  "and P.LAYPRICE <= :MAX_LAYPRICE " &
                --  "and P.LAYPRICE >= :MIN_LAYPRICE " &
                  "order by M.STARTTS, P.MARKETID, P.SELECTIONID ");
    Stm.Set("MAX_LAYPRICE",100.0);
    Prices.Read_List(Stm, Price_List);
    T.Commit;

    Log(Layprice_Low'Img & " " & Layprice_High'Img & " " & Price_List.Length'Img);

    begin
      for Price of Price_List loop -- all runners in race
        if Price.Pricets.Day /= Current_Date.Day then
          Log(Me, "start Treat date: " & Current_Date.String_Date_Iso );
          Current_Date := Price.Pricets;
        end if;

        if Layprice_Low <= Price.Layprice and then Price.Layprice <= Layprice_High then
          T.Start;
          --for Dtg in Delta_Tics_Type'Range loop
          for Dtg in Ia_Min_Tic .. Ia_Max_Tic loop
            Log(Me, "start Treat price: " & Dtg'Img  & " " & Price.To_String );
            Run(Price_Data => Price,
                Delta_Tics => Dtg,
                Lay_Size   => Lay_Size);
            Log(Me, "stop  Treat price: " & Dtg'Img  & " " & Price.To_String );
          end loop;
          T.Commit;
        end if;
      end loop;
    end;
  end;

  Log(Me, "Close Db");
  Sql.Close_Session;
  Logging.Close;

exception
    --    when Lock.Lock_Error =>
    --      Log(Me, "lock error, exit");
    --      Logging.Close;

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
end Lay_All_Watch_Back;
