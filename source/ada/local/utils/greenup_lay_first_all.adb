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
--with Rpc;
--with Lock ;
with Ini;
with Logging; use Logging;
with Bot_Svn_Info;
with Utils; use Utils;
--with Table_Abets;
with Tics;
with Sim;

with Prices;
with Price_Histories;
with Markets;
with Runners;
with Bets;

procedure Greenup_Lay_First_All is

  package Ev renames Ada.Environment_Variables;
  --use type Rpc.Result_Type;
  use type Ada.Containers.Count_Type;
  use type Tics.Tics_Type;

  Me              : constant String := "Greenup_Lay_First_All.";

  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Cmd_Line        : Command_Line_Configuration;
  -------------------------------------------------------------
  type Bet_Type is record
    Laybet    : Bets.Bet_Type;
    Backbet   : Bets.Bet_Type;
  end record;
  -- use type Table_Abets.Data_Type;
  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_Type);
  subtype Delta_Tics_Type is Tics.Tics_Type;

  Global_Overshoot : Fixed_Type := 1.2;


  -----------------------------------------------------------------
  procedure Check_Bet ( R : in Runners.Runner_Type;
                        B : in out Bets.Bet_Type) is
  begin
    if B.Status(1) = 'M' then
      if B.Side(1..4) = "BACK" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := True;
          B.Profit := B.Size * (B.Pricematched - 1.0);
        elsif R.Status(1..5) = "LOSER" then
          B.Betwon := False;
          B.Profit := -B.Size;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
      elsif B.Side(1..3) = "LAY" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := False;
          B.Profit := -B.Size * (B.Pricematched - 1.0);
        elsif R.Status(1..5) = "LOSER" then
          B.Profit := B.Size;
          B.Betwon := True;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
      end if;
    end if;
    B.Insert;
    Log("Check_Bet", "inserted " & B.To_String);
  end Check_Bet;

  -----------------------------------------------------------------

  procedure Run(Price_Data             : in Prices.Price_Type;
                Min_Tic                : in Delta_Tics_Type;
                Max_Tic                : in Delta_Tics_Type;
                Lay_Size               : in Bet_Size_Type;
                Price_During_Race_List : in Price_Histories.Lists.List) is

    Market                 : Markets.Market_Type;
    Eos                    : Boolean := False;
    Runner                 : Runners.Runner_Type;--table_Arunners.Data_Type;
    Tic_Lay                : Tics.Tics_Type := 1;
    Bet                    : Bet_Type;
    Bet_Name               : String_Object;
    Back_Size              : Bet_Size_Type := 0.0;
    Bn                     : Betname_Type := (others => ' ');
    Reference              : String(1..30) := (others  => ' ');
  begin
    Log(Me & "Run", "start");

    Market.Marketid := Price_Data.Marketid;
    Market.Read(Eos);
    if Eos then
      Log(Me & "Run", "no market found");
      return;
    end if;

    if not Market.Marketname_Ok2 then
      Log(Me & "Run", "bad market found - Name not Ok "  & Trim(Market.Marketname,Both));
      return;
    end if;

    if Price_Data.Layprice < 10.0 then
      Move("00" & F8_Image(Price_Data.Layprice), Reference);
    elsif Price_Data.Layprice < 100.0 then
      Move("0" & F8_Image(Price_Data.Layprice),  Reference);
    elsif Price_Data.Layprice < 1000.0 then
      Move(F8_Image(Price_Data.Layprice)      ,  Reference);
    end if;


    -- Log(Me & "Run", "Treat market: " &  Price_Data.Marketid);
    -- Log(Me & "Run", "Market: " & Market.To_String);

    Runner.Marketid := Price_Data.Marketid;
    Runner.Selectionid := Price_Data.Selectionid;
    Runner.Read(Eos);

    if Price_During_Race_List.Length > 10 then -- we are in a member of OKMARKETS
      if Eos then
        Log(Me & "Run", "no runner found " & Runner.To_String);
        return;
      end if;

      if Runner.Status(1..7) = "REMOVED" then
        Log(Me & "Run", "runner removed " & Runner.To_String);
        return;
      end if;


      Tic_Lay := Tics.Get_Tic_Index(Price_Data.Layprice);
      -- Log(Me & "Run", "tic_lay " & Tic_Lay'img & " " & Price_Data.To_String);

      for Tic in Min_Tic .. Max_Tic loop
        declare
          B_Price : Fixed_Type := Tics.Get_Tic_Price(Tic_Lay + Tic);
        begin
          Back_Size := Lay_Size * Bet_Size_Type(Price_Data.Layprice/B_Price);
        end;

        Bet_Name.Set("OK_FILTER2_GREENUP3_LAY_FIRST_TICS_" & Trim(Reference,Both) & "_" &  Trim(Tic'Img,Both) );

        Move(Bet_Name.Fix_String,Bn);
        Sim.Place_Bet(Bet_Name         => Bn,
                      Market_Id        => Market.Marketid,
                      Side             => Lay,
                      Runner_Name      => Runner.Runnernamestripped,
                      Selection_Id     => Price_Data.Selectionid,
                      Size             => Lay_Size,
                      Price            => Bet_Price_Type(Price_Data.Layprice),
                      Bet_Persistence  => Persist,
                      Bet_Placed       => Price_Data.Pricets,
                      Bet              => Bet.Laybet ) ;

        Move("M",Bet.Laybet.Status);
        Bet.Laybet.Pricematched := Price_Data.Layprice;
        Move(F8_Image(Price_Data.Layprice),Bet.Laybet.Reference);
        Bet.Laybet.Powerdays := Integer_4(Tic);
        Check_Bet(Runner, Bet.Laybet);


        Sim.Place_Bet(Bet_Name         => Bn,
                      Market_Id        => Market.Marketid,
                      Side             => Back,
                      Runner_Name      => Runner.Runnernamestripped,
                      Selection_Id     => Price_Data.Selectionid,
                      Size             => Back_Size,
                      Price            => Bet_Price_Type(Tics.Get_Tic_Price(Tic_Lay + Tic)),
                      Bet_Persistence  => Persist,
                      Bet_Placed       => Price_Data.Pricets,
                      Bet              => Bet.Backbet ) ;
        Move("U",Bet.Backbet.Status);
        Bet.Backbet.Powerdays := Integer_4(Tic);
        Move(F8_Image(Price_Data.Layprice),Bet.Backbet.Reference);

        -- see if we meet stop_loss or greenup
        for Race_Data of Price_During_Race_List loop
          if Race_Data.Backprice > Fixed_Type(0.0)
            -- and then Race_Data.Layprice > Fixed_Type(0.0)
            and then Race_Data.Backprice < Fixed_Type(1000.0)
            -- and then Race_Data.Layprice < Fixed_Type(1000.0)
          then   -- must be valid
            if Race_Data.Pricets + (0,0,0,1,0) >= Price_Data.Pricets then
              if Race_Data.Backprice >= Bet.Backbet.Price  -- a match
                and then Race_Data.Backprice <= Fixed_Type(Global_Overshoot * Bet.Backbet.Price) -- but only if it does not 'overshoot' too much
              then -- a match
                Move("M",Bet.Backbet.Status);
                Bet.Backbet.Pricematched := Race_Data.Backprice;
                exit;
                --                elsif Race_Data.Backprice <= Bet.Stop_Loss_Backbet.Price then -- a match
                --                  Move("M",Bet.Stop_Loss_Backbet.Status);
                --                  exit;
              end if;
            end if;
          end if;
        end loop;
        Check_Bet(Runner, Bet.Backbet);
      end loop;


    else
      Log(Me & "not enough data for runner" & Price_During_Race_List.Length'Img, Price_Data.To_String);
    end if;
  end Run;
  ---------------------------------------------------------------------
  --use type Sql.Transaction_Status_Type;
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
  Log("1");

  Define_Switch
    (Cmd_Line,
     Sa_Min_Layprice'Access,
     Long_Switch => "--min_lay=",
     Help        => "Min layprice");
  Log("2");

  Define_Switch
    (Cmd_Line,
     Sa_Max_Layprice'Access,
     Long_Switch => "--max_lay=",
     Help        => "Max layprice");
  Log("3");

  Define_Switch
    (Cmd_Line,
     Ia_Min_Tic'Access,
     Long_Switch => "--min_tic=",
     Help        => "min tic");
  Log("4");

  Define_Switch
    (Cmd_Line,
     Ia_Max_Tic'Access,
     Long_Switch => "--max_tic=",
     Help        => "max tic");
  Log("5");

  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");
  Log("6");

  Define_Switch
    (Cmd_Line,
     Sa_Par_Inifile'Access,
     Long_Switch => "--inifile=",
     Help        => "use alternative inifile");
  Log("7");

  Getopt (Cmd_Line);  -- process the command line
  Log("8");


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
    Ev.Set("BOT_NAME","greenup_lfa");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
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

  Layprice_High := Fixed_Type'Value(Sa_Max_Layprice.all);
  Layprice_Low  := Fixed_Type'Value(Sa_Min_Layprice.all);

  declare
    Stm                    : Sql.Statement_Type;
    T                      : Sql.Transaction_Type;
    Price_List             : Prices.Lists.List;
    Price_During_Race_List : Price_Histories.Lists.List;
    Start                  : Calendar2.Time_Type := (2015,12,31,0,0,0,0);
  begin
    T.Start;
    Stm.Prepare(
                "select P.* " &
                  "from APRICES P, AMARKETS M, AEVENTS E, ARUNNERS R, OKMARKETS O " &
                  "where E.EVENTID=M.EVENTID " &
                  "and M.MARKETTYPE = 'WIN' " &
                  "and E.COUNTRYCODE in ('GB','IE') " &
                  "and O.MARKETID = M.MARKETID " &
                  "and P.MARKETID = M.MARKETID " &
                  "and R.MARKETID = P.MARKETID " &
                  "and R.SELECTIONID = P.SELECTIONID " &
                  "and P.STATUS <> 'REMOVED' " &
                  "and E.EVENTTYPEID =  7 " &
                  "and M.NUMRUNNERS >=  8 " &
                  "and M.NUMRUNNERS <= 16 " &
                  "and P.LAYPRICE <= :MAX_LAYPRICE " &
                  "and P.LAYPRICE >= :MIN_LAYPRICE " &
                  "and M.STARTTS  >= :STARTDATE " &
                  "order by M.STARTTS, P.MARKETID, P.SELECTIONID ");
    Stm.Set("MAX_LAYPRICE",Layprice_High);
    Stm.Set("MIN_LAYPRICE",Layprice_Low);
    Stm.Set("STARTDATE",Start);
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
          Price_During_Race_List.Clear;
          Sim.Read_Marketid_Selectionid(Marketid    => Price.Marketid,
                                        Selectionid => Price.Selectionid,
                                        Animal      => Horse,
                                        List        => Price_During_Race_List) ;

          Log(Me, "start Treat price: "  & Price.To_String );
          Run(Price_Data             => Price,
              Min_Tic                => Delta_Tics_Type(Ia_Min_Tic),
              Max_Tic                => Delta_Tics_Type(Ia_Max_Tic),
              Lay_Size               => Lay_Size,
              Price_During_Race_List => Price_During_Race_List);
          Log(Me, "stop Treat price: " & Price.To_String );
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
end Greenup_Lay_First_All;
