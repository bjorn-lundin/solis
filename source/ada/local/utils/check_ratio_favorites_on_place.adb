with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers;
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
with Tics;
with Sim;

with Prices;
with Price_Histories;
with Markets;
with Runners;
with Bets;

procedure Check_Ratio_Favorites_on_Place is

  package Ev renames Ada.Environment_Variables;
  use type Rpc.Result_Type;
  use type Ada.Containers.Count_Type;

  Me : constant String := "Favs_on_Place.";
  Ba_Check_For_Panic : aliased Boolean := False;

  -----------------------------------------------------------------

  function Name(Start_Price, Bet_Price : Fixed_Type) return String is
    Prefix : String := "G3_D";
  begin
    if Start_Price < 10.0 then
      if Bet_Price < 10.0 then
        return Prefix & "_00" & F8_Image(Start_Price) & "_00" & F8_Image(Bet_Price) & "_" & Ba_Check_For_Panic'Img(1);
      elsif Bet_Price < 100.0 then
        return Prefix & "_00" & F8_Image(Start_Price) & "_0" & F8_Image(Bet_Price) & "_" & Ba_Check_For_Panic'Img(1);
      else
        return Prefix & "_00" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price) & "_" & Ba_Check_For_Panic'Img(1);
      end if;

    elsif Start_Price < 100.0 then
      if Bet_Price < 10.0 then
        return Prefix & "_0" & F8_Image(Start_Price) & "_00" & F8_Image(Bet_Price) & "_" & Ba_Check_For_Panic'Img(1);
      elsif Bet_Price < 100.0 then
        return Prefix & "_0" & F8_Image(Start_Price) & "_0" & F8_Image(Bet_Price) & "_" & Ba_Check_For_Panic'Img(1);
      else
        return Prefix & "_0" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price) & "_" & Ba_Check_For_Panic'Img(1);
      end if;

    elsif Start_Price < 1000.0 then
      if Bet_Price < 10.0 then
        return Prefix & "_" & F8_Image(Start_Price) & "_00" & F8_Image(Bet_Price) & "_" & Ba_Check_For_Panic'Img(1);
      elsif Bet_Price < 100.0 then
        return Prefix & "_" & F8_Image(Start_Price) & "_0" & F8_Image(Bet_Price) & "_" & Ba_Check_For_Panic'Img(1);
      else
        return Prefix & "_" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price) & "_" & Ba_Check_For_Panic'Img(1);
      end if;
    end if;

    return "WTF-" & Prefix & " _" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price) & "_" & Ba_Check_For_Panic'Img(1);
  end Name;
  -----------------------------------------------------------------
  function Price_Is_Ok(Price : Price_Histories.Price_History_Type) return Boolean is
  begin
    return
      Price.Backprice  > Fixed_Type(1.0)    and then -- must be valid
      Price.Layprice   > Fixed_Type(1.0)    and then -- must be valid
      Price.Backprice  < Fixed_Type(1000.1) and then -- must be valid
      Price.Layprice   < Fixed_Type(1000.1) and then -- must be valid
      Fixed_Type(Price.Layprice / Price.Backprice) <= Fixed_Type(1.15); -- max 15% diff
  end Price_Is_Ok;

  -----------------------------------------------------------------
  procedure Run(Price_Data : in Prices.Price_Type;
                Backsize   : in Bet_Size_Type;
                Laysize    : in Bet_Size_Type) is

    subtype Sim_Type is Bot_Types.Bet_Market_Type range Winner .. Winner;
    Market                 : array (Sim_Type ) of Markets.Market_Type;
    Eos                    : array (Sim_Type ) of Boolean := (others => False);
    Price_During_Race_List : array (Sim_Type ) of Price_Histories.Lists.List;
    Runner                 : array (Sim_Type ) of Runners.Runner_Type;
    History_Exists         : array (Sim_Type ) of Boolean := (others => False);
    Bet                    : array (Sim_Type, Bot_Types.Bet_Side_Type ) of Bets.Bet_Type;
    Match_Time             : array (Bot_Types.Bet_Side_Type ) of Calendar2.Time_Type;
    Panicbet               : Bets.Bet_Type;
    Panicbet_Placed_Ts     : Calendar2.Time_Type;

    Back_Bet_Name          : String_Object;
    Ref                    : String_Object;

    Bn                     : Betname_Type := (others => ' ');
    Bet_Price              : Fixed_Type := 0.0;
    Panic_Price            : Fixed_Type := 0.0;
    Back_Tick              : Tics.Tics_Type := 1;
    --Found_Place_Market     : Boolean := False;
    use type Tics.Tics_Type ;
    subtype Betting_Limit_Type is Tics.Tics_Type range 1 .. 20;
  begin
    Log(Me & "Run", "start");

    -- Log(Me & "Run", "Treat market: " &  Price_Data.Marketid);
    Market(Winner).Marketid := Price_Data.Marketid;
    Market(Winner).Read(Eos(Winner));
    if Eos(Winner) then
      Log(Me & "Run", "no market found");
      return;
    end if;

    -- Log(Me & "Run", "Market: " & Market.To_String);
    Sim.Read_Marketid_Selectionid(Marketid    => Market(Winner).Marketid,
                                  Selectionid => Price_Data.Selectionid,
                                  Animal      => Horse,
                                  List        => Price_During_Race_List(Winner)) ;


    History_Exists(Winner) := Price_During_Race_List(Winner).Length > 80;
    if History_Exists(Winner) then
      Runner(Winner).Marketid := Market(Winner).Marketid;
      Runner(Winner).Selectionid := Price_Data.Selectionid;
      Runner(Winner).Read(Eos(Winner));
      if Eos(Winner) then
        Log(Me & "Run", "no Winner runner found");
        History_Exists(Winner) := False;
      end if;
    end if;

    if History_Exists(Winner) then
      if Runner(Winner).Status(1..7) = "REMOVED" then
        Log(Me & "Run", "runner removed " & Runner(Winner).To_String);
        return;
      end if;

      Tic_Loop  : for Bet_Tic in Betting_Limit_Type'Range loop
        begin
          Back_Tick := Tics.Get_Tic_Index(Price_Data.Backprice);
          Bet_Price := Tics.Get_Tic_Price(Back_Tick - Bet_Tic);
          Panic_Price := Tics.Get_Tic_Price(Back_Tick - 20);
        exception
          when Constraint_Error =>  exit Tic_Loop;
        end;
        --  Back_Size := Lay_Size * Bet_Size_Type(Price_Data.Layprice/Bet_Price);

        Back_Bet_Name.Set(Name(Start_Price => Price_Data.Backprice, Bet_Price => Bet_Price));
        declare
          type Bet_State_Type is (Started, Backbet_Placed, Laybet_Placed, Laybet_Matched, Panicbet_Placed);
          Bet_State  : Bet_State_Type := Started;
          Not_Set    : Calendar2.Time_Type := Calendar2.Time_Type_Last - (1,1,1,1,1);
          Tmp        : String := Back_Bet_Name.Fix_String;
        begin
          Ref.Set( Tmp(10 .. Tmp'Last) & "_TICS_" & F8_Image(Fixed_Type(Bet_Tic)));
          Match_Time(Back) := Not_Set;
          Match_Time(Lay) := Not_Set;
          Panicbet_Placed_Ts := Not_Set;
          Bet(Winner,Back) := Bets.Empty_Data;
          Bet(Winner,Lay) := Bets.Empty_Data;
          Move("WIN_" & Back_Bet_Name.Fix_String,Bn);
          Bet_State := Started;

          Race_Win : for Race_Data of Price_During_Race_List(Winner) loop
            if Price_Is_Ok(Race_Data) then
              case Bet_State is
                when Started => --start with backing with PRICE_DATA
                  Sim.Place_Bet(Bet_Name         => Bn,
                                Market_Id        => Market(Winner).Marketid,
                                Side             => Back,
                                Runner_Name      => Runner(Winner).Runnernamestripped,
                                Selection_Id     => Price_Data.Selectionid,
                                Size             => Backsize,
                                Price            => Bet_Price_Type(Race_Data.Backprice),
                                Bet_Persistence  => Persist,
                                Bet_Placed       => Price_Data.Pricets,
                                Bet              => Bet(Winner,Back) ) ;
                  Match_Time(Back) := Race_Data.Pricets;
                  -- placed just before start -> always matched
                  Move("M",Bet(Winner,Back).Status);
                  Bet(Winner,Back).Check_Outcome(Runner(Winner));
                  Move(Ref.Fix_String,Bet(Winner,Back).Reference);
                  Bet(Winner,Back).Insert;
                  Bet_State := Backbet_Placed;

                when Backbet_Placed =>
                  if Race_Data.Layprice <= Bet_Price then
                    Sim.Place_Bet(Bet_Name         => Bn,
                                  Market_Id        => Market(Winner).Marketid,
                                  Side             => Lay,
                                  Runner_Name      => Runner(Winner).Runnernamestripped,
                                  Selection_Id     => Price_Data.Selectionid,
                                  Size             => Laysize,
                                  Price            => Bet_Price_Type(Price_Data.Layprice),
                                  Bet_Persistence  => Persist,
                                  Bet_Placed       => Race_Data.Pricets,
                                  Bet              => Bet(Winner,Lay) ) ;
                    Match_Time(Lay) := Race_Data.Pricets;
                    Bet_State := Laybet_Placed;
                  end if;

                when Laybet_Placed =>
                  if Race_Data.Pricets >= Match_Time(Lay) + (0,0,0,1,0) then -- wait 1 sec
                    if Race_Data.Layprice <= Bet_Price then     -- must be same or lower for a lay match
                      Move("M",Bet(Winner,Lay).Status);
                      Bet_State := Laybet_Matched;
                    elsif Ba_Check_For_Panic and Then
                          bet_State = Laybet_Placed and then  --check for panic
                          Race_Data.Backprice >= Panic_Price then
                      Sim.Place_Bet(Bet_Name         => Bn,
                                    Market_Id        => Market(Winner).Marketid,
                                    Side             => Lay,
                                    Runner_Name      => Runner(Winner).Runnernamestripped,
                                    Selection_Id     => Price_Data.Selectionid,
                                    Size             => Laysize,
                                    Price            => Bet_Price_Type(100.0),
                                    Bet_Persistence  => Persist,
                                    Bet_Placed       => Race_Data.Pricets,
                                    Bet              => Panicbet ) ;
                      Bet_State := Panicbet_Placed;
                      Panicbet_Placed_Ts := Race_Data.Pricets;

                    end if;
                  end if;

                when Laybet_Matched =>
                  Bet(Winner,Lay).Check_Outcome(Runner(Winner));
                  Move(Ref.Fix_String,Bet(Winner,Lay).Reference);
                  Bet(Winner,Lay).Insert;
                  exit Race_Win;

                when Panicbet_Placed =>
                  if Race_Data.Pricets >= Panicbet_Placed_Ts + (0,0,0,1,0) then -- wait 1 sec
                    Panicbet.Check_Outcome(Runner(Winner));
                    Move(Ref.Fix_String,Panicbet.Reference);
                    Panicbet.Powerdays := 999; -- panic
                    Panicbet.Insert;
                    exit Race_Win;
                  end if;

              end case;
            end if;  -- price is ok
          end loop Race_Win;
        end;
      end loop Tic_Loop ;

    else
      Log(Me & "not enough data for runner" & Price_During_Race_List(Winner).Length'Img, Price_Data.To_String);
    end if;
  exception
    when Sql.Duplicate_Index =>
      Log("Winner/Back " & Bet(Winner,Back).To_String);
      Log("Winner/Lay  " & Bet(Winner,Lay).To_String);
      raise;
  end Run;
  pragma Unreferenced(Run);
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
  ------------------------------ main start -------------------------------------
  Current_Date : Calendar2.Time_Type := Calendar2.Clock;

  Sa_Min_Price     : aliased Gnat.Strings.String_Access;
  Sa_Max_Price     : aliased Gnat.Strings.String_Access;
  Sa_Logfilename   : aliased Gnat.Strings.String_Access;

  Sa_Par_Inifile   : aliased Gnat.Strings.String_Access;
  Cmd_Line         : Command_Line_Configuration;
  --Global_Backsize  : constant Bet_Size_Type := 100.0;
  --Global_Laysize   : constant Bet_Size_Type := 100.0;
  Current_Marketid : Marketid_Type := (others => ' ');

  Price_High    : Fixed_Type := 1000.0;
  Price_Low     : Fixed_Type := 0.0;

  P_Use : Prices.Price_Type;
  P_Best : array (1..3) of prices.Price_Type;
  Runner : Runners.Runner_Type;
  Eos_Runner : Boolean := False;


  subtype Sim_Type is Bot_Types.Bet_Market_Type range Place .. Winner;
  Market                 : array (Sim_Type ) of Markets.Market_Type;
  Eos                    : array (Sim_Type ) of Boolean := (others => False);


  type Stat_Record is record
    Num_Races      : Integer_4 := 0;
    Num_Won_Races  : Integer_4 := 0;
    Avg_Back_Price : Fixed_Type := 0.0;
    Avg_Lay_Price  : Fixed_Type := 0.0;
  end record;

  Stats : array (Sim_Type ) of Stat_Record;



begin
  Define_Switch
    (Cmd_Line,
     Sa_Min_Price'Access,
     Long_Switch => "--min=",
     Help        => "Min price");

  Define_Switch
    (Cmd_Line,
     Sa_Max_Price'Access,
     Long_Switch => "--max=",
     Help        => "Max price");

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
     Ba_Check_For_Panic'Access,
     Long_Switch => "--panic",
     Help        => "withdraw if runner semms to loose");

  Getopt (Cmd_Line);  -- process the command line

  if Sa_Min_Price.all = "" then
    raise Constraint_Error with "no min-price set";
  elsif Sa_Max_Price.all = "" then
    raise Constraint_Error with "no max-price set";
  elsif Sa_Logfilename.all = "" then
    raise Constraint_Error with "no log file name set";
  end if;

  Log(Sa_Min_Price.all & "' '" & Sa_Max_Price.all & "'");

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","bet_during_race_3");
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

  Price_High := Fixed_Type'Value(Sa_Max_Price.all);
  Price_Low  := Fixed_Type'Value(Sa_Min_Price.all);

  declare
    Stm         : Sql.Statement_Type;
    T           : Sql.Transaction_Type;
    Price_List  : Prices.Lists.List;
    First_Run   : Boolean := True;
    Place_Price : Prices.Price_Type;
  begin
    T.Start;
    Stm.Prepare(
                "select P.* " &
                  "from APRICES P, AMARKETS M, AEVENTS E, ARUNNERS R " &
                  "where E.EVENTID=M.EVENTID " &
                  "and P.MARKETID = M.MARKETID " &
                  "and P.MARKETID = R.MARKETID " &
                  "and P.SELECTIONID = R.SELECTIONID " &
                  "and R.STATUS <> 'REMOVED' " &
                  "and M.MARKETTYPE = 'WIN' " &
                  "and E.COUNTRYCODE in ('GB','IE') " &
                  "and E.EVENTTYPEID = 7 " &
                  "and P.BACKPRICE <= :MAX_PRICE " &
                  "and P.BACKPRICE >= :MIN_PRICE " &
                  "order by M.STARTTS, P.MARKETID, P.SELECTIONID ");
    Stm.Set("MAX_PRICE",Price_High);
    Stm.Set("MIN_PRICE",Price_Low);
    Prices.Read_List(Stm, Price_List);
    T.Commit;

    begin
      P_Use.Backprice := 1000.1;
      for I in P_Best'Range loop
        P_Best(I).Backprice := 1000.1;
      end loop;
      T.Start;

      for Price of Price_List loop -- all runners in race

        if Price.Pricets.Day /= Current_Date.Day then
          Log(Me, "start treat date: " & Current_Date.String_Date_Iso );
          Current_Date := Price.Pricets;
        end if;


        if Current_Marketid /= Price.Marketid and then not First_Run then --new market
          P_Use := P_Best(3);
          Stats(Winner).Num_Races := Stats(Winner).Num_Races + 1;

          Market(Winner).Marketid := P_Use.Marketid;
          Market(Winner).Read(Eos(Winner));
          if not Eos(Winner) then
            Runner.Marketid := P_Use.Marketid;
            Runner.Selectionid := P_Use.Selectionid;
            Runner.Read(Eos_Runner);
            if not Eos_Runner then
              if Runner.Is_Winner then
                Stats(Winner).Num_Won_Races := Stats(Winner).Num_Won_Races + 1;
                Stats(Winner).Avg_Back_Price := ((Stats(Winner).Avg_Back_Price * Fixed_Type(Stats(Winner).Num_Races )) + Price.Backprice) / Fixed_Type(Stats(Winner).Num_Races +1);
                Stats(Winner).Avg_Lay_Price  := ((Stats(Winner).Avg_Lay_Price  * Fixed_Type(Stats(Winner).Num_Races )) + Price.Layprice)  / Fixed_Type(Stats(Winner).Num_Races +1);
              end if;
            end if;


            --check the place stuff
            Market(Winner).Corresponding_Place_Market(Place_Market => Market(Place),Found => Eos(Place));
            Eos(Place) := not Eos(Place); -- we get found - not 'not found'
            if not Eos(Place) then
              Stats(Place).Num_Races := Stats(Place).Num_Races + 1;
              Runner.Marketid := Market(Place).Marketid;
              Runner.Selectionid := P_Use.Selectionid;
              Runner.Read(Eos_Runner);
              if not Eos_Runner then
                if Runner.Is_Winner then
                  Stats(Place).Num_Won_Races := Stats(Place).Num_Won_Races + 1;
                end if;
              end if;

              Place_Price.Marketid := Market(Place).Marketid;
              Place_Price.Selectionid := P_Use.Selectionid;
              Place_Price.Read(Eos_Runner);
              if not Eos_Runner then
                if Runner.Is_Winner then
                  Stats(Place).Avg_Back_Price := ((Stats(Place).Avg_Back_Price * Fixed_Type(Stats(Place).Num_Races )) + Place_Price.Backprice) / Fixed_Type(Stats(Place).Num_Races +1);
                  Stats(Place).Avg_Lay_Price  := ((Stats(Place).Avg_Lay_Price  * Fixed_Type(Stats(Place).Num_Races )) + Place_Price.Layprice)  / Fixed_Type(Stats(Place).Num_Races +1);
                end if;
              end if;

            end if;
          end if;
          P_Use.Backprice := 1000.1;
          for I in P_Best'Range loop
            P_Best(I).Backprice := 1000.1;
          end loop;
        end if;

        if Price.Backprice < P_Best(1).Backprice then
          P_Best(1) := Price;
        elsif Price.Backprice < P_Best(2).Backprice then
          P_Best(2) := Price;
        elsif price.Backprice < P_Best(3).Backprice then
          P_Best(3) := Price;
        end if;


        Current_Marketid := Price.Marketid;
        First_Run := False;
      end loop;
      T.Commit;
    end;
  end;

  for I in Sim_Type'Range loop
    Log("Main", "----------" & I'Img & "----------------");
    Log("Main", "Stats.Num_Races=      " & Stats(i).Num_Races'Img);
    Log("Main", "Stats.Num_Won_Races=  " & Stats(i).Num_Won_Races'Img);
    Log("Main", "Stats.Avg_Back_Price= " & F8_Image(Stats(i).Avg_Back_Price));
    Log("Main", "Stats.Avg_Lay_Price=  " & F8_Image(Stats(i).Avg_Lay_Price));
    Log("Main", "");
  end loop;

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
end Check_Ratio_Favorites_on_Place;
