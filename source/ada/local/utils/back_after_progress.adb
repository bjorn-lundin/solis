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

procedure Back_After_Progress is

  package Ev renames Ada.Environment_Variables;
  use type Rpc.Result_Type;
  use type Ada.Containers.Count_Type;

  Me : constant String := "Back_After_Progress.";

  -----------------------------------------------------------------

  function Name(Start_Price, Bet_Price : Fixed_Type) return String is

  begin
    if Start_Price < 10.0 then
      if Bet_Price < 10.0 then
        return "BACK_AFTER_00" & F8_Image(Start_Price) & "_00" & F8_Image(Bet_Price);
      elsif Bet_Price < 100.0 then
        return "BACK_AFTER_00" & F8_Image(Start_Price) & "_0" & F8_Image(Bet_Price);
      else
        return "BACK_AFTER_00" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price);
      end if;

    elsif Start_Price < 100.0 then
      if Bet_Price < 10.0 then
        return "BACK_AFTER_0" & F8_Image(Start_Price) & "_00" & F8_Image(Bet_Price);
      elsif Bet_Price < 100.0 then
        return "BACK_AFTER_0" & F8_Image(Start_Price) & "_0" & F8_Image(Bet_Price);
      else
        return "BACK_AFTER_0" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price);
      end if;

    elsif Start_Price < 1000.0 then
      if Bet_Price < 10.0 then
        return "BACK_AFTER_" & F8_Image(Start_Price) & "_00" & F8_Image(Bet_Price);
      elsif Bet_Price < 100.0 then
        return "BACK_AFTER_" & F8_Image(Start_Price) & "_0" & F8_Image(Bet_Price);
      else
        return "BACK_AFTER_" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price);
      end if;
    end if;

    return "WTF-BACK_AFTER_" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price);
  end Name;


  procedure Run(Price_Data : in Prices.Price_Type;
                Backsize   : in Bet_Size_Type) is

    Market                 : array (Bot_Types.Bet_Market_Type ) of Markets.Market_Type;
    Eos                    : array (Bot_Types.Bet_Market_Type ) of Boolean := (others => False);
    Price_During_Race_List : array (Bot_Types.Bet_Market_Type ) of Price_Histories.Lists.List;
    Runner                 : array (Bot_Types.Bet_Market_Type ) of Runners.Runner_Type;
    History_Exists         : array (Bot_Types.Bet_Market_Type ) of Boolean := (others => False);
    Bet                    : array (Bot_Types.Bet_Market_Type ) of Bets.Bet_Type;

    Back_Bet_Name          : String_Object;

    Bn                     : Betname_Type := (others => ' ');
    B_Price                : Fixed_Type := 0.0;
    Found_Place_Market     : Boolean := False;
    Back_Tic               : Tics.Tics_Type := 1;
    Start_Tic              : Tics.Tics_Type := 1;
    use type Tics.Tics_Type ;
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


    Market(Winner).Corresponding_Place_Market(Place_Market => Market(Place),Found => Found_Place_Market);

    if Found_Place_Market then
      Sim.Read_Marketid_Selectionid(Marketid    => Market(Place).Marketid,
                                    Selectionid => Price_Data.Selectionid,
                                    Animal      => Horse,
                                    List        => Price_During_Race_List(Place)) ;

      History_Exists(Place) := Price_During_Race_List(Place).Length > 80;

      if History_Exists(Place) then
        Runner(Place).Marketid := Market(Place).Marketid;
        Runner(Place).Selectionid := Price_Data.Selectionid;
        Runner(Place).Read(Eos(Place));
        if Eos(Place) then
          Log(Me & "Run", "no place runner found");
          History_Exists(Place) := False;
        end if;
      end if;
    else
      Log(Me & "Run", "no place market found");
      -- return;
    end if;

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

       Start_Tic := Tics.Get_Tic_Index(Price_Data.Backprice);

      for Delta_Tics in Tics.Tics_Type'Range loop
        Back_Tic := Start_Tic - Delta_Tics;
        exit when Back_Tic = 1;
        B_Price := Tics.Get_Tic_Price(Back_Tic);
        --  Back_Size := Lay_Size * Bet_Size_Type(Price_Data.Layprice/B_Price);

        Back_Bet_Name.Set(Name(Start_Price => Price_Data.Backprice, Bet_Price => B_Price));

        declare
          Not_Set    : Calendar2.Time_Type := Calendar2.Time_Type_Last - (1,1,1,1,1);
          Match_Time : Calendar2.Time_Type := Not_Set;
        begin
          Race_Win : for Race_Data of Price_During_Race_List(Winner) loop
            if Race_Data.Backprice > Fixed_Type(1.0)   and then -- must be valid
              Race_Data.Layprice   > Fixed_Type(1.0)    and then -- must be valid
              Race_Data.Backprice  < Fixed_Type(1000.1) and then -- must be valid
              Race_Data.Layprice   < Fixed_Type(1000.1) then     -- must be valid

              if Race_Data.Pricets  >= Price_Data.Pricets and then
                Race_Data.Backprice <= B_Price             and then
                Match_Time           = Not_Set             then -- first time condition true

                Match_Time := Race_Data.Pricets;

              elsif  Race_Data.Pricets >= Match_Time + (0,0,0,1,0) then -- wait 1 sec
                if Race_Data.Backprice >= B_Price then     -- must be same or better for match
                  Move("WIN_" & Back_Bet_Name.Fix_String,Bn);
                  Sim.Place_Bet(Bet_Name         => Bn,
                                Market_Id        => Market(Winner).Marketid,
                                Side             => Back,
                                Runner_Name      => Runner(Winner).Runnernamestripped,
                                Selection_Id     => Price_Data.Selectionid,
                                Size             => Backsize,
                                Price            => Bet_Price_Type(Race_Data.Backprice),
                                Bet_Persistence  => Persist,
                                Bet_Placed       => Match_Time,
                                Bet              => Bet(Winner) ) ;
                  Move("M",Bet(Winner).Status);
                  Bet(Winner).Check_Outcome(Runner(Winner));
                  Bet(Winner).Insert;
                end if;
                exit Race_Win;  -- match directly - gets 1 chance only for matching price after the 1s delay is passed
              end if;
            end if;
          end loop Race_Win;

          Race_Plc : for Race_Data of Price_During_Race_List(Place) loop
            exit Race_Plc when not History_Exists(Place);

            if Race_Data.Backprice > Fixed_Type(1.0)   and then -- must be valid
              Race_Data.Layprice  > Fixed_Type(1.0)    and then -- must be valid
              Race_Data.Backprice < Fixed_Type(1000.1) and then -- must be valid
              Race_Data.Layprice  < Fixed_Type(1000.1) then     -- must be valid

              if Race_Data.Pricets >= Match_Time + (0,0,0,1,0) then

                -- take first bet we can get
                Move("PLC_" & Back_Bet_Name.Fix_String,Bn);
                Sim.Place_Bet(Bet_Name         => Bn,
                              Market_Id        => Market(Place).Marketid,
                              Side             => Back,
                              Runner_Name      => Runner(Place).Runnernamestripped,
                              Selection_Id     => Price_Data.Selectionid,
                              Size             => Backsize,
                              Price            => Bet_Price_Type(Race_Data.Backprice),
                              Bet_Persistence  => Persist,
                              Bet_Placed       => Match_Time,
                              Bet              => Bet(Place) ) ;
                Move("M",Bet(Place).Status);
                Bet(Place).Check_Outcome(Runner(Place));
                Bet(Place).Insert;
                exit Race_Plc;  -- match directly - one chance only after 1s delay is passed - no special odds is needed
              end if;
            end if;
          end loop Race_Plc;
        end;
      end loop; -- delta_tics_type

    else
      Log(Me & "not enough data for runner" & Price_During_Race_List(Winner).Length'Img, Price_Data.To_String);
    end if;
  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
  ------------------------------ main start -------------------------------------
  Current_Date : Calendar2.Time_Type := Calendar2.Clock;

  Sa_Min_Backprice : aliased Gnat.Strings.String_Access;
  Sa_Max_Backprice : aliased Gnat.Strings.String_Access;
  Sa_Logfilename   : aliased Gnat.Strings.String_Access;

  Sa_Par_Inifile   : aliased Gnat.Strings.String_Access;
  Cmd_Line         : Command_Line_Configuration;
  Global_Backsize  : constant Bet_Size_Type := 100.0;

  Backprice_High   : Fixed_Type := 0.0;
  Backprice_Low    : Fixed_Type := 0.0;

begin

  Define_Switch
    (Cmd_Line,
     Sa_Min_Backprice'Access,
     Long_Switch => "--min_back=",
     Help        => "Min backprice");

  Define_Switch
    (Cmd_Line,
     Sa_Max_Backprice'Access,
     Long_Switch => "--max_back=",
     Help        => "Min Backprice");

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


  Getopt (Cmd_Line);  -- process the command line

  if Sa_Min_Backprice.all = "" then
    raise Constraint_Error with "no min-back-price set";
  elsif Sa_Max_Backprice.all = "" then
    raise Constraint_Error with "no max-back-price set";
  elsif Sa_Logfilename.all = "" then
    raise Constraint_Error with "no log file name set";
  end if;

  Log(Sa_Min_Backprice.all & "' '" & Sa_Max_Backprice.all & "'");

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","back_after_progress");
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

  Backprice_High := Fixed_Type'Value(Sa_Max_Backprice.all);
  Backprice_Low  := Fixed_Type'Value(Sa_Min_Backprice.all);

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
                  "and P.BACKPRICE <= :MAX_BACKPRICE " &
                  "and P.BACKPRICE >= :MIN_BACKPRICE " &
                  "order by M.STARTTS, P.MARKETID, P.SELECTIONID ");
    Stm.Set("MAX_BACKPRICE",Backprice_High);
    Stm.Set("MIN_BACKPRICE",Backprice_Low);
    Prices.Read_List(Stm, Price_List);
    T.Commit;

    Log(Backprice_Low'Img & " " & Backprice_High'Img & " " & Price_List.Length'Img);
    begin
      for Price of Price_List loop -- all runners in race
        if Price.Pricets.Day /= Current_Date.Day then
          Log(Me, "start treat date: " & Current_Date.String_Date_Iso );
          Current_Date := Price.Pricets;
        end if;
        T.Start;
        Run(Price_Data => Price, Backsize => Global_Backsize);
        T.Commit;
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
end Back_After_Progress;
