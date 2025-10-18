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

procedure Bet_During_Race_1 is

  package Ev renames Ada.Environment_Variables;
  use type Rpc.Result_Type;
  use type Ada.Containers.Count_Type;

  Me : constant String := "Bet_During_Race_1.";

  -----------------------------------------------------------------

  function Name(Start_Price, Bet_Price : Fixed_Type) return String is
    Prefix : String := "BET_DURING";
  begin
    if Start_Price < 10.0 then
      if Bet_Price < 10.0 then
        return Prefix & "_00" & F8_Image(Start_Price) & "_00" & F8_Image(Bet_Price);
      elsif Bet_Price < 100.0 then
        return Prefix & "_00" & F8_Image(Start_Price) & "_0" & F8_Image(Bet_Price);
      else
        return Prefix & "_00" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price);
      end if;

    elsif Start_Price < 100.0 then
      if Bet_Price < 10.0 then
        return Prefix & "_0" & F8_Image(Start_Price) & "_00" & F8_Image(Bet_Price);
      elsif Bet_Price < 100.0 then
        return Prefix & "_0" & F8_Image(Start_Price) & "_0" & F8_Image(Bet_Price);
      else
        return Prefix & "_0" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price);
      end if;

    elsif Start_Price < 1000.0 then
      if Bet_Price < 10.0 then
        return Prefix & "_" & F8_Image(Start_Price) & "_00" & F8_Image(Bet_Price);
      elsif Bet_Price < 100.0 then
        return Prefix & "_" & F8_Image(Start_Price) & "_0" & F8_Image(Bet_Price);
      else
        return Prefix & "_" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price);
      end if;
    end if;

    return "WTF-" & Prefix & " _" & F8_Image(Start_Price) & "_" & F8_Image(Bet_Price);
  end Name;
  -----------------------------------------------------------------
  function Price_Is_Ok(Price : Price_Histories.Price_History_Type) return Boolean is
  begin
    return
      Price.Backprice  > Fixed_Type(1.0)    and then -- must be valid
      Price.Layprice   > Fixed_Type(1.0)    and then -- must be valid
      Price.Backprice  < Fixed_Type(1000.1) and then -- must be valid
      Price.Layprice   < Fixed_Type(1000.1) ;     -- must be valid
  end Price_Is_Ok;

  -----------------------------------------------------------------
  procedure Run(Price_Data : in Prices.Price_Type;
                Backsize   : in Bet_Size_Type;
                Laysize    : in Bet_Size_Type) is

    Market                 : array (Bot_Types.Bet_Market_Type ) of Markets.Market_Type;
    Eos                    : array (Bot_Types.Bet_Market_Type ) of Boolean := (others => False);
    Price_During_Race_List : array (Bot_Types.Bet_Market_Type ) of Price_Histories.Lists.List;
    Runner                 : array (Bot_Types.Bet_Market_Type ) of Runners.Runner_Type;
    History_Exists         : array (Bot_Types.Bet_Market_Type ) of Boolean := (others => False);
    Bet                    : array (Bot_Types.Bet_Market_Type, Bot_Types.Bet_Side_Type ) of Bets.Bet_Type;
    Match_Time             : array (Bot_Types.Bet_Side_Type ) of Calendar2.Time_Type;

    Back_Bet_Name          : String_Object;

    Bn                     : Betname_Type := (others => ' ');
    Bet_Price              : Fixed_Type := 0.0;
    Found_Place_Market     : Boolean := False;
    use type Tics.Tics_Type ;
    subtype Betting_Limit_Type is Tics.Tics_Type range 1 .. 200;
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
          Found_Place_Market := False;
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

      Tic_Loop  : for Bet_Tic in Betting_Limit_Type'Range loop
        Bet_Price := Tics.Get_Tic_Price(Bet_Tic);
        --  Back_Size := Lay_Size * Bet_Size_Type(Price_Data.Layprice/Bet_Price);

        Back_Bet_Name.Set(Name(Start_Price => Price_Data.Backprice, Bet_Price => Bet_Price));

        declare
          type Bet_State_Type is (Running, Bet_Candidate, Bet_Placed, Bet_Matched);
          Bet_State  : Bet_State_Type := Running;
          Not_Set    : Calendar2.Time_Type := Calendar2.Time_Type_Last - (1,1,1,1,1);
        begin
          Side_Loop_Winner : for Bet_Side in Bet_Side_Type loop
            Match_Time(Back) := Not_Set;
            Match_Time(Lay) := Not_Set;
            Bet(Winner,Bet_Side) := Bets.Empty_Data;
            Bet_State := Running;

            Race_Win : for Race_Data of Price_During_Race_List(Winner) loop
              if Price_Is_Ok(Race_Data) then

                case Bet_State is
                  when Running =>
                    case Bet_Side is
                      when Back =>
                        -- must come from a higher odds
                        if Race_Data.Pricets >= Price_Data.Pricets and then
                          Race_Data.Backprice >= Bet_Price then
                          Bet_State := Bet_Candidate;
                        end if;

                      when Lay =>
                        -- must come from a lower odds
                        if Race_Data.Pricets >= Price_Data.Pricets and then
                          Race_Data.Backprice >= Bet_Price then
                          Bet_State := Bet_Candidate;
                        end if;
                    end case;

                  when Bet_Candidate =>
                    case Bet_Side is
                      when Back =>

                        if Race_Data.Backprice <= Bet_Price then
                          Move("WIN_" & Back_Bet_Name.Fix_String,Bn);
                          Sim.Place_Bet(Bet_Name         => Bn,
                                        Market_Id        => Market(Winner).Marketid,
                                        Side             => Bet_Side,
                                        Runner_Name      => Runner(Winner).Runnernamestripped,
                                        Selection_Id     => Price_Data.Selectionid,
                                        Size             => Backsize,
                                        Price            => Bet_Price_Type(Race_Data.Backprice),
                                        Bet_Persistence  => Persist,
                                        Bet_Placed       => Race_Data.Pricets,
                                        Bet              => Bet(Winner,Bet_Side) ) ;
                          Match_Time(Back) := Race_Data.Pricets;
                          Bet_State := Bet_Placed;
                        end if;

                      when Lay =>

                        if Race_Data.Layprice >= Bet_Price  then
                          Move("WIN_" & Back_Bet_Name.Fix_String,Bn);
                          Sim.Place_Bet(Bet_Name         => Bn,
                                        Market_Id        => Market(Winner).Marketid,
                                        Side             => Bet_Side,
                                        Runner_Name      => Runner(Winner).Runnernamestripped,
                                        Selection_Id     => Price_Data.Selectionid,
                                        Size             => Laysize,
                                        Price            => Bet_Price_Type(Race_Data.Layprice),
                                        Bet_Persistence  => Persist,
                                        Bet_Placed       => Race_Data.Pricets,
                                        Bet              => Bet(Winner,Bet_Side) ) ;
                          Match_Time(Lay) := Race_Data.Pricets;
                          Bet_State := Bet_Placed;
                        end if;
                    end case;

                  when Bet_Placed =>
                    case Bet_Side is
                      when Back =>
                        if Race_Data.Pricets >= Match_Time(Back) + (0,0,0,1,0) and then -- wait 1 sec
                          Race_Data.Backprice >= Bet_Price then     -- must be same or higher for match
                          Move("M",Bet(Winner,Bet_Side).Status);
                          Bet_State := Bet_Matched;
                        end if;
                      when Lay =>
                        if Race_Data.Pricets >= Match_Time(Lay) + (0,0,0,1,0) and then -- wait 1 sec
                          Race_Data.Backprice <= Bet_Price then     -- must be same or lower for match
                          Move("M",Bet(Winner,Bet_Side).Status);
                          Bet_State := Bet_Matched;
                        end if;
                    end case;

                  when Bet_Matched =>
                    Bet(Winner,Bet_Side).Check_Outcome(Runner(Winner));
                    Log("insert Winner/" & Bet_Side'Img & " " & Bet(Winner,Bet_Side).To_String);
                    Bet(Winner,Bet_Side).Insert;
                    exit Race_Win; -- match directly

                end case;
              end if;  -- price is ok
            end loop Race_Win;
          end loop Side_Loop_Winner;

          Bet_State := Running;
          Side_Loop_Plc : for Bet_Side in Bet_Side_Type loop
            Bet(Place,Bet_Side) := Bets.Empty_Data;
            Race_Plc : for Race_Data of Price_During_Race_List(Place) loop
              exit Race_Plc when not History_Exists(Place);

              if Price_Is_Ok(Race_Data)  then     -- must be valid

                case Bet_State is
                  when Running =>
                    Bet_State := Bet_Candidate;

                  when Bet_Candidate => -- must be after WIN bets
                    case Bet_Side is
                      when Back => -- use whatever price we get
                        if Race_Data.Pricets >= Match_Time(Back) then
                          Move("PLC_" & Back_Bet_Name.Fix_String,Bn);
                          Sim.Place_Bet(Bet_Name         => Bn,
                                        Market_Id        => Market(Place).Marketid,
                                        Side             => Bet_Side,
                                        Runner_Name      => Runner(Place).Runnernamestripped,
                                        Selection_Id     => Price_Data.Selectionid,
                                        Size             => Backsize,
                                        Price            => Bet_Price_Type(Race_Data.Backprice),
                                        Bet_Persistence  => Persist,
                                        Bet_Placed       => Race_Data.Pricets,
                                        Bet              => Bet(Place,Bet_Side) ) ;
                          -- Match_Time := Race_Data.Pricets; -- update to reflect new bet time
                          Bet_State := Bet_Placed;
                        end if;

                      when Lay =>

                        if Race_Data.Pricets >= Match_Time(Lay) then
                          Move("PLC_" & Back_Bet_Name.Fix_String,Bn);
                          Sim.Place_Bet(Bet_Name         => Bn,
                                        Market_Id        => Market(Place).Marketid,
                                        Side             => Bet_Side,
                                        Runner_Name      => Runner(Place).Runnernamestripped,
                                        Selection_Id     => Price_Data.Selectionid,
                                        Size             => Laysize,
                                        Price            => Bet_Price_Type(Race_Data.Layprice),
                                        Bet_Persistence  => Persist,
                                        Bet_Placed       => Race_Data.Pricets,
                                        Bet              => Bet(Place,Bet_Side) ) ;
                          -- Match_Time := Race_Data.Pricets;
                          Bet_State := Bet_Placed;
                        end if;
                    end case;

                  when Bet_Placed =>
                    case Bet_Side is
                      when Back =>
                        if Race_Data.Pricets >= Match_Time(Back) + (0,0,0,1,0) and then -- wait 1 sec
                          Race_Data.Backprice >= Bet_Price then     -- must be same or higher for match
                          Move("M",Bet(Place,Bet_Side).Status);
                          Bet_State := Bet_Matched;
                        end if;
                      when Lay =>
                        if Race_Data.Pricets >= Match_Time(Lay) + (0,0,0,1,0) and then -- wait 1 sec
                          Race_Data.Backprice <= Bet_Price then     -- must be same or lower for match
                          Move("M",Bet(Place,Bet_Side).Status);
                          Bet_State := Bet_Matched;
                        end if;
                    end case;

                  when Bet_Matched =>
                    Bet(Place,Bet_Side).Check_Outcome(Runner(Place));
                    Log("insert Place/" & Bet_Side'Img & " " & Bet(Place,Bet_Side).To_String);
                    Bet(Place,Bet_Side).Insert;
                    exit Race_Plc; -- match directly

                end case;
              end if;
            end loop Race_Plc;
          end loop Side_Loop_Plc;
        end;
      end loop Tic_Loop ;

    else
      Log(Me & "not enough data for runner" & Price_During_Race_List(Winner).Length'Img, Price_Data.To_String);
    end if;
  exception
    when Sql.Duplicate_Index =>
      Log("Winner/Back " & Bet(Winner,Back).To_String);
      Log("Winner/Lay  " & Bet(Winner,Lay).To_String);
      Log("Place /Back " & Bet(Place,Back).To_String);
      Log("Place /Lay  " & Bet(Place,Lay).To_String);
      raise;
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
  Global_Laysize   : constant Bet_Size_Type := 100.0;

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
    Ev.Set("BOT_NAME","bet_during_race_1");
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
        Run(Price_Data => Price, Backsize => Global_Backsize, Laysize => Global_Laysize);
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
end Bet_During_Race_1;
