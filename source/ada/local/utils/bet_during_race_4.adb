with Ada.Strings; use Ada.Strings;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;

with Sim;
with Utils; use Utils;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Text_Io;
with Price_Histories;
with Bets;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Markets;
with Runners;
with Bot_Svn_Info;
with Ini;


procedure Bet_During_Race_4 is

  package Ev renames Ada.Environment_Variables;
  Global_Bet_List : Bets.Lists.List;

  Lay_Size  : Bet_Size_Type := 30.0;
  Back_Size : Bet_Size_Type := 30.0;

  type Bet_Status_Type is (No_Bet_Laid, Bet_Laid, Bet_Matched);
  Lay_Bet_Status : Bet_Status_Type := No_Bet_Laid;
  Back_Bet_Status : Bet_Status_Type := No_Bet_Laid;


  Global_Back_1_At,
  Global_Back_2_At,
  Global_Min_Delta,
  Global_Max_Price   : Fixed_Type := 0.0;
  Ba_Immediate_Match : aliased Boolean := False;


  --------------------------------------------------------------------------

  function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  type Best_Runners_Array_Type is array (1..12) of Price_Histories.Price_History_Type;


  procedure Treat_Lay(List         : in     Price_Histories.Lists.List ;
                      Market       : in     Markets.Market_Type;
                      Wr           : in     Price_Histories.Price_History_Type ;
                      Bra          : in     Best_Runners_Array_Type ;
                      Old_Bra      : in     Best_Runners_Array_Type ;
                      Status       : in out Bet_Status_Type;
                      Bet_List     : in out Bets.Lists.List) is
    pragma Unreferenced(List);
    pragma Unreferenced(Wr);
    pragma Unreferenced(Status);
    -- pragma Unreferenced(BRA);
    -- pragma Unreferenced(Old_bra);
    Bet    : Bets.Bet_Type;
    Runner : Runners.Runner_Type;
    Name   : Betname_Type := (others => ' ');
    Idx    : Integer := 0;
    Local_Max_Price : Fixed_Type := Global_Max_Price + Fixed_Type(25.0);
    Local_Bra : Best_Runners_Array_Type := Bra;
  begin

    -- remove runners from local-BRA that already are betted on
    for B of Bet_List loop
      for I in Local_Bra'Range loop
        if Local_Bra(I).Selectionid = B.Selectionid then
          Local_Bra(I) := Price_Histories.Empty_Data;
        end if;
      end loop;
    end loop;

    -- find the first one to lay. lay only one at a time
    for I in Local_Bra'Range loop
      if Local_Bra(I).Selectionid > 0 and then
        Local_Bra(I).Layprice >= Global_Min_Delta + Old_Bra(I).Layprice and then
        Old_Bra(I).Layprice < Global_Max_Price then
        Idx := I;
        exit;
      end if;
    end loop;

    if Idx > Integer(0) then --Candidate Found To Lay

      if Local_Bra(Idx).Backprice >= Fixed_Type(1.0)and then
        Local_Bra(Idx).Layprice  >= Fixed_Type(1.0) and then
        Local_Bra(Idx).Backprice <= Fixed_Type(100.0) and then
        Local_Bra(Idx).Layprice  <= Local_Max_Price then

        Runner.Selectionid := Local_Bra(Idx).Selectionid;
        Runner.Marketid := Local_Bra(Idx).Marketid;

        if Global_Min_Delta < 10.0 then
          Move("WIN_LAY_" & F8_Image(Global_Max_Price) & "_0" & F8_Image(Global_Min_Delta) & "_" & Ba_Immediate_Match'Img(1), Name);
        else
          Move("WIN_LAY_" & F8_Image(Global_Max_Price) & "_" & F8_Image(Global_Min_Delta) & "_" & Ba_Immediate_Match'Img(1), Name);
        end if;


        Bet := Bets.Create(Name   => Name,
                           Side   => Lay,
                           Size   => Lay_Size,
                           Price  => Price_Type(Local_Max_Price),
                           Placed => Local_Bra(Idx).Pricets,
                           Runner => Runner,
                           Market => Market);
        Bet_List.Append(Bet);
      end if;
    end if;


    -- Try To check outcome


    for B of Bet_List loop
      --find runner
      declare
        R : Price_Histories.Price_History_Type;
      begin

        for I in Local_Bra'Range loop
          if Bra(I).Selectionid = B.Selectionid then
            R := Bra(I);
            exit;
          end if;
        end loop;

        if R.Selectionid > 0 then -- found
          if R.Pricets > B.Betplaced + (0,0,0,1,0) then -- 1 second later at least, time for BF delay
            if R.Layprice <= B.Price and then -- Laybet so yes '<=' NOT '>='
              R.Layprice >  Fixed_Type(1.0) and then -- sanity
              R.Backprice >  Fixed_Type(1.0) and then -- sanity
              B.Status(1)  = 'U' then -- sanity
              B.Status(1..20) := "MATCHED             "; --Matched
              B.Pricematched := R.Layprice;
              B.Check_Outcome;
              B.Insert;
              exit;
            elsif Ba_Immediate_Match then
              B.Status(1) := 'L'; -- lapsed. will not be matched anymore
              exit;
            end if;
          end if;
        end if;
      end;
    end loop;
  end Treat_Lay;

  --------------------------------------------------------------------------
  procedure Treat_Back(List         : in     Price_Histories.Lists.List ;
                       Market       : in out Markets.Market_Type;
                       Bra          : in     Best_Runners_Array_Type ;
                       Status       :    out Bet_Status_Type;
                       Back_1_At    : in Fixed_Type;
                       Back_2_At    : in Fixed_Type) is
    pragma Unreferenced(List);
    use type Price_Histories.Price_History_Type;
    Runner : Runners.Runner_Type;
    Name : Betname_Type := (others => ' ');
  begin

    if Bra(1).Backprice <= Back_1_At and then
      Bra(2).Backprice >= Back_2_At and then
      Bra(2).Backprice < Fixed_Type(10_000.0) then  -- so it exists

      declare
        Bet_Place                    : Bets.Bet_Type;
        Place_Market                 : Markets.Market_Type;
        Found                        : Boolean := False;
        Place_Price_During_Race_List : Price_Histories.Lists.List;
        Place_Bet_Status             :  Bet_Status_Type := No_Bet_Laid;
      begin
        Market.Corresponding_Place_Market(Place_Market => Place_Market, Found => Found);
        if Found then
          Runner.Marketid := Place_Market.Marketid;
          Runner.Selectionid := Bra(1).Selectionid;

          if Back_2_At < 10.0 then
            Move("PLC_BACK_" & F8_Image(Back_1_At) & "_0" & F8_Image(Back_2_At) & "_" & Ba_Immediate_Match'Img(1), Name);
          else
            Move("PLC_BACK_" & F8_Image(Back_1_At) & "_" & F8_Image(Back_2_At) & "_" & Ba_Immediate_Match'Img(1), Name);
          end if;

          -- read the place odds for this runner - if any
          Sim.Read_Marketid_Selectionid(Marketid    => Place_Market.Marketid,
                                        Selectionid => Runner.Selectionid,
                                        Animal      => Horse,
                                        List        => Place_Price_During_Race_List) ;
          for Po of Place_Price_During_Race_List loop
            case Place_Bet_Status is
              when No_Bet_Laid =>
                if Po.Pricets >= Bra(1).Pricets then
                  Bet_Place := Bets.Create(Name   => Name,
                                           Side   => Back,
                                           Size   => Back_Size,
                                           Price  => Price_Type(Po.Backprice),
                                           Placed => Po.Pricets,
                                           Runner => Runner,
                                           Market => Place_Market);
                  Place_Bet_Status := Bet_Laid;
                end if;

              when Bet_Laid =>
                if Po.Pricets > Bet_Place.Betplaced + (0,0,0,1,0) then -- 1 second later at least, time for BF delay
                  if Po.Backprice >= Bet_Place.Price and then -- Backbet so yes '>=' NOT '<='
                    Po.Layprice > Fixed_Type(1.0) and then -- sanity
                    Po.Backprice > Fixed_Type(1.0) and then -- sanity
                    Bet_Place.Status(1)  = 'U' then -- sanity
                    Bet_Place.Status(1..20) := "MATCHED             "; --Matched
                    Bet_Place.Pricematched := Po.Backprice;
                    Bet_Place.Check_Outcome;
                    Bet_Place.Insert;
                    Place_Bet_Status := Bet_Matched;
                  elsif Ba_Immediate_Match then
                    Place_Bet_Status := Bet_Matched;
                    exit;
                  end if;
                end if;

              when Bet_Matched => exit;
            end case;
          end loop;
          Status := Bet_Matched; -- bet handled, exit outer loop
        else
          Status := Bet_Matched; -- no placemarket, skip this in outer loop
        end if;
      end;
    end if; -- if ok odds

  end Treat_Back;
  --------------------------------------------------------------------------

  Best_Runners      : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
  Old_Best_Runners  : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
  Worst_Runner      : Price_Histories.Price_History_Type := Price_Histories.Empty_Data;

  procedure Sort_Array(List : in out Price_Histories.Lists.List ;
                       Bra  :    out Best_Runners_Array_Type;
                       Wr   :    out Price_Histories.Price_History_Type ) is

    Price             : Price_Histories.Price_History_Type;
  begin
    -- ok find the runner with lowest backprice:
    Backprice_Sorter.Sort(List);

    Price.Backprice := 10_000.0;
    Bra := (others => Price);
    Wr.Layprice := 10_000.0;

    declare
      Idx : Integer := 0;
    begin
      for Tmp of List loop
        if Tmp.Status(1..6) = "ACTIVE" and then
          Tmp.Backprice > Fixed_Type(1.0) and then
          Tmp.Layprice < Fixed_Type(1_000.0)  then
          Idx := Idx +1;
          exit when Idx > Bra'Last;
          Bra(Idx) := Tmp;
        end if;
      end loop;
    end ;

    for Tmp of List loop
      if Tmp.Status(1..6) = "ACTIVE" and then
        Tmp.Backprice > Fixed_Type(1.0) and then
        Tmp.Layprice < Fixed_Type(1_000.0) and then
        Tmp.Selectionid /= Bra(1).Selectionid and then
        Tmp.Selectionid /= Bra(2).Selectionid then

        Wr := Tmp;
      end if;
    end loop;

    -- for i in BRA'range loop
    --   Log("Best_Runners(i)" & i'Img & " " & BRA(i).To_String);
    -- end loop;
    -- Log("Worst_Runner " & WR.To_String);

  end Sort_Array;
  ---------------------------------------------------------------

  Start_Date     : constant Calendar2.Time_Type := (2016,03,16,0,0,0,0);
  One_Day        : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date   :          Calendar2.Time_Type := Start_Date;
  Stop_Date      : constant Calendar2.Time_Type := (2018,03,01,0,0,0,0);
  T              :          Sql.Transaction_Type;
  Cmd_Line       :          Command_Line_Configuration;
  Sa_Logfilename : aliased  Gnat.Strings.String_Access;
  Sa_Action      : aliased  Gnat.Strings.String_Access;
  Sa_Back_1_At   : aliased  Gnat.Strings.String_Access;
  Sa_Back_2_At   : aliased  Gnat.Strings.String_Access;
  Sa_Min_Delta   : aliased  Gnat.Strings.String_Access;
  Sa_Max_Price   : aliased  Gnat.Strings.String_Access;

  type Action_Type is (Do_Back, Do_Lay);
  Global_Action  : Action_Type := Do_Back;

begin
  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Define_Switch
    (Cmd_Line,
     Sa_Action'Access,
     Long_Switch => "--action=",
     Help        => "back, lay");

  Define_Switch
    (Cmd_Line,
     Sa_Back_1_At'Access,
     Long_Switch => "--back_1_at=",
     Help        => "max price of leader for backing leader");

  Define_Switch
    (Cmd_Line,
     Sa_Back_2_At'Access,
     Long_Switch => "--back_2_at=",
     Help        => "min price of second placer for backing leader");

  Define_Switch
    (Cmd_Line,
     Sa_Min_Delta'Access,
     Long_Switch => "--min_delta=",
     Help        => "min delta of runner for laying it");

  Define_Switch
    (Cmd_Line,
     Sa_Max_Price'Access,
     Long_Switch => "--max_price=",
     Help        => "max price that min_delta applies to");

  Define_Switch
    (Cmd_Line,
     Ba_Immediate_Match'Access,
     Long_Switch => "--immediate_match",
     Help        => "abandon bet if not matched within a second");


  Getopt (Cmd_Line);  -- process the command line

  if Sa_Back_1_At.all /= "" then
    Global_Back_1_At := Fixed_Type'Value(Sa_Back_1_At.all);
  end if;

  if Sa_Back_2_At.all /= "" then
    Global_Back_2_At := Fixed_Type'Value(Sa_Back_2_At.all);
  end if;

  if Sa_Min_Delta.all /= "" then
    Global_Min_Delta := Fixed_Type'Value(Sa_Min_Delta.all);
  end if;

  if Sa_Max_Price.all /= "" then
    Global_Max_Price := Fixed_Type'Value(Sa_Max_Price.all);
  end if;


  if Sa_Action.all = "back" then
    Global_Action := Do_Back;
  elsif Sa_Action.all = "lay" then
    Global_Action := Do_Lay;
  else
    raise Constraint_Error with "Bad Action: '" & Sa_Action.all & "'";
  end if;


  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","lay_during_race3");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log("main", "Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database_home", "host", ""),
     Port     => Ini.Get_Value("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value("database_home", "name", ""),
     Login    => Ini.Get_Value("database_home", "username", ""),
     Password => Ini.Get_Value("database_home", "password", ""));
  Log("main", "db Connected");

  Log("main", "params start");

  Log("main", "params stop");


  Date_Loop : loop
    T.Start;
    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse);
    Log("start process");

    declare
      Cnt       : Integer := 0;
      First_Run : Boolean := True;
    begin
      Market_Loop : for Market of Sim.Market_With_Data_List loop
        if Market.Markettype(1..3) = "WIN" then

          Cnt := Cnt + 1;
          --   Log( F8_Image(Fixed_Type(Cnt)*100.0/ Fixed_Type(Sim.Market_Id_With_Data_List.Length)) & " %");
          Back_Bet_Status := No_Bet_Laid;
          Lay_Bet_Status := No_Bet_Laid;
          -- list of timestamps in this market
          declare
            Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
              Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);

          begin
            Loop_Ts : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              declare
                List : Price_Histories.Lists.List := Timestamp_To_Prices_History_Map(Timestamp.To_String);
              begin
                --  Log("in loop", Timestamp.To_String & "_" & F8_Image(Back_1_At) & "_" & F8_Image(Back_2_At));

                Sort_Array(List => List,
                           Bra  => Best_Runners,
                           Wr   => Worst_Runner);

                case Global_Action is
                  when Do_Back =>
                    Treat_Back(List          => List,
                               Market        => Market,
                               Bra           => Best_Runners,
                               Status        => Back_Bet_Status,
                               Back_1_At     => Global_Back_1_At,
                               Back_2_At     => Global_Back_2_At);
                  when Do_Lay =>
                    if First_Run then -- to get values on old_Best_runners
                      First_Run := False;
                    else
                      Treat_Lay(List          => List,
                                Market        => Market,
                                Bra           => Best_Runners,
                                Old_Bra       => Old_Best_Runners,
                                Wr            => Worst_Runner,
                                Status        => Lay_Bet_Status,
                                Bet_List      => Global_Bet_List);
                    end if;
                end case;
                Old_Best_Runners := Best_Runners;

              end;
              exit Loop_Ts when  (Global_Action = Do_Back and then Back_Bet_Status = Bet_Matched);
              --or else
              --  (Global_Action = Do_Lay and then Lay_Bet_Status = Bet_Matched);
            end loop Loop_Ts; --  Timestamp
          end;
          Log("num lay bets laid" & Global_Bet_List.Length'Img);
          Global_Bet_List.Clear;
        end if; -- Market_type(1..3) = WIN
      end loop Market_Loop;
    end;

    Current_Date := Current_Date + One_Day;
    exit when Current_Date = Stop_Date;

    T.Commit;
  end loop Date_Loop;

  Sql.Close_Session;    -- no need for db anymore

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Bet_During_Race_4;
