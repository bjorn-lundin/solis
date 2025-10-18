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
with Price_Histories; use Price_Histories;
with Bets;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Markets;
with Runners;
with Bot_Svn_Info;
with Ini;
--with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Tics;

procedure Lay_Losers_1 is
  use type Ada.Containers.Count_Type;

  package Ev renames Ada.Environment_Variables;

  Lay_Size           : Bet_Size_Type := 40.0;

  Ba_Back_Bet        : aliased Boolean := False;

  --   type Odds_Record is record
  --      Back_Num : Natural := 0;
  --      Lay_Num : Natural := 0;
  --   end record;

  --type Bet_Status_Type is (No_Bet_Laid, Bet_Laid, Bet_Matched);
  --Commission : Fixed_Type  := 0.065;

  subtype Key is String(1..7);

  package Odds_Maps is new Ada.Containers.Hashed_Maps
    (Key,
     Natural,
     Ada.Strings.Hash,
     "=",
     "=");

  --------------------------------------------------------------------------

  function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  type Best_Runners_Array_Type is array (1..12) of Price_Histories.Price_History_Type;


  procedure Treat_Lay(Market           : in Markets.Market_Type;
                      Bra              : in Best_Runners_Array_Type ;
                      Min_Lay_Price,
                      Max_Lay_Price,
                      Max_Leader_Price : in Price_Type;
                      Lay_Idx          : in Fixed_Type;
                      Name             : in Betname_Type;
                      Bet_List         : in out Bets.Lists.List) is
    Bet            : Bets.Bet_Type;
    Runner         : Runners.Runner_Type;
    Local_Bra      : Best_Runners_Array_Type := Bra;
    --Two : constant Fixed_Type := 2.0;
    Local_Bet_List : Bets.Lists.List;
  begin
    -- remove runners from local-BRA that already are betted on
    for B of Bet_List loop
      for I in Local_Bra'Range loop
        if Local_Bra(I).Selectionid = B.Selectionid then
          Local_Bra(I) := Price_Histories.Empty_Data;
        end if;
      end loop;
    end loop;

    for I in Local_Bra'Range loop
      --if Local_Bra(I).Selectionid > Integer_4(0) and then Lay_Bet_Status = No_Bet_Laid then
      if Local_Bra(I).Selectionid > Integer_4(0) then
        --  Log("Treat_Lay", I'Img & " " & Local_Bra(I).To_String);
        --  Log("Treat_Lay", " 1 " & Local_Bra(1).To_String);
        --Local_Bra(I).Layprice     <= Fixed_Type(Two * Local_Bra(I).Backprice) and then -- not too big difference allowed

        if Local_Bra(I).Selectionid > Integer_4(0) and then  -- sanity
          Local_Bra(1).Backprice    > Fixed_Type(1.0) and then  -- sanity
          Min_Lay_Price             <= Local_Bra(I).Backprice and then
          Local_Bra(I).Backprice    <= Max_Lay_Price and then
          Local_Bra(I).Layprice     >= Lay_Idx * Local_Bra(I).Backprice and then
                Bra(1).Backprice    <= Max_Leader_Price and then
                Bra(1).Backprice    > Fixed_Type(1.0) then  -- sanity


          Runner.Selectionid := Local_Bra(I).Selectionid;
          Runner.Marketid := Local_Bra(I).Marketid;

          declare
            Tic       : Tics.Tics_Type := Tics.Get_Nearest_Higher_Tic_Index((Lay_Idx + Fixed_Type(1.0)) * Local_Bra(I).Backprice);
            Lay_Price : Price_Type := Price_Type(Tics.Get_Tic_Price(Tic));
          begin
            Bet := Bets.Create(Name   => Name,
                               Side   => Lay,
                               Size   => Lay_Size,
                               Price  => Lay_Price,
                               Placed => Local_Bra(I).Pricets,
                               Runner => Runner,
                               Market => Market);
          end;
          Bet_List.Append(Bet);
          Log("Bet_laid", Bet.To_String);
          --exit; -- one bet only per race
        end if;
      end if;
    end loop;


    -- Try To check outcome

    for B of Bet_List loop
      declare
        R              : Price_Histories.Price_History_Type;
        Price_Ok       : Boolean := False;
        Pricematched   : Fixed_Type := 0.0;
        Is_Race_Winner : Boolean := False;
      begin
        Log("checking bet ", B.To_String);
        for I in Bra'Range loop      --find runner
          if Bra(I).Selectionid = B.Selectionid then
            R := Bra(I);
            exit;
          end if;
        end loop;

        Log("R is ", R.To_String);

        if R.Selectionid > 0 and then B.Status(1) = 'U' then -- found unmatched bet
          if R.Pricets > B.Betplaced + (0,0,0,1,0) then -- 1 second later at least, time for BF delay

            if B.Side(1..3) = "LAY" then
              Price_Ok := R.Layprice <= B.Price and then R.Layprice > Fixed_Type(1.0) ; -- sanity
              Pricematched := R.Layprice;
            elsif B.Side(1..4) = "BACK" then
              Price_Ok := R.Backprice >= B.Price and then R.Backprice > Fixed_Type(1.0); -- sanity
              Pricematched := R.Backprice;
            end if;
            Log("Price_OK ", Price_Ok'Img);

            if Price_Ok then
              B.Status := (others => ' ');
              B.Status(1..7) := "MATCHED"; --Matched
              B.Pricematched := Pricematched;
              Is_Race_Winner := Sim.Is_Race_Winner(B.Selectionid, B.Marketid);

              if B.Side(1..3) = "LAY" then
                B.Betwon := not Is_Race_Winner;
                if B.Betwon then
                  B.Profit :=  B.Sizematched; -- commission is calculated/market
                else
                  B.Profit := -B.Sizematched * (B.Pricematched - 1.0);
                end if;

              elsif B.Side(1..4) = "BACK" then
                B.Betwon := Is_Race_Winner;
                if B.Betwon then
                  B.Profit :=  B.Sizematched * (B.Pricematched - 1.0);  -- commision is calculated/market
                else
                  B.Profit := -B.Sizematched;
                end if;
              end if;

              B.Insert;
              Log("Bet_Inserted", B.To_String);
              -- if we just matched a laybet - put a backbet then
              if B.Side(1..3) = "LAY" and then Ba_Back_Bet then
                -- Bet.Size = Lay_Bet.Size - 5
                -- Lay_Bet.Size * Lay_Bet.Pricematched = Back_Size * Back_Price;
                declare
                  Back_Size  : Bet_Size_Type  := Bet_Size_Type(B.Size - Fixed_Type(1.0));
                  Back_Price : Fixed_Type := B.Size * B.Pricematched ;
                  Tic        : Tics.Tics_Type;
                begin
                  Runner.Selectionid := B.Selectionid;
                  Runner.Marketid    := B.Marketid;
                  -- get real tic value
                  --  Log(B.Size'Img & "/" & B.Pricematched'Img & "/" & Back_Size'Img & "/" & Back_Price'Img);
                  Back_Price := Back_Price / Fixed_Type(Back_Size);
                  --  Log(B.Size'Img & "/" & B.Pricematched'Img & "/" & Back_Size'Img & "/" & Back_Price'Img);

                  Tic := Tics.Get_Nearest_Higher_Tic_Index(Back_Price);
                  Back_Price := Tics.Get_Tic_Price(Tic);

                  Bet.Clear;
                  Bet := Bets.Create(Name   => Name,
                                     Side   => Back,
                                     Size   => Back_Size,
                                     Price  => Price_Type(Back_Price),
                                     Placed => R.Pricets,
                                     Runner => Runner,
                                     Market => Market);
                  Local_Bet_List.Append(Bet);
                end;
              end if;
            end if;
          end if;
        end if;
      end;
    end loop;

    for B of Local_Bet_List loop
      Bet_List.Append(B);
    end loop;
  end Treat_Lay;
  -- pragma Unreferenced (Treat_Lay);



  procedure Sort_Array(List : in out Price_Histories.Lists.List ;
                       Bra  : in out Best_Runners_Array_Type ) is

    Price             : Price_Histories.Price_History_Type;
  begin
    -- ok find the runner with lowest backprice:
    Backprice_Sorter.Sort(List);

    Price.Backprice := 10_000.0;
    Bra := (others => Price);

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

  end Sort_Array;
  -- pragma Unreferenced (Sort_Array);
  ---------------------------------------------------------------

  Start_Date          : constant Calendar2.Time_Type := (2016,03,16,0,0,0,0);
  One_Day             : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date        :          Calendar2.Time_Type := Start_Date;
  Stop_Date           : constant Calendar2.Time_Type := (2018,08,01,0,0,0,0);
  T                   :          Sql.Transaction_Type;
  Cmd_Line            :          Command_Line_Configuration;
  Sa_Logfilename      : aliased  Gnat.Strings.String_Access;
  Sa_Min_Lay_Price    : aliased  Gnat.Strings.String_Access;
  Sa_Max_Lay_Price    : aliased  Gnat.Strings.String_Access;
  Sa_Lay_Idx          : aliased  Gnat.Strings.String_Access;
  Sa_Max_Leader_Price : aliased  Gnat.Strings.String_Access;
  Min_Lay_Price       :          Price_Type := 0.0;
  Max_Lay_Price       :          Price_Type := 0.0;
  Max_Leader_Price    :          Price_Type := 0.0;
  Lay_Idx             :          Fixed_Type := 0.0;

begin
  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Define_Switch
    (Cmd_Line,
     Sa_Min_Lay_Price'Access,
     Long_Switch => "--min_lay_price=",
     Help        => "back price must be higher that this");

  Define_Switch
    (Cmd_Line,
     Sa_Max_Lay_Price'Access,
     Long_Switch => "--max_lay_price=",
     Help        => "back price must be lower that this");

  Define_Switch
    (Cmd_Line,
     Sa_Max_Leader_Price'Access,
     Long_Switch => "--max_leader_price=",
     Help        => "leader's back price must be lower that this");

  Define_Switch
    (Cmd_Line,
     Sa_Lay_Idx'Access,
     Long_Switch => "--lay_idx=",
     Help        => "lay the runner at this idx * backprice");

  Define_Switch
    (Cmd_Line,
     Ba_Back_Bet'Access,
     Long_Switch => "--back_bet",
     Help        => "do a back bet too");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","lay_losers_1");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);
  Log("main", "params start");
  Log("main", "Sa_Min_Lay_Price" & Sa_Min_Lay_Price.all);
  Log("main", "Sa_Max_Lay_Price" & Sa_Max_Lay_Price.all);
  Log("main", "Sa_Max_Leader_Price" & Sa_Max_Leader_Price.all);
  Log("main", "Sa_Lay_idx" & Sa_Lay_Idx.all);
  Log("main", "params stop");

  Min_Lay_Price    := Price_Type'Value(Sa_Min_Lay_Price.all);
  Max_Lay_Price    := Price_Type'Value(Sa_Max_Lay_Price.all);
  Max_Leader_Price := Price_Type'Value(Sa_Max_Leader_Price.all);
  Lay_Idx          := Fixed_Type'Value(Sa_Lay_Idx.all);

  if Min_Lay_Price >= Max_Lay_Price then
    Log("main", "Min >= Mac -> stop");
    return;
  end if;


  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");

  Log("main", "Connect Db " &
        Ini.Get_Value("database_home", "host", "")  & " " &
        Ini.Get_Value("database_home", "port", 5432)'Img & " " &
        Ini.Get_Value("database_home", "name", "") & " " &
        Ini.Get_Value("database_home", "username", "") & " " &
        Ini.Get_Value("database_home", "password", "")
     );

  Sql.Connect
    (Host     => Ini.Get_Value("database_home", "host", ""),
     Port     => Ini.Get_Value("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value("database_home", "name", ""),
     Login    => Ini.Get_Value("database_home", "username", ""),
     Password => Ini.Get_Value("database_home", "password", ""));
  Log("main", "db Connected");

  Date_Loop : loop
    T.Start;
    Log("start fill maps");
    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse);
    Log("start process maps");
    T.Commit;

    declare
      Cnt       : Integer := 0;
    begin
      Market_Loop : for Market of Sim.Market_With_Data_List loop
        T.Start;
        if Market.Markettype(1..3) = "WIN" and then
          Market.Marketname_Ok then

          Cnt := Cnt + 1;
          -- list of timestamps in this market
          declare
            Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);
            Bet_List                        : Bets.Lists.List;
          begin
            Loop_Ts : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              declare
                List     : Price_Histories.Lists.List := Timestamp_To_Prices_History_Map(Timestamp.To_String);
                Bra      : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
                Name     : Betname_Type := (others => ' ');
                Five     : String(1..5) := "05.00";
              begin
                if Min_Lay_Price /= 5.0 then
                  Five := F8_Image(Fixed_Type(Min_Lay_Price));
                end if;

                Move("WIN_" &
                       Five & "_" &
                       F8_Image(Fixed_Type(Max_Lay_Price)) & "_" &
                       F8_Image(Fixed_Type(Max_Leader_Price)) & "_" &
                       F8_Image(Lay_Idx)   & "_LAY_LOSERS_1", Name);


                Sort_Array(List => List, Bra  => Bra);
                Treat_Lay(Market           => Market,
                          Bra              => Bra,
                          Min_Lay_Price    => Min_Lay_Price,
                          Max_Lay_Price    => Max_Lay_Price,
                          Max_Leader_Price => Max_Leader_Price,
                          Lay_Idx          => Lay_Idx,
                          Name             => Name,
                          Bet_List         => Bet_List);
              end;
            end loop Loop_Ts; --  Timestamp
            Bets.Sum_Laybets(Bet_List, -2000.0);
          end;
        end if; -- Market_type(1..3) = WIN
        T.Commit;
      end loop Market_Loop;
    end;

    --Sim.Delete_Shared_Mem(Current_Date, Bot_Types.Horse);

    Current_Date := Current_Date + One_Day;
    exit when Current_Date = Stop_Date;

  end loop Date_Loop;

  Sql.Close_Session;

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
    Sql.Close_Session;
end Lay_Losers_1;
