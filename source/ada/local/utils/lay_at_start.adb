


with Ada.Strings; use Ada.Strings;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;

with Sim;
with Utils; --use Utils;
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
--with Prices;
with Bot_Svn_Info;
with Ini;
--with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Tics;

procedure Lay_At_Start is
  use type Ada.Containers.Count_Type;

  package Ev renames Ada.Environment_Variables;

  Bet_Size           : Bet_Size_Type := 30.0;

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

  --------------------------------------------------------------------------------

	      --  Lay_2_2_4_11_17_Win
  procedure Do_Place_Lay_Bets_At_Start(Bettype       : String;
                                       Br            : Best_Runners_Array_Type;
                                       Market        : Markets.Market_Type;
                                       Bet_List      : in out Bets.Lists.List) is

    Max_Back_Price       : Fixed_Type;
    Max_Lay_Price        : Max_Lay_Price_Type;
    First_Bet            : Integer := 0;
    Place_Num            : Integer := 0;
    Num_Bets             : Integer := 0;
    Lay_Price            : Fixed_Type := 0.0;
    Tic                  : Tics.Tics_Type := 1;
    Image                : String := Bettype;
   -- Service              : String := "Do_Place_Lay_Bets_At_Start";
    Runner               : Runners.Runner_Type;
    use type Tics.Tics_Type;
    Bet                  : Bets.Bet_Type;
  begin          --1         2         3
	      --  123456789012345678901234567890123456789
	      --  Lay_2_2_4_11_17_Win

    Num_Bets  := Integer'Value(Image(5..5));
    First_Bet := Integer'Value(Image(7..7));
    Place_Num := Integer'Value(Image(9..9));
    Max_Back_Price := Fixed_Type'Value(Image(11..12));
    Max_Lay_Price := Max_Lay_Price_Type'Value(Image(14..15));

    if Place_Num <= Br'Last and then
      Br(1).Backprice >= Fixed_Type(1.01) and then
      Br(Place_Num).Backprice < Fixed_Type(10_000.0) and then  -- so it exists
      Br(Place_Num).Backprice <= Max_Back_Price and then
      Br(Place_Num).Layprice <= Fixed_Type(Max_Lay_Price) then

      for I in Br'Range loop

        Runner.Selectionid := Br(I).Selectionid;

--          Log(Service & " " & Bettype & " I=" & I'Img &
--                " Num_Bets=" & Num_Bets'Img &
--                " First_Bet=" & First_Bet'Img &
--                " Place_Num=" & Place_Num'Img &
--                " Max_Back_Price=" & Max_Back_Price'Img &
--                " Max_Lay_Price=" & Max_Lay_Price'Img &
--                " Br(I).Layprice=" & Br(I).Layprice'Img &
--                " Br(I).Backprice=" & Br(I).Backprice'Img);

        if I >= First_Bet then
          if I < First_Bet + Num_Bets then
            if Br (I).Layprice <= Fixed_Type(Max_Lay_Price) then
              if Br (I).Backprice <= Max_Back_Price then
	                -- to get legal odds
                Tic := Tics.Get_Nearest_Higher_Tic_Index(Br(I).Layprice);
                Lay_Price := Tics.Get_Tic_Price(Tic+1); -- some small margin to get the bet

                Bet := Bets.Create(Name   => Image,
                                   Side   => Lay,
                                   Size   => 100.0,
                                   Price  => Price_Type(Lay_Price),
                                   Placed => Br(I).Pricets,
                                   Runner => Runner,
                                   Market => Market);
                Bet_List.Append(Bet);
              else
                null;
--                Log (Service & " " & Bettype & ": I =" & I'Img & " Br (I).Backprice <= Max_Back_Price= " & Boolean'Image(Br (I).Backprice <= Max_Back_Price));
              end if;
            else
                null;
--              Log (Service & " " & Bettype & ": I =" & I'Img & " Br (I).Layprice <= Max_Lay_Price= " & Boolean'Image(Br(I).Layprice <= Fixed_Type(Max_Lay_Price)));
            end if;
          else
--            Log (Service & " " & Bettype & ": I =" & I'Img & " I < First_Bet + Num_Bets= " & Boolean'Image(I < First_Bet + Num_Bets));
                null;
          end if;
        else
                null;
--          Log (Service & " " & Bettype & ": I =" & I'Img & " I >= First_Bet= " & Boolean'Image(I >= First_Bet));
        end if;
      end loop;
    end if;
--    Bets_Allowed(Bettype).Has_Betted := True; -- disabled in send_lay_bet for this type of bets
  end Do_Place_Lay_Bets_At_Start;



  procedure Check_Bets(List : in out Bets.Lists.List;
                       Bra  : in     Best_Runners_Array_Type) is
    begin
      -- Try To check outcome
      for B of List loop
        declare
          R              : Price_Histories.Price_History_Type;
          Price_Ok       : Boolean := False;
          Pricematched   : Fixed_Type := 0.0;
          Is_Race_Winner : Boolean := False;
        begin

          if B.Status(1) = 'U' then -- found unmatched bet

            for I in Bra'Range loop      --find runner
              if Bra(I).Selectionid = B.Selectionid then
                R := Bra(I);
                exit;
              end if;
            end loop;

            -- Found and then 1 second later at least, time for BF delay
      --      if R.Selectionid > 0 and then R.Pricets > B.Betplaced + (0,0,0,1,0) then

              if B.Side(1..3) = "LAY" then
                Price_Ok := R.Layprice <= B.Price and then R.Layprice > Fixed_Type(1.0) ; -- sanity
                Pricematched := R.Layprice;
              elsif B.Side(1..4) = "BACK" then
                Price_Ok := R.Backprice >= B.Price and then R.Backprice > Fixed_Type(1.0); -- sanity
                Pricematched := R.Backprice;
              end if;
              --      Log("Price_OK ", Price_Ok'Img);

              if Price_Ok then
                Move("MATCHED",B.Status);
                B.Pricematched := Pricematched;
                begin
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
                  --          Log("Bet_Inserted", B.To_String);
                exception
                  when others =>
                    Log("No-race-WInner ", "winner is missing in " & B.Marketid);
                end;
              end if;
            end if;
      --    end if;
        end;
      end loop;

  end Check_Bets;




  procedure Treat_Back(Market             : in     Markets.Market_Type;
                       Bra                : in     Best_Runners_Array_Type ;
                       Name               : in     Bot_Types.Betname_Type;
                       Bet_List           : in out Bets.Lists.List) is
    Bet            : Bets.Bet_Type;
    Runner         : Runners.Runner_Type;
    Localname      : Betname_Type := Name;
  begin
    if Bet_List.Length = 0 then -- ie no previous bet in this race, do the betting
      --  Log("Treat_Lay", I'Img & " " & Bra(I).To_String);
      --  Log("Treat_Lay", " 1 " & Bra(1).To_String);

      for J in 1 .. 5 loop -- back 5 first runners this market
        Runner.Selectionid := Bra(J).Selectionid;
        Runner.Marketid := Bra(J).Marketid;

        Localname(3) := 'B';
        Localname(8) := J'Img(2);

        Bet := Bets.Create(Name   => Localname,
                           Side   => Back,
                           Size   => Bet_Size,
                           Price  => Price_Type(Bra(J).Backprice),
                           Placed => Bra(J).Pricets,
                           Runner => Runner,
                           Market => Market);
        Bet_List.Append(Bet);
        --Log("Bet_laid-WIN-BACK", Bet.To_String);
      end loop;

      for J in 1 .. 5 loop -- lay 5 first runners winners market
        Runner.Selectionid := Bra(J).Selectionid;
        Runner.Marketid := Bra(J).Marketid;

        Localname(3) := 'L';
        Localname(8) := J'Img(2);

        Bet := Bets.Create(Name   => Localname,
                           Side   => Lay,
                           Size   => Bet_Size,
                           Price  => Price_Type(Bra(J).Layprice),
                           Placed => Bra(J).Pricets,
                           Runner => Runner,
                           Market => Market);
        Bet_List.Append(Bet);
        -- Log("Bet_laid-WIN_LAY", Bet.To_String);
      end loop;

    else
      -- Try To check outcome
      for B of Bet_List loop
        declare
          R              : Price_Histories.Price_History_Type;
          Price_Ok       : Boolean := False;
          Pricematched   : Fixed_Type := 0.0;
          Is_Race_Winner : Boolean := False;
        begin

          if B.Status(1) = 'U' then -- found unmatched bet

            for I in Bra'Range loop      --find runner
              if Bra(I).Selectionid = B.Selectionid then
                R := Bra(I);
                exit;
              end if;
            end loop;

            -- Found and then 1 second later at least, time for BF delay
            if R.Selectionid > 0 and then R.Pricets > B.Betplaced + (0,0,0,1,0) then

              if B.Side(1..3) = "LAY" then
                Price_Ok := R.Layprice <= B.Price and then R.Layprice > Fixed_Type(1.0) ; -- sanity
                Pricematched := R.Layprice;
              elsif B.Side(1..4) = "BACK" then
                Price_Ok := R.Backprice >= B.Price and then R.Backprice > Fixed_Type(1.0); -- sanity
                Pricematched := R.Backprice;
              end if;
              --      Log("Price_OK ", Price_Ok'Img);

              if Price_Ok then
                Move("MATCHED",B.Status);
                B.Pricematched := Pricematched;
                begin
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
                  --          Log("Bet_Inserted", B.To_String);
                exception
                  when others =>
                    Log("No-race-WInner ", "winner is missing in " & B.Marketid);
                end;
              end if;
            end if;
          end if;
        end;
      end loop;
    end if;
  end Treat_Back;
  pragma Unreferenced(Treat_Back);


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

  Start_Date          : constant Calendar2.Time_Type := (2016,3,16,0,0,0,0);
  One_Day             : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date        :          Calendar2.Time_Type := Start_Date;
  Stop_Date           : constant Calendar2.Time_Type := (2018,11,16,0,0,0,0);
  T                   :          Sql.Transaction_Type;
  Cmd_Line            :          Command_Line_Configuration;
  Sa_Logfilename      : aliased  Gnat.Strings.String_Access;
  Sa_Percent_Of_Race  : aliased  Gnat.Strings.String_Access;
  --Percent             : Fixed_Type := 0.0;

begin
  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Define_Switch
    (Cmd_Line,
     Sa_Percent_Of_Race'Access,
     Long_Switch => "--percent_of_race=",
     Help        => "bet at the estimated time given by the percentage");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","Bet_At_Fixed_Timings");
  end if;

  --  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);
  Log("main", "params start");
  Log("main", "Sa_Percent_Of_Race " & Sa_Percent_Of_Race.all);
  Log("main", "params stop");

  --Percent := Fixed_Type'Value(Sa_Percent_Of_Race.all) / 100.0;

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
      Cnt           : Integer := 0;
      Market_Ok     : Boolean := False;
  --    Eos           : Boolean := False;
  --    Avg_Racetime  : Seconds_Type := 0;
      Win_Market    : Markets.Market_Type;
    begin
      Market_Loop : for Market of Sim.Market_With_Data_List loop
      Log(Market.To_String);
        T.Start;
        if Market.Markettype(1..3) = "PLA" then
--            --find win market name
--            begin
--              pragma Compile_Time_Warning(True,"fix place_win_map ..");
--  --            Win_Market.Marketid := Sim.Place_Win_Map(Market.Marketid);
--              Win_Market.Read(Eos);
--              Market_Ok := not Eos and then Win_Market.Marketname_Ok2;
--              if Market_Ok then
--                Avg_Racetime := Sim.Racetime_Map(Win_Market.Marketname);
--              end if;
--            exception
--              when others => Market_Ok := False;
--            end ;
            Market_Ok := False;  --tmp !!!!
        elsif Market.Markettype(1..3) = "WIN" then
          begin
            Market_Ok := Market.Marketname_Ok2;
            if Market_Ok then
            --  Avg_Racetime := Sim.Racetime_Map(Market.Marketname);
              Win_Market := Market;
            end if;
          exception
            when others => Market_Ok := False;
          end ;
        else
          Market_Ok := False;
          Log("WTF - market " & Market.To_String);
        end if;

        if Market_Ok then
          Cnt := Cnt + 1;
          -- list of timestamps in this market
          declare
            Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);
            Bet_List                        : Bets.Lists.List;
            Race_Started                    : Boolean := False;
            Last_Timestamp                  : Time_Type := Time_Type_First;
  --          Betat_Timestamp                 : Time_Type := Time_Type_First;

          begin
            Loop_Ts : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop

              if not Race_Started and then
                Last_Timestamp /= Time_Type_First and Then
                Timestamp - Last_Timestamp < (0,0,0,1,0) then

                Race_Started := True;
    --            Betat_Timestamp := Timestamp + To_Interval(Seconds_Type(Percent * Natural(Avg_Racetime)));
              elsif Race_Started then -- Timestamp >= Betat_Timestamp then

                declare
                  List     : Price_Histories.Lists.List := Timestamp_To_Prices_History_Map(Timestamp.To_String);
                  Bra      : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
                  Name     : Betname_Type := (others => ' ');
                  Tail     : String(1..4) := "_Pln"; -- for 'plain'
                begin

                  if Utils.Position(S => Win_Market.Marketname, Match => "Hrd") > Integer(0) then
                    Tail := "_Hrd";
                  elsif Utils.Position(S => Win_Market.Marketname, Match => "Chs") > Integer(0) then
                    Tail := "_Chs";
                  end if;


 	      --  123456789012345678901234567890123456789
	      --  Lay_2_2_4_11_17_Win

              --  Num_Bets  := Integer'Value(Image(5..5));
              --  First_Bet := Integer'Value(Image(7..7));
              --  Place_Num := Integer'Value(Image(9..9));
              --  Max_Back_Price := Fixed_Type'Value(Image(11..12));
              --  Max_Lay_Price := Max_Lay_Price_Type'Value(Image(14..15));

                  Sort_Array(List => List, Bra  => Bra);

                  for Num_Bets in 1..5 loop
                    for First_Bet in 2..5 loop
                      for Place_Num in 4..5 loop
                        for Max_Back_Price in 10..30 loop
                       --   for Max_Lay_Price in 1..5 loop

                          Move("LAY_" &
                                 Num_Bets'Img(2) & "_" &
                                 First_Bet'Img(2) & "_" &
                                 Place_Num'Img(2) & "_" &
                                 Max_Back_Price'Img(2..3) & "_" &
                                 Integer(Max_Back_Price + 5)'Img(2..3) & "_" &
                                 "Win_" &
                                 Tail, Name);

                          Do_Place_Lay_Bets_At_Start(Name, Bra, Market, Bet_List);

                     --     end loop;
                        end loop;
                      end loop;
                    end loop;
                  end loop;

                  Check_Bets(Bet_List, Bra);
                  exit Loop_Ts;

--                    Treat_Back(Market             => Market,
--                               Bra                => Bra,
--                               Name               => Name,
--                               Bet_List           => Bet_List);

                end;
              end if;
              Last_Timestamp := Timestamp;

            end loop Loop_Ts; --  Timestamp
          end;

        end if;
      T.Commit;
    end loop Market_Loop;
  end;

  Current_Date := Current_Date + One_Day;
  exit when Current_Date = Stop_Date;

end loop Date_Loop;
Sql.Close_Session;

exception
when E: others =>
  Stacktrace.Tracebackinfo(E);
  Sql.Close_Session;
end Lay_At_Start;
