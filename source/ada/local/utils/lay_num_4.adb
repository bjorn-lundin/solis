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
--with Ada.Containers.Hashed_Maps;
--with Ada.Strings.Hash;
with Ada.Containers;

procedure Lay_Num_4 is
 use type Ada.Containers.Count_Type;

  package Ev renames Ada.Environment_Variables;

  Lay_Size           : Bet_Size_Type := 40.0;


  --   type Odds_Record is record
  --      Back_Num : Natural := 0;
  --      Lay_Num : Natural := 0;
  --   end record;

  --type Bet_Status_Type is (No_Bet_Laid, Bet_Laid, Bet_Matched);
  --Commission : Fixed_Type  := 0.065;

--    subtype Key is String(1..7);
--
--    package Odds_Maps is new Ada.Containers.Hashed_Maps
--      (Key,
--       Natural,
--       Ada.Strings.Hash,
--       "=",
--       "=");

  --------------------------------------------------------------------------

  function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  type Best_Runners_Array_Type is array (1..12) of Price_Histories.Price_History_Type;

  procedure Treat_Lay(Market                      : in     Markets.Market_Type;
                      Bra                         : in     Best_Runners_Array_Type ;
                      Max_Leader_Price, Lay_Price : in Price_Type;
                      Name                        : in Betname_Type;
                      Bet_List                    : in out Bets.Lists.List) is
    Bet            : Bets.Bet_Type;
    Runner         : Runners.Runner_Type;
 --   Five           : constant Fixed_Type := 5.0;
    Local_Bet_List : Bets.Lists.List;
    --I              : Integer := 4;
    Localname      : Betname_Type := Name;
  begin
    if Bet_List.Length = 0 then -- ie no previous bet in this race
      --  Log("Treat_Lay", I'Img & " " & Bra(I).To_String);
      --  Log("Treat_Lay", " 1 " & Bra(1).To_String);

      if Bra(4).Selectionid > Integer_4(0) and then  -- sanity
        Bra(4).Backprice    >= Fixed_Type(1.0) and then  -- sanity
        Bra(4).Layprice     >= Fixed_Type(1.0) and then  -- sanity
      --  Bra(4).Layprice     <= Fixed_Type(Five * Bra(4).Backprice) and then -- not too big difference allowed
        Bra(1).Backprice    <= Max_Leader_Price and then
        Bra(1).Backprice    > Fixed_Type(1.0) then  -- sanity

        for J in 1 .. 4 loop
          Runner.Selectionid := Bra(J).Selectionid;
          Runner.Marketid := Bra(J).Marketid;
          Localname(19..22) := "_" & J'Img(2) & "TH";

          Bet := Bets.Create(Name   => Localname,
                             Side   => Lay,
                             Size   => Lay_Size,
                             Price  => Lay_Price,
                             Placed => Bra(J).Pricets,
                             Runner => Runner,
                             Market => Market);
          Bet_List.Append(Bet);
          Log("Bet_laid", Bet.To_String);
        end loop;
      end if;
    end if;
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
  Sa_Max_Leader_Price : aliased  Gnat.Strings.String_Access;
  Sa_Lay_Price        : aliased  Gnat.Strings.String_Access;
  Max_Leader_Price    : Price_Type;
  Lay_Price           : Price_Type;

begin
  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");


  Define_Switch
    (Cmd_Line,
     Sa_Max_Leader_Price'Access,
     Long_Switch => "--max_leader_price=",
     Help        => "leader's back price must be lower that this");

  Define_Switch
    (Cmd_Line,
     Sa_Lay_Price'Access,
     Long_Switch => "--lay_price=",
     Help        => "lay the runner at this price");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","lay_during_race3");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);
  Log("main", "params start");
  Log("main", "Sa_Max_Leader_Price" & Sa_Max_Leader_Price.all);
  Log("main", "Sa_Lay_Price" & Sa_Lay_Price.all);
  Log("main", "params stop");

  Max_Leader_Price := Price_Type'Value(Sa_Max_Leader_Price.all);
  Lay_Price        := Price_Type'Value(Sa_Lay_Price.all);

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
              begin

                Move("WIN_LAY_" &
                       F8_Image(Fixed_Type(Max_Leader_Price)) & "_" &
                       F8_Image(Fixed_Type(Lay_Price)) , Name);

                Sort_Array(List => List, Bra  => Bra);
                Treat_Lay(Market           => Market,
                          Bra              => Bra,
                          Max_Leader_Price => Max_Leader_Price,
                          Lay_Price        => Lay_Price,
                          Name             => Name,
                          Bet_List         => Bet_List);
              end;
            end loop Loop_Ts; --  Timestamp
            --Bets.Sum_Laybets(Bet_List, -12_000.0);
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
end Lay_Num_4;
