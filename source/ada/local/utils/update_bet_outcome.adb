with Ada.Strings; use Ada.Strings;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Text_Io;


with Types;    use Types;
with Bot_Types ; use Bot_Types;
with Sql;
with Sim;
with Calendar2; use Calendar2;
with Logging;               use Logging;
with Ini;
with Bets;
with Price_Histories;
with Markets;

procedure Update_Bet_Outcome is
  package Ev renames Ada.Environment_Variables;
  Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type;
  Find_Plc_Market,
  Select_Bets           : Sql.Statement_Type;

  Sa_Betname          : aliased Gnat.Strings.String_Access;


  Betlist : Bets.Lists.List;
  type Best_Runners_Array_Type is array (1..12) of Price_Histories.Price_History_Type;
  Gdebug : Boolean := True;
  -------------------------------
  procedure Debug (What : String) is
  begin
    if Gdebug then
      Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_Iso (Clock, " " , "") & " " & What);
    end if;
  end Debug;
  pragma Warnings(Off, Debug);
  -------------------------------
  procedure Print (What : String) is
  begin
    Text_Io.Put_Line (What);
  end Print;
  pragma Unreferenced (Print);
  -------------------------------

  function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  procedure Read_Bets(List : in out bets.Lists.List; Date : Calendar2.Time_Type) is
    Start : Calendar2.Time_Type := Date;
    Stop  : Calendar2.Time_Type := Date;

  begin
    Start.Hour := 0;
    Start.Minute := 0;
    Start.Second := 0;
    Start.Millisecond := 0;

    Stop.Hour := 23;
      Stop.Minute := 59;
    Stop.Second := 59;
    Stop.Millisecond := 999;
    Select_Bets.Prepare("select * from ABETS where BETNAME like :BETNAME " &
      "and BETPLACED >= :TSSTART and BETPLACED <= :TSSTOP");
    Select_Bets.Set("BETNAME", "PLC%");
    Select_Bets.Set_Timestamp("TSSTART", Start);
    Select_Bets.Set_Timestamp("TSSTOP", Stop);
    Bets.Read_List(Select_Bets, List);
  end Read_Bets;

  --------------------------------------------------------

  procedure Update_To_Place_Market(List : in out bets.Lists.List) is
    Eos : Boolean := True;
    Pm : Markets.Market_Type;
    Cnt : Integer := 0;
  begin
    Debug("total #" &  List.Length'Img);

    Find_Plc_Market.Prepare("select MP.* from AMARKETS MW, AMARKETS MP " &
                              "where MW.EVENTID = MP.EVENTID " &
                              "and MW.STARTTS = MP.STARTTS " &
                              "and MW.MARKETID = :WINMARKETID " &
                              "and MP.MARKETTYPE = 'PLACE' " &
                              "and MW.MARKETTYPE = 'WIN'");
    for B of List loop
      Cnt := Cnt +1;
      Find_Plc_Market.Set("WINMARKETID", B.Marketid);
      Find_Plc_Market.Open_Cursor;
      Find_Plc_Market.Fetch(Eos);
      if not Eos then
        PM := Markets.Get(Find_Plc_Market);
        B.Marketid := Pm.Marketid;
        B.Fullmarketname := Pm.Marketname;
        B.Update_Withcheck;
      end if;
      Find_Plc_Market.Close_Cursor;
      if Cnt rem 1_000 = 0 then
        Debug("did #" & Cnt'Img);
      end if;

    end loop;
  end Update_To_Place_Market;
  pragma Unreferenced (Update_To_Place_Market);


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
  ---------------------------------------------------------
  procedure Update_Bets(List : in out bets.Lists.List;
                        --Date : Calendar2.Time_Type;
                        Bra  : Best_Runners_Array_Type ) is
  begin
    for B of List loop
      declare
        R              : Price_Histories.Price_History_Type;
        Price_Ok       : Boolean := False;
        Pricematched   : Fixed_Type := 0.0;
        Is_Race_Winner : Boolean := False;
      begin
     --   Log("checking bet ", B.To_String);
        for I in Bra'Range loop      --find runner
          if Bra(I).Selectionid = B.Selectionid then
            R := Bra(I);
            exit;
          end if;
        end loop;

       -- Log("R is ", R.To_String);

        if R.Selectionid > 0 and then B.Status(1) = 'U' then -- found unmatched bet
          if R.Pricets > B.Betplaced + (0,0,0,1,0) then -- 1 second later at least, time for BF delay

            if B.Side(1..3) = "LAY" then
              Price_Ok := R.Layprice <= B.Price and then R.Layprice > Fixed_Type(1.0) ; -- sanity
              Pricematched := R.Layprice;
            elsif B.Side(1..4) = "BACK" then
              Price_Ok := R.Backprice >= B.Price and then R.Backprice > Fixed_Type(1.0); -- sanity
              Pricematched := R.Backprice;
            end if;
        --    Log("Price_OK ", Price_Ok'Img);

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

                B.Update_Withcheck;
                Log("Bet_Updated", B.To_String);
              exception
                when others =>
                  Log("No-race-WInner ", "winner is missing in " & B.Marketid);
              end;
            end if;
          end if;
        end if;
      end;
    end loop;

  end Update_Bets;

  --------------------------------------------------------

  Start_Date          : constant Calendar2.Time_Type := (2016,03,16,0,0,0,0);
  One_Day             : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date        :          Calendar2.Time_Type := Start_Date;
  Stop_Date           : constant Calendar2.Time_Type := (2018,08,01,0,0,0,0);


begin
  Define_Switch
    (Cmd_Line,
     Sa_Betname'Access,
     Long_Switch => "--betname=",
     Help        => "betname for equity");




  Getopt (Cmd_Line);  -- process the command line




  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");

  Debug("Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database_home","host",""),
     Port     => Ini.Get_Value("database_home","port", 5432),
     Db_Name  => Ini.Get_Value("database_home","name",""),
     Login    => Ini.Get_Value("database_home","username",""),
     Password => Ini.Get_Value("database_home","password",""));
  Debug("db Connected");


--    Date_Loop2 : loop
--      T.Start;
--      Log("start fill maps");
--      Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse);
--      Log("start process maps");
--      Betlist.Clear;
--      Read_Bets(List => Betlist, Date => Current_Date);
--      T.Commit;
--
--      T.Start;
--      Update_To_Place_Market(Betlist);
--      T.Commit;
--      Current_Date := Current_Date + One_Day;
--      exit Date_Loop2 when Current_Date = Stop_Date;
--    end loop Date_Loop2;
--
--    Sql.Close_Session;
--    return;



  Date_Loop : loop
    T.Start;
    Log("start fill maps");
    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse);
    Log("start process maps");
    Betlist.Clear;
    Read_Bets(List => Betlist, Date => Current_Date);
    T.Commit;

    declare
      Ok : Boolean := False;
    begin
      Market_Loop : for Market of Sim.Market_With_Data_List loop
        T.Start;
        -- is market betted on ?
        Ok := False;
        for B of Betlist loop
          if B.Marketid = Market.Marketid then
            Ok := True;
            exit;
          end if;
        end loop;

        if Ok then
          -- list of timestamps in this market
          declare
            Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);
          begin
            Loop_Ts : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              declare
                List     : Price_Histories.Lists.List := Timestamp_To_Prices_History_Map(Timestamp.To_String);
                Bra      : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
              begin
                Sort_Array(List => List, Bra  => Bra);
                Update_Bets(List => Betlist, --Date => Current_Date,
                            Bra => bra);
              end;
            end loop Loop_Ts; --  Timestamp
          end;
        end if; -- Market_type(1..3) = PLC
        T.Commit;
      end loop Market_Loop;
    end;

    Current_Date := Current_Date + One_Day;
    exit when Current_Date = Stop_Date;
  end loop Date_Loop;

  Sql.Close_Session;



end Update_Bet_Outcome;
