with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Characters.Handling; use Ada.Characters.Handling;

with Text_Io;

with Gnat; use Gnat;
with Gnat.Awk;

with Logging; use Logging;
with Stacktrace;
with Bot_System_Number;
with Calendar2; use Calendar2;
with Utils; use Utils;
with Bot_Svn_Info;
with Sql;

package body Sim is

  package EV renames Ada.Environment_Variables;
  package AD renames Ada.Directories;

  Select_Sampleids_In_One_Market,
  Select_Sampleids_In_One_Market_2,
  Select_Event_In_One_Market,
  Select_Prices_In_One_Market,
  Select_Race_Winner_In_One_Market,
  Select_All_Markets_Horse,
  Select_All_Markets_Hound,
  Select_Pricets_In_A_Market,
  Select_All_Win_Markets,
  --Select_All_Place_Markets,
  Select_Pricets_For_Market : Sql.Statement_Type;

  Current_Market                : Markets.Market_Type := Markets.Empty_Data;
  Global_Price_During_Race_List : Price_Histories.Lists.List;

  Global_Current_Pricets: Calendar2.Time_Type := Calendar2.Time_Type_First ;
  package Pricets_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Calendar2.Time_Type);
  Pricets_List          : Pricets_List_Pack.List;

  Object          : constant String := "Sim.";
  Min_Num_Samples : constant Ada.Containers.Count_Type := 50;
--  Global_Race_Times_Filled : Boolean := False;

  use type Ada.Containers.Count_Type;

  ----------------------------------------------------------

  function Is_Race_Winner(Runner               : Runners.Runner_Type;
                          Marketid             : Marketid_Type)
                          return Boolean is
  begin
    return Is_Race_Winner(Runner.Selectionid, Marketid);
  end Is_Race_Winner;

  function Is_Race_Winner(Selectionid          : Integer_4;
                          Marketid             : Marketid_Type)
                          return Boolean is
    Service : constant String := "Is_Race_Winner";
  begin
    -- Log("Is_Race_Winner" & Selectionid'Img & " " & Marketid );

    -- Marketid_Winner_Map.Element((Marketid)) is a list of winning Arunners
    for R of Winners_Map.Element(Marketid) loop
      if Selectionid = R.Selectionid then
        return True;
      end if;
    end loop;
    return False;
  exception
    when Constraint_Error =>
      Log(Object & Service, "Key not in map '" & Marketid & "'");
      return False;
  end Is_Race_Winner;

  ----------------------------------------------------------
  procedure Get_Market_Prices(Market_Id  : in     Marketid_Type;
                              Market     : in out Markets.Market_Type;
                              Animal     : in     Animal_Type;
                              Price_List : in out Prices.Lists.List;
                              In_Play    :    out Boolean) is
    Service : constant String := "Get_Market_Prices";
    --Eos : Boolean := False;
    use type Markets.Market_Type;
    --Price_During_Race_Data : Price_Histories.Price_History_Type;
    Price_Data : Prices.Price_Type;
   -- T : Sql.Transaction_Type;
    Start,
    Stop,
    Ts      : Calendar2.Time_Type := Calendar2.Time_Type_First ;
  begin
   -- Log(Object & Service, "start");
    In_Play := True;
    -- Log(Object & Service, "Marketid '" & Market_Id & "' Current_Market = Table_Amarkets.Empty_Data " & Boolean'image(Current_Market = Table_Amarkets.Empty_Data));
    -- trigg for a new market
    if Current_Market = Markets.Empty_Data then
       --reset the fifo for ny race
      Log(Object & Service, "clear Fifo");
      for I in Num_Runners_Type loop
        Fifo(I).Clear;
      end loop;
      Global_Current_Pricets := Calendar2.Time_Type_First ;
      Pricets_List.Clear;
      Global_Price_During_Race_List.Clear;
      Log(Object & Service, "set Current_Market");
      Current_Market := Market;
      Log(Object & Service, "start Read_Marketid '" & Market_Id & "'");
      Sim.Read_Marketid(Marketid => Market_Id, Animal => Animal, List => Global_Price_During_Race_List) ;
      Log(Object & Service, "done Read_Marketid '" & Market_Id & "' len" & Global_Price_During_Race_List.Length'Img);
      -- get a list of unique ts in the race i/Users/bnl/svn/botstart/bot-1-0/history/data/ai/plc/rewards/1.123631656.datn order
      for Item of Global_Price_During_Race_List loop
        if Item.Pricets /= Ts then
          Pricets_List.Append(Item.Pricets);
          Ts := Item.Pricets;
          --set first value for next loop in else below
          if Global_Current_Pricets = Calendar2.Time_Type_First then
            Global_Current_Pricets := Item.Pricets ; -- start of this race
            Start := Item.Pricets;
          end if;
        end if;
        Stop := Item.Pricets;
      end loop;
      Log(Object & Service, "len Pricets_List" & Pricets_List.Length'Img & "start/stop " & Start.To_String & "/" & Stop.To_String);
    else
      if not Pricets_List.Is_Empty then
        Global_Current_Pricets := Pricets_List.First_Element;
        Pricets_List.Delete_First;
      else
        Move("CLOSED",Market.Status);
        Log(Object & Service, "reset Current_Market");
        Current_Market := Markets.Empty_Data;
       -- Log(Object & Service, "stop 1");
        return;
      end if;
    end if;

    -- optimization :
    -- 1 remove used entries
    -- 2 exit when done

    declare
      Have_Been_Inside : Boolean := False;
    begin
      for Race_Data of Global_Price_During_Race_List loop
       -- Log(Object & Service, "testing for Price_Data " & Global_Current_Pricets.To_String & "---" & Race_Data.To_String );
        if Race_Data.Pricets = Global_Current_Pricets then
          Have_Been_Inside := True;
          Price_Data := (
                         Marketid     => Race_Data.Marketid,
                         Selectionid  => Race_Data.Selectionid,
                         Pricets      => Race_Data.Pricets,
                         Status       => Race_Data.Status,
                         Totalmatched => Race_Data.Totalmatched,
                         Backprice    => Race_Data.Backprice,
                         Layprice     => Race_Data.Layprice,
                         Ixxlupd      => Race_Data.Ixxlupd,
                         Ixxluts      => Race_Data.Ixxluts
                        );
          Price_List.Append(Price_Data);
          --Log(Object & Service, "appended Price_Data " & Price_Data.To_String);

        elsif Have_Been_Inside then
          exit;
        end if;
      end loop;
    end;
    Move("OPEN",Market.Status);

  --  Log(Object & Service, "stop 2");

  end Get_Market_Prices;

  ----------------------------------------------------------------------
  procedure Place_Bet (Bet_Name         : in     Betname_Type;
                       Market_Id        : in     Marketid_Type;
                       Side             : in     Bet_Side_Type;
                       Runner_Name      : in     Runnername_Type;
                       Selection_Id     : in     Integer_4;
                       Size             : in     Bet_Size_Type;
                       Price            : in     Bet_Price_Type;
                       Bet_Persistence  : in     Bet_Persistence_Type;
                       Bet_Placed       : in     Calendar2.Time_Type := Calendar2.Time_Type_First;
                       Bet              :    out Bets.Bet_Type) is
    pragma Unreferenced(Bet_Persistence);

    Execution_Report_Status        : String (1..50)  :=  (others => ' ') ;
    Execution_Report_Error_Code    : String (1..50)  :=  (others => ' ') ;
    Instruction_Report_Status      : String (1..50)  :=  (others => ' ') ;
    Instruction_Report_Error_Code  : String (1..50)  :=  (others => ' ') ;
    Order_Status                   : String (1..50)  :=  (others => ' ') ;

    Bet_Id        : Integer_8 := 0;
    Now           : Calendar2.Time_Type := Calendar2.Clock;
    Side_String   : Bet_Side_String_Type := (others => ' ');
    Market        : Markets.Market_Type;
    Eos           : Boolean := False;
    Local_Bet_Placed  :  Calendar2.Time_Type := Bet_Placed;
  begin
    if Local_Bet_Placed = Calendar2.Time_Type_First then
      Local_Bet_Placed := Now;
    end if;

    Move(Side'Img, Side_String);
    Market.Marketid := Market_Id;
    Market.Read(Eos);
    if Eos Then
      raise Constraint_Error with "no such market " & Market.To_String;
    end if;

    Bet_Id := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
    Move( "EXECUTION_COMPLETE", Order_Status);
    Move( "SUCCESS", Execution_Report_Status);
    Move( "SUCCESS", Execution_Report_Error_Code);
    Move( "SUCCESS", Instruction_Report_Status);
    Move( "SUCCESS", Instruction_Report_Error_Code);

    Bet := (
            Betid          => Bet_Id,
            Marketid       => Market_Id,
            Betmode        => Bot_Mode(Simulation),
            Powerdays      => 0,
            Selectionid    => Selection_Id,
            Reference      => (others         => '-'),
            Size           => Fixed_Type(Size),
            Price          => Fixed_Type(Price),
            Side           => Side_String,
            Betname        => Bet_Name,
            Betwon         => False,
            Profit         => 0.0,
            Status         => Order_Status, -- ??
            Exestatus      => Execution_Report_Status,
            Exeerrcode     => Execution_Report_Error_Code,
            Inststatus     => Instruction_Report_Status,
            Insterrcode    => Instruction_Report_Error_Code,
            Startts        => Market.Startts,
            Betplaced      => Local_Bet_Placed,
            Pricematched   => Fixed_Type(Price),
            Sizematched    => Fixed_Type(Size),
            Runnername     => Runner_Name,
            Fullmarketname => Market.Marketname,
            Svnrevision    => Bot_Svn_Info.Revision,
            Ixxlupd        => (others         => ' '), --set by insert
            Ixxluts        => Now              --set by insert
           );
  end Place_Bet;
  -----------------------------------------------------------------------

  procedure Filter_List(Price_List, Avg_Price_List : in out Prices.Lists.List; Alg : Algorithm_Type := None)  is
  begin
    case Alg is
      when None =>
        Avg_Price_List := Price_List.Copy;
        return;
      when Avg =>
        --Log ("Filter_List : start Price_List.Length" & Price_List.Length'Img);
        for S of Price_List loop
        -- find my index in the array
          Num_Run: for I in Num_Runners_Type'Range loop

            if S.Selectionid = Fifo(I).Selectionid then
              -- insert elements at bottom and remove from top
              -- check if list needs trim
              loop
                exit when Fifo(I).One_Runner_Sample_List.Length <= Min_Num_Samples;
                Fifo(I).One_Runner_Sample_List.Delete_First;
              end loop;
              -- append the new value
              Fifo(I).One_Runner_Sample_List.Append(S);

              if Fifo(I).One_Runner_Sample_List.Length >= Min_Num_Samples then
              -- recalculate the avg values
                declare
                  Backprice,Layprice : Fixed_Type := 0.0;
                  Sample             : Prices.Price_Type;
                  Cnt                : Natural := 0;
                begin
                  for S2 of Fifo(I).One_Runner_Sample_List loop
                    Backprice := Backprice + S2.Backprice;
                    Layprice := Layprice + S2.Layprice;
                    Sample := S2; -- save some data
                    Cnt := Cnt +1 ;
                  --  Log ("Filter_List Cnt : " & Cnt'Img & Sample.To_String );
                  end loop;
                  Sample.Backprice := Backprice / Fixed_Type(Fifo(I).One_Runner_Sample_List.Length);
                  Sample.Layprice := Layprice / Fixed_Type(Fifo(I).One_Runner_Sample_List.Length);
                  Avg_Price_List.Append(Sample);
                 -- Log ("Filter_List : avg " & Sample.To_String );
                end;
              end if;
              exit Num_Run;
            end if;
          end loop Num_Run;
        end loop;
    end case;
   -- Log ("Filter_List : done" );
  end Filter_List;
  -------------------------------

  procedure Clear(F : in out Fifo_Type) is
  begin
    F.Selectionid    := 0;
    F.Avg_Lay_Price  := 0.0;
    F.Avg_Back_Price := 0.0;
    F.In_Use         := False;
    F.Index          := Num_Runners_Type'First;
    F.One_Runner_Sample_List.Clear;
  end Clear;


  procedure Read_Marketid (Marketid : in Marketid_Type;
                           Animal   : in Animal_Type;
                           List     : out Price_Histories.Lists.List) is
  --  Service : constant String := "Read_Marketid";
    Prices_History_Data : Price_Histories.Price_History_Type;
    Filename            : String := "markets/" & "win_" & Marketid & ".dat";
    T                   : Sql.Transaction_Type;
    Eos                 : Boolean := False;
    package Serializer is new Disk_Serializer(Price_Histories.Lists.List,Animal);
  begin

    if not Serializer.File_Exists(Filename) then
    --  Log(Object & Service, "Filename '" & Filename & "' does NOT exist. Read from DB and create");
      T.Start;
      Select_Sampleids_In_One_Market.Prepare(
                                             "select * " &
                                               "from APRICESHISTORY " &
                                               "where MARKETID = :MARKETID " &
                                               "order by PRICETS" ) ;

      Select_Sampleids_In_One_Market.Set("MARKETID", Marketid);
      Select_Sampleids_In_One_Market.Open_Cursor;
      loop
        Select_Sampleids_In_One_Market.Fetch(Eos);
        exit when Eos;
        Prices_History_Data := Price_Histories.Get(Select_Sampleids_In_One_Market);
        List.Append(Prices_History_Data);
      end loop;
      Select_Sampleids_In_One_Market.Close_Cursor;
      T.Commit;

      Serializer.Write_To_Disk(List, Filename);
    else
      Serializer.Read_From_Disk(List, Filename);
    end if;

  end Read_Marketid;
  -------------------------------------------------------------------------


  procedure Read_Marketid_Selectionid(Marketid    : in     Marketid_Type;
                                      Selectionid : in     Integer_4;
                                      Animal      : in     Animal_Type;
                                      List        :    out Price_Histories.Lists.List) is
  --  Service : constant String := "Read_Marketid";
    Price_History_Data : Price_Histories.Price_History_Type;
    Filename           : String := "markets_selid/" & Marketid & "_" & Trim(Selectionid'Img) & ".dat";
    T                  : Sql.Transaction_Type;
    Eos                : Boolean := False;
    package Serializer is new Disk_Serializer(Price_Histories.Lists.List,Animal);
  begin

    if not Serializer.File_Exists(Filename) then
    --  Log(Object & Service, "Filename '" & Filename & "' does NOT exist. Read from DB and create");
      T.Start;
      Select_Sampleids_In_One_Market_2.Prepare(
                                               "select * " &
                                                 "from APRICESHISTORY " &
                                                 "where MARKETID = :MARKETID " &
                                                 "and SELECTIONID = :SELECTIONID " &
                                                 "order by PRICETS" ) ;

      Select_Sampleids_In_One_Market_2.Set("MARKETID", Marketid);
      Select_Sampleids_In_One_Market_2.Set("SELECTIONID", Selectionid);

      Select_Sampleids_In_One_Market_2.Open_Cursor;
      loop
        Select_Sampleids_In_One_Market_2.Fetch(Eos);
        exit when Eos;
        Price_History_Data := Price_Histories.Get(Select_Sampleids_In_One_Market_2);
        List.Append(Price_History_Data);
      end loop;
      Select_Sampleids_In_One_Market_2.Close_Cursor;
      T.Commit;
      Serializer.Write_To_Disk(List, Filename);
    else
      Serializer.Read_From_Disk(List, Filename);
    end if;

  end Read_Marketid_Selectionid;
  -------------------------------------------------------------------------

  procedure Create_Runner_Data(Price_List : in Prices.Lists.List;
                               Alg        : in Algorithm_Type;
                               Is_Winner  : in Boolean;
                               Is_Place   : in Boolean ) is
    F         : Text_Io.File_Type;
    Indicator : String(1..3) := (others => ' ');
    Placement : String(1..3) := (others => ' ');
  begin
    case Alg is
      when None => Indicator := "nor";
      when Avg  => Indicator := "avg";
    end case;
    -- many runners 1 value only

    if Is_Winner then
      Placement  := "win";
    elsif Is_Place then
      Placement  := "plc";
    else
      Placement  := "los";
    end if;

    for Runner of Price_List loop
      declare
        Filename : String := Skip_All_Blanks(Ev.Value("BOT_SCRIPT") & "/plot/race_price_runner_data/" &
                                               Runner.Marketid & "_" &
                                               Runner.Selectionid'Img & "_" &
                                               Indicator & "_" &
                                               Placement & ".dat");
      begin
        if not AD.Exists(Filename) then
          Text_Io.Create(F, Text_Io.Out_File,    Filename);
        else
          Text_Io.Open  (F, Text_Io.Append_File, Filename);
        end if;
        Text_IO.Put_Line(F, Runner.Pricets.To_String & " | " &
                           Runner.Marketid & " | " &
                           Runner.Selectionid'Img & " | " &
                           Trim(Runner.Status) & " | " &
                           F8_Image(Runner.Backprice) & " | " &
                           F8_Image(Runner.Layprice) );
        Text_Io.Close(F);
      end;
    end loop;
  end Create_Runner_Data;
  -----------------------------------------------------------------

  function Get_Win_Market(Place_Market_Id : Marketid_Type) return Markets.Market_Type is
    Winner_Market   : Markets.Market_Type;
    Market          : Markets.Market_Type;
    Found           : Boolean := False;
  begin
    Market.Marketid := Place_Market_Id;
    Market.Corresponding_Win_Market(Win_Market => Winner_Market,
                                    Found      => Found);
    --Log(Object & "Get_Win_Market", "plc= '" & Place_Market_Id & "' win = '" & Winner_Market.Marketid & "'");
    return Winner_Market;
  end Get_Win_Market;
  -----------------------------------------------------------------

  function Get_Place_Market(Winner_Market_Id : Marketid_Type) return Markets.Market_Type is
    Place_Market    : Markets.Market_Type;
    Market          : Markets.Market_Type;
    Found           : Boolean := False;
  begin
    Market.Marketid := Winner_Market_Id;
    Market.Corresponding_Place_Market(Place_Market => Place_Market,
                                      Found        => Found);
    --Log(Object & "Get_Place_Market", "plc= '" & Place_Market.Marketid & "' win = '" & Winner_Market_Id & "'");
    return Place_Market;
  end Get_Place_Market;
  -----------------------------------------------------------------

  procedure Create_Bet_Data(Bet : in Bets.Bet_Type) is
    F         : Text_Io.File_Type with Warnings => Off;
    Indicator : String(1..3) := (others => ' ');
    Odds_Market : Markets.Market_Type ;
  begin
    case Bet.Betwon is
      when True  => Indicator := "won";
      when False => Indicator := "bad";
    end case;

    Log(Object & "Create_Bet_Data", Bet.To_String);
    Log(Object & "Create_Bet_Data", "odds= '" & Odds_Market.Marketid & "'");
    if Position(Bet.Betname, "PLC") > Integer(0) then
      Log(Object & "Create_Bet_Data", "was in PLC");
      Odds_Market := Get_Win_Market(Bet.Marketid);
    elsif Position(Bet.Betname, "WIN") > Integer(0) then
      Log(Object & "Create_Bet_Data", "was in WIN");
      Odds_Market.Marketid := Bet.Marketid;
    else
      Log(Object & "Create_Bet_Data", "was in neither WIN nor PLC");
      Odds_Market.Marketid := Bet.Marketid;
    end if;
    Log(Object & "Create_Bet_Data", "odds= '" & Odds_Market.Marketid & "'");

    declare
      Filename : String := Skip_All_Blanks(Ev.Value("BOT_SCRIPT") & "/plot/race_price_runner_data/" &
                                             Odds_Market.Marketid & "_" &
                                             Lower_Case(Bet.Betname) & "_" &
                                             Indicator & ".dat");
    begin
      if not AD.Exists(Filename) then
        Text_Io.Create(F, Text_Io.Out_File,    Filename);
      else
        Text_Io.Open  (F, Text_Io.Out_File, Filename);
      end if;
      Text_IO.Put_Line(F, Bet.Betplaced.To_String & " | " &
                         Bet.Marketid & " | " &
                         Bet.Selectionid'Img & " | " &
                         Bet.Side & " | " &
                         F8_Image(Bet.Pricematched) & " | " &
                         F8_Image(Bet.Sizematched) & " | " &
                         F8_Image(50.0) );
      Text_Io.Close(F);
    end;


  end Create_Bet_Data;
  --------------------------------------------------------------------------------------------
-- start lay_during_race2
  -------------------------------------------------------------------------
  procedure Read_All_Markets(Date   : in     Calendar2.Time_Type;
                             Animal : in     Animal_Type;
                             List   :    out Markets_Pack.List) is
  --  Service  : constant String := "Read_All_Markets";
    T        : Sql.Transaction_Type;
    Eos,Eos2 : Boolean := False;
    Filename : String := Date.String_Date_ISO & "/all_market_ids.dat";
    Marketid : Marketid_Type := (others => ' ');
    package Serializer is new Disk_Serializer(Markets_Pack.List,Animal);
    Market   : Markets.Market_Type;
    Date1    :  Calendar2.Time_Type := Date;
    Date2    :  Calendar2.Time_Type := Date;
  begin
    Date1.Hour := 0;
    Date1.Minute := 0;
    Date1.Second := 0;
    Date1.Millisecond := 0;

    Date2.Hour := 23;
    Date2.Minute := 59;
    Date2.Second := 59;
    Date2.Millisecond := 999;

    List.Clear;
    if not Serializer.File_Exists(Filename) then
      T.Start;
      case Animal is
        when Horse =>
          Select_All_Markets_Horse.Prepare (
                                            "select M.MARKETID,STARTTS " &
                                              "from APRICESHISTORY H, AMARKETS M, AEVENTS E " &
                                              "where true " &
                                              "and H.MARKETID = M.MARKETID " &
                                              "and E.EVENTID = M.EVENTID " &
                                              "and E.EVENTTYPEID = 7 " &
                                              "and M.MARKETTYPE in ('PLACE', 'WIN') " &
                                              "and M.STARTTS >= :DATE1 " &
                                              "and M.STARTTS <= :DATE2 " &
                                              "group by M.MARKETID,M.STARTTS " &
                                              "order by M.STARTTS,M.MARKETID");
          Select_All_Markets_Horse.Set ("DATE1", Date1);
          Select_All_Markets_Horse.Set ("DATE2", Date2);
          Select_All_Markets_Horse.Open_Cursor;
          loop
            Select_All_Markets_Horse.Fetch (Eos);
            exit when Eos;
            Select_All_Markets_Horse.Get ("MARKETID", Marketid);
            Market.Marketid := Marketid;
            Market.Read (Eos2); -- must exist, just read id
            List.Append (Market);
          end loop;
          Select_All_Markets_Horse.Close_Cursor;

        when Hound =>
          Select_All_Markets_Hound.Prepare (
                                            "select M.MARKETID " &
                                              "from AMARKETS M, AEVENTS E " &
                                              "where true " &
                                              "and M.MARKETTYPE in ('PLACE', 'WIN') " &
                                              "and E.EVENTID = M.EVENTID " &
                                              "and E.EVENTTYPEID = 4339 " &
                                              "and M.STARTTS::date = :DATE " &
                                              "order by M.STARTTS");
          Select_All_Markets_Hound.Set ("DATE", Date.String_Date_ISO );
          Select_All_Markets_Hound.Open_Cursor;
          loop
            Select_All_Markets_Hound.Fetch (Eos);
            exit when Eos;
            Select_All_Markets_Hound.Get ("MARKETID", Marketid);
            Market.Marketid := Marketid;
            Market.Read (Eos2); -- must exist, just read id
            List.Append (Market);
          end loop;
          Select_All_Markets_Hound.Close_Cursor;
        when Human => null;
      end case;
      T.Commit;
      Serializer.Write_To_Disk(List, Filename);
    else
      Serializer.Read_From_Disk(List, Filename);
    end if;
  end Read_All_Markets;
  -------------------------------------------------------------------------

  procedure Fill_Marketid_Pricets_Map (Market_With_Data_List      : in     Markets_Pack.List;
                                       Date                       : in     Calendar2.Time_Type;
                                       Animal                     : in     Animal_Type;
                                       Marketid_Pricets_Map       :    out Marketid_Pricets_Maps.Map) is
    Eos          : Boolean := False;
    Pricets_List : Timestamp_Pack.List;
    Filename     : String := Date.String_Date_ISO & "/marketid_pricets_map.dat";
    Ts           : Calendar2.Time_Type := Calendar2.Time_Type_First;
    T            : Sql.Transaction_Type;
    package Serializer is new Disk_Serializer(Marketid_Pricets_Maps.Map,Animal);
  begin
    Marketid_Pricets_Map.Clear;
    if not Serializer.File_Exists(Filename) then
      T.Start;
      Select_Pricets_In_A_Market.Prepare(
                                         "select distinct(PRICETS) " &
                                           "from APRICESHISTORY " &
                                           "where MARKETID = :MARKETID " &
                                           "and STATUS <> 'REMOVED' "  &
                                           "order by PRICETS"  ) ;
      for Market of Market_With_Data_List loop
        Select_Pricets_In_A_Market.Set("MARKETID", Market.Marketid) ;
        Select_Pricets_In_A_Market.Open_Cursor;
        Pricets_List.Clear;
        loop
          Select_Pricets_In_A_Market.Fetch(Eos);
          exit when Eos;
          Select_Pricets_In_A_Market.Get(1,Ts);
          Pricets_List.Append(Ts);
        end loop;
        Select_Pricets_In_A_Market.Close_Cursor;
        Marketid_Pricets_Map.Insert(Market.Marketid, Pricets_List);
      end loop;
      T.Commit;

      Serializer.Write_To_Disk(Marketid_Pricets_Map, Filename);
    else
      Serializer.Read_From_Disk(Marketid_Pricets_Map, Filename);
    end if;

  end Fill_Marketid_Pricets_Map;
  -------------------------------------------------------------
  -------------------------------------------------------------

--    procedure Fill_Winners_Map (Market_List : in     Markets.Lists.List;
--                                Animal      : in     Animal_Type;
--                                Winners_Map :    out Marketid_Winner_Maps.Map ) is
--      Eos             : Boolean := False;
--      Filename : String := "all_winners_map.dat";
--      Runner_Data : Runners.Runner_Type;
--      Runner_List : Runners.Lists.List;
--      T : Sql.Transaction_Type;
--      package Serializer is new Disk_Serializer(Marketid_Winner_Maps.Map, Animal);
--    begin
--      Winners_Map.Clear;
--      if not Serializer.File_Exists(Filename) then
--        T.Start;
--        Select_Race_Winner_In_One_Market.Prepare(
--          "select * " &
--          "from ARUNNERS " &
--          "where MARKETID = :MARKETID " &
--          "and STATUS = 'WINNER' ") ;
--        for Market of Market_List loop
--          Runner_List.Clear;
--          Select_Race_Winner_In_One_Market.Set("MARKETID", Market.Marketid) ;
--          Select_Race_Winner_In_One_Market.Open_Cursor;
--          loop
--            Select_Race_Winner_In_One_Market.Fetch(Eos);
--            exit when Eos;
--            Runner_Data := Runners.Get(Select_Race_Winner_In_One_Market);
--            Runner_List.Append(Runner_Data);
--          end loop;
--          Select_Race_Winner_In_One_Market.Close_Cursor;
--          Winners_Map.Insert(Market.Marketid, Runner_List);
--        end loop;
--        T.Commit;
--        Serializer.Write_To_Disk(Winners_Map, Filename);
--      else
--        Serializer.Read_From_Disk(Winners_Map, Filename);
--      end if;
--    end Fill_Winners_Map;

  -------------------------------------------------------------

  procedure Fill_Winners_Map (Market_With_Data_List    : in     Markets_Pack.List;
                              Date                     : in     Calendar2.Time_Type;
                              Animal                   : in     Animal_Type;
                              Winners_Map              :    out Marketid_Winner_Maps.Map ) is
    Eos             : Boolean := False;
    Filename        : String := Date.String_Date_ISO & "/winners_map.dat";
    Runner_Data     : Runners.Runner_Type;
    Runner_List     : Runners.Lists.List;
    T               : Sql.Transaction_Type;
    package Serializer is new Disk_Serializer(Marketid_Winner_Maps.Map, Animal);
  begin
    Winners_Map.Clear;
    if not Serializer.File_Exists(Filename) then
      T.Start;
      Select_Race_Winner_In_One_Market.Prepare(
                                               "select * " &
                                                 "from ARUNNERS " &
                                                 "where MARKETID = :MARKETID " &
                                                 "and STATUS = 'WINNER' ") ;
      for Market of Market_With_Data_List loop
        Runner_List.Clear;
        Select_Race_Winner_In_One_Market.Set("MARKETID", Market.Marketid) ;
        Select_Race_Winner_In_One_Market.Open_Cursor;
        loop
          Select_Race_Winner_In_One_Market.Fetch(Eos);
          exit when Eos;
          Runner_Data := Runners.Get(Select_Race_Winner_In_One_Market);
          Runner_List.Append(Runner_Data);
        end loop;
        Select_Race_Winner_In_One_Market.Close_Cursor;
        Winners_Map.Insert(Market.Marketid, Runner_List);
      end loop;
      T.Commit;

      Serializer.Write_To_Disk(Winners_Map, Filename);
    else
      Serializer.Read_From_Disk(Winners_Map, Filename);
    end if;
  end Fill_Winners_Map;

  -------------------------------------------------------------

  procedure Fill_Prices_Map (Market_With_Data_List    : in     Markets_Pack.List;
                             Date                     : in     Calendar2.Time_Type;
                             Animal                   : in     Animal_Type;
                             Prices_Map               :    out Marketid_Prices_Maps.Map ) is
    Eos             : Boolean := False;
    Filename        : String := Date.String_Date_ISO & "/prices_map.dat";
    Price_Data      : Prices.Price_Type;
    Price_List      : Prices.Lists.List;
    T               : Sql.Transaction_Type;
    package Serializer is new Disk_Serializer (Marketid_Prices_Maps.Map, Animal);
  begin
    Prices_Map.Clear;
    if not Serializer.File_Exists(Filename) then
      T.Start;
      Select_Prices_In_One_Market.Prepare(
                                          "select * " &
                                            "from APRICES " &
                                            "where MARKETID = :MARKETID " &
                                            "order by SELECTIONID") ;
      for Market of Market_With_Data_List loop
        Price_List.Clear;
        Select_Prices_In_One_Market.Set("MARKETID", Market.Marketid) ;
        Select_Prices_In_One_Market.Open_Cursor;
        loop
          Select_Prices_In_One_Market.Fetch(Eos);
          exit when Eos;
          Price_Data := Prices.Get(Select_Prices_In_One_Market);
          Price_List.Append(Price_Data);
        end loop;
        Select_Prices_In_One_Market.Close_Cursor;
        Prices_Map.Insert(Market.Marketid, Price_List);
      end loop;
      T.Commit;

      Serializer.Write_To_Disk(Prices_Map, Filename);
    else
      Serializer.Read_From_Disk(Prices_Map, Filename);
    end if;
  end Fill_Prices_Map;
    -------------------------------------------------------------

  procedure Fill_Events_Map (Market_With_Data_List    : in     Markets_Pack.List;
                             Date                     : in     Calendar2.Time_Type;
                             Animal                   : in     Animal_Type;
                             Events_Map               :    out Eventid_Events_Maps.Map ) is
    Eos             : Boolean := False;
    Filename        : String := Date.String_Date_ISO & "/events_map.dat";
    Event_Data      : Events.Event_Type;
    Event_List      : Events.Lists.List;
    T               : Sql.Transaction_Type;
    package Serializer is new Disk_Serializer (Eventid_Events_Maps.Map, Animal);
  begin
    Events_Map.Clear;
    if not Serializer.File_Exists(Filename) then
      T.Start;
      Select_Event_In_One_Market.Prepare(
                                         "select * " &
                                           "from AEVENTS " &
                                           "where EVENTID = :EVENTID " &
                                           "order by OPENTS") ;
      for Market of Market_With_Data_List loop
        Event_List.Clear;
        Select_Event_In_One_Market.Set("EVENTID", Market.Eventid) ;
        Select_Event_In_One_Market.Open_Cursor;
        loop
          Select_Event_In_One_Market.Fetch(Eos);
          exit when Eos;
          Event_Data := Events.Get (Select_Event_In_One_Market);
          if not Events_Map.Contains (Key => Market.Eventid) then
            -- several markets points to 1 event
            Events_Map.Insert (Market.Eventid, Event_Data);
          end if;
        end loop;
        Select_Event_In_One_Market.Close_Cursor;
      end loop;
      T.Commit;

      Serializer.Write_To_Disk(Events_Map, Filename);
    else
      Serializer.Read_From_Disk(Events_Map, Filename);
    end if;
  end Fill_Events_Map;

  -----------------------------------------

  procedure Fill_Marketid_Runners_Pricets_Map (
                                               Market_With_Data_List                    : in     Markets_Pack.List;
                                               Marketid_Pricets_Map                     : in     Marketid_Pricets_Maps.Map;
                                               Date                                     : in     Calendar2.Time_Type;
                                               Animal                                   : in     Animal_Type;
                                               Marketid_Timestamp_To_Apriceshistory_Map :    out Marketid_Timestamp_To_Prices_History_Maps.Map) is
    Eos                             : Boolean := False;
    Apriceshistory_List             : Price_Histories.Lists.List;
    Price_History_Data              : Price_Histories.Price_History_Type;
    T                               : Sql.Transaction_Type;
    Cnt                             : Integer := 0;
    Timestamp_To_Apriceshistory_Map : Timestamp_To_Prices_History_Maps.Map;
    Filename                        : String := Date.String_Date_ISO & "/marketid_timestamp_to_apriceshistory_map.dat";
    package Serializer is new Disk_Serializer(Marketid_Timestamp_To_Prices_History_Maps.Map, Animal);
  begin
    Marketid_Timestamp_To_Apriceshistory_Map.Clear;
    if not Serializer.File_Exists(Filename) then
      T.Start;
      Select_Pricets_For_Market.Prepare(
                                        "select * " &
                                          "from APRICESHISTORY " &
                                          "where MARKETID = :MARKETID " &
                                          "and PRICETS = :PRICETS " &
                                          "and STATUS <> 'REMOVED' "  &
                                          "order by SELECTIONID"  ) ;
      for Market of Market_With_Data_List loop
        Cnt := Cnt + 1;
        Log("marketid '" & Market.Marketid & "' " & Cnt'Img & "/" & Market_With_Data_List.Length'Img );
        --Marketid_Pricets_Maps(Marketid) is a list of pricets
        for Pricets of Marketid_Pricets_Map(Market.Marketid) loop
          -- do rest here with marketid and pricets
          Select_Pricets_For_Market.Set("MARKETID", Market.Marketid) ;
          Select_Pricets_For_Market.Set("PRICETS", Pricets) ;
          Select_Pricets_For_Market.Open_Cursor;
          loop
            Select_Pricets_For_Market.Fetch(Eos);
            exit when Eos;
            Price_History_Data := Price_Histories.Get(Select_Pricets_For_Market);
            Apriceshistory_List.Append(Price_History_Data);
          end loop;
          Select_Pricets_For_Market.Close_Cursor;
          --Log("Insert Market.Marketid & _ & Pricets.To_String '" & Market.Marketid & "_" & Pricets.To_String & "'");
          Timestamp_To_Apriceshistory_Map.Insert(Pricets.To_String, Apriceshistory_List);
          Apriceshistory_List.Clear;
        end loop;
        Marketid_Timestamp_To_Apriceshistory_Map.Insert(Market.Marketid, Timestamp_To_Apriceshistory_Map);
        Timestamp_To_Apriceshistory_Map.Clear;
      end loop;  -- Market_With_Data_List
      T.Commit;

      Serializer.Write_To_Disk(Marketid_Timestamp_To_Apriceshistory_Map, Filename);
    else
      Serializer.Read_From_Disk(Marketid_Timestamp_To_Apriceshistory_Map, Filename);
    end if;
  end Fill_Marketid_Runners_Pricets_Map;
  -------------------------------------------------------------------


  procedure Fill_Win_Place_Map (Date          : in     Calendar2.Time_Type;
                                Animal        : in     Animal_Type;
                                Win_Place_Map :    out Win_Place_Maps.Map) is
    T               : Sql.Transaction_Type;
    Eos             : Boolean := False;
    Place_Marketid,
    Win_Marketid    : Marketid_Type := (others => ' ');
    Filename        : String := Date.String_Date_ISO & "/win_place_map.dat";
    package Serializer is new Disk_Serializer(Win_Place_Maps.Map, Animal);
  begin
    Win_Place_Map.Clear;
    if not Serializer.File_Exists(Filename) then
      T.Start;
      Select_All_Win_Markets.Prepare (
                                      "select distinct(M.MARKETID) " &
                                        "from APRICESHISTORY RP, AMARKETS M " &
                                        "where RP.MARKETID = M.MARKETID " &
                                        "and M.MARKETTYPE = 'WIN' " &
                                        "and STARTTS::date = :DATE " &
                                        "order by M.MARKETID");

      Select_All_Win_Markets.Set("DATE", Date.String_Date_ISO) ;
      Select_All_Win_Markets.Open_Cursor;
      loop
        Select_All_Win_Markets.Fetch(Eos);
        exit when Eos;
        Select_All_Win_Markets.Get(1,Win_Marketid);
        Place_Marketid := Get_Place_Market(Win_Marketid).Marketid;
        Win_Place_Map.Insert(Win_Marketid, Place_Marketid);
      end loop;
      Select_All_Win_Markets.Close_Cursor;
      T.Commit;
      Serializer.Write_To_Disk(Win_Place_Map, Filename);
    else
      Serializer.Read_From_Disk(Win_Place_Map, Filename);
    end if;

  end Fill_Win_Place_Map;

  -------------------------------------------------------------------

-- cannot have 3 results for same key ..
--    procedure Fill_Place_Win_Map (Date          : in     Calendar2.Time_Type;
--                                  Animal        : in     Animal_Type;
--                                  Place_Win_Map :    out Place_Win_Maps.Map) is
--      T               : Sql.Transaction_Type;
--      Eos             : Boolean := False;
--      Place_Marketid,
--      Win_Marketid    : Marketid_Type := (others => ' ');
--      Filename        : String := Date.String_Date_ISO & "/place_win_map.dat";
--      package Serializer is new Disk_Serializer(Place_Win_Maps.Map, Animal);
--    begin
--      Place_Win_Map.Clear;
--      if not Serializer.File_Exists(Filename) then
--        T.Start;
--        Select_All_Place_Markets.Prepare (
--                                          "select distinct(M.MARKETID) " &
--                                            "from APRICESHISTORY RP, AMARKETS M " &
--                                            "where RP.MARKETID = M.MARKETID " &
--                                            "and M.MARKETTYPE = 'PLACE' " &
--                                            "and STARTTS::date = :DATE " &
--                                            "order by M.MARKETID");
--
--        Select_All_Place_Markets.Set("DATE", Date.String_Date_ISO) ;
--        Select_All_Place_Markets.Open_Cursor;
--        loop
--          Select_All_Place_Markets.Fetch(Eos);
--          exit when Eos;
--          Select_All_Place_Markets.Get(1,Win_Marketid);
--          Win_Marketid := Get_Win_Market(Win_Marketid).Marketid;
--          Place_Win_Map.Insert(Place_Marketid, Win_Marketid);
--        end loop;
--        Select_All_Place_Markets.Close_Cursor;
--        T.Commit;
--        Serializer.Write_To_Disk(Place_Win_Map, Filename);
--      else
--        Serializer.Read_From_Disk(Place_Win_Map, Filename);
--      end if;
--    end Fill_Place_Win_Map;


  procedure Fill_Rewards_Map(Date   : in     Calendar2.Time_Type;
                             Animal : in     Animal_Type;
                             Side   : in     Bot_Types.Bet_Side_Type;
                             Rm     :    out Rewards_Maps.Map) is
    Soside : String_Object:= Create(Side'Img);

    Filename_Map        : String := Date.String_Date_ISO & "/" & Soside.Lower_Case & "_rewards_map.dat";
    package Serializer is new Disk_Serializer(Rewards_Maps.Map, Animal);

--      package Fnames is new Ada.Containers.Doubly_Linked_Lists(Unbounded_String);
--      Fname_List      : Fnames.List;
      Market_Type     : String := "win";
  begin
    RM.Clear;
    if not Serializer.File_Exists(Filename_Map) then

      --for all markets this date
      -- put filename in list

      Market_Loop : for Market of Sim.Market_With_Data_List loop

        Log("Fill_Rewards_Map", "Start " & Market.To_String) ;

        if Market.Markettype(1..3) = "PLA" then
          Market_Type := "plc";
        elsif Market.Markettype(1..3) = "WIN" then
          Market_Type := "win";
        else
          Market_Type := "xxx";
        end if;

        Scope_File: declare
          -- scope for parsing a single file
          Path            : String := Ev.Value("BOT_HISTORY") & "/data/ai/" & Market_Type & "/rewards/" & Soside.Lower_Case;
          Computer_File   : Awk.Session_Type;
          Filename        : String := Path & "/" & Market.Marketid & ".dat";
          Header_Seen     : Boolean := False;

          Map_Selectionid : array (2..17) of Integer_4 := (others => 0);

          Timestamps   :  array (2..17) of Timestamp_To_Reward_Maps.Map;
          Selectionids    :  Selectionid_Maps.Map;

          --use Map_Selectionid_Maps;
          Num_Runners : Integer := 0;
        begin
          --   Text_Io.Put_Line("marketid='" & Market.Marketid & "' " & Market.Startts.String_Date_Time_Iso & " " & Calendar2.Clock.String_Date_Time_Iso);

          Log("Fill_Rewards_Map", Filename) ;

          Awk.Set_Current (Computer_File);
          begin
            Awk.Open (Separators => "|", Filename   => Filename);
          exception
            when GNAT.AWK.File_Error =>
              Log("Fill_Rewards_Map", "File not Found, skipping " &  Filename) ;
              return;
          end;


          while not Awk.End_Of_File loop
            Awk.Get_Line;
           -- Log("Fill_Rewards_Map", Awk.Field(0)) ;

            if not Header_Seen then
              --treat headers, which col is what selectionid
              for I in 2 .. 17 loop
                --Ts(I).Clear;
                declare
                  Sel : Integer_4 := Integer_4'Value(Awk.Field(Awk.Count(I)));
                begin
                  Log("Fill_Rewards_Map",I'Img & " ->" & Sel'Img ) ;
                  if Sel > 0 then
                     Map_Selectionid(I) := Sel;
                  else
                    Num_Runners := I-1;
                    exit;
                  end if;
                end;
              end loop;
              Header_Seen := True;
            --  Log("Fill_Rewards_Map", Market.Marketid & "/" & "map-selids") ;

            else
              --treat data
              declare
                Ts_Val   : Calendar2.Time_Type := Market.Startts;
              begin
                Ts_Val.Hour := Hour_Type'Value(Awk.Field(1)(1..2));
                Ts_Val.Minute := Minute_Type'Value(Awk.Field(1)(4..5));
                Ts_Val.Second := Second_Type'Value(Awk.Field(1)(7..8));
                Ts_Val.Millisecond := Millisecond_Type'Value(Awk.Field(1)(10..12));

                for I in 2 .. Num_Runners loop
                  Timestamps(I).Insert(Key => Ts_Val.To_String, New_Item => Fixed_Type'Value(Awk.Field(Awk.Count(I))));
                end loop;
              end;
            end if;
          end loop;

          for I in 2 .. Num_Runners loop
            Selectionids.Insert(Key => Map_Selectionid(I), New_Item => Timestamps(I));
          end loop;

          Rm.Insert(Key => Market.Marketid, New_Item => Selectionids);

          Log("Fill_Rewards_Map done =  ", Market.Marketid);

--            Log("Fill_Rewards_Map, Rm(""1.123648137"")(8554946)(""2016-03-16 14:22:12.762"") =  ",
--                Fixed_Type'Image(Rm("1.123648137")(8554946)("2016-03-16 14:22:12.762")));

          Awk.Close (Computer_File);
        end Scope_File;

      end loop Market_Loop;
      Serializer.Write_To_Disk(Rm, Filename_Map);
    else
      Serializer.Read_From_Disk(Rm, Filename_Map);
    end if;
  end Fill_Rewards_Map;

  -------------------------------------------------------------
  procedure Fill_Wints_Placets_Map (Date              : in     Calendar2.Time_Type;
                                    Animal            : in     Animal_Type;
                                    Wints_Placets_Map :    out Wints_Placets_Maps.Map) is
    Place_Marketid  : Marketid_Type := (others => ' ');
    Filename        : String := Date.String_Date_Iso & "/wints_placets_map.dat";
    package Serializer is new Disk_Serializer(Wints_Placets_Maps.Map, Animal);
    Plc_Found       : Boolean := False;
  begin
    Wints_Placets_Map.Clear;
    if not Serializer.File_Exists(Filename) then

      Loop_Market_Sim    : for Market of Sim.Market_With_Data_List loop

        if Market.Markettype(1..3) = "WIN" then
          begin
            Place_Marketid := Sim.Win_Place_Map(Market.Marketid);
            Plc_Found := Place_Marketid(1) /= ' ';
          --  Log("Fill_Wints_Placets_Map", Market.Marketid & " -> " & Place_Marketid);
          exception
            when Constraint_Error => Plc_Found := False;
          end ;

          if Plc_Found then
            declare
              Plc_Map : Sim.Marketid_Pricets_Maps.Map := Sim.Marketid_Pricets_Map.Copy; -- make deep copy first
            begin
              Loop_Timestamp_Sim_Win : for Timestamp_Win of Sim.Marketid_Pricets_Map(Market.Marketid) loop
                begin
                  Loop_Timestamp_Sim_Plc : for Timestamp_Plc of Plc_Map(Place_Marketid) loop
                    -- find first plc than is larger than win
                    if Timestamp_Plc > Timestamp_Win then
                      begin
                        Wints_Placets_Map.Insert(Timestamp_Win.To_String, Timestamp_Plc.To_String);
                      exception
                        when Constraint_Error => null;
                      end;
                      exit Loop_Timestamp_Sim_Plc;
                    end if;
                  end loop Loop_Timestamp_Sim_Plc;
                exception
                  when Constraint_Error => null;
                end;
              end loop Loop_Timestamp_Sim_Win;
            exception
              when Constraint_Error => null;
            end;
          end if;
        end if;
      end loop Loop_Market_Sim;

      Serializer.Write_To_Disk(Wints_Placets_Map, Filename);
    else
      Serializer.Read_From_Disk(Wints_Placets_Map, Filename);
    end if;
  end Fill_Wints_Placets_Map;


  -------------------------------------------------------------

  package body Disk_Serializer is
    --------------------------------------------------------
    Ani  : String := Lower_Case(Animal'Img);
    Path : String := Ev.Value("BOT_HISTORY") & "/data/streamed_objects/" & Ani & "/";
    --Path : String := "/mnt/samsung1gb/data/streamed_objects/";

    function File_Exists(Filename : String) return Boolean is
     -- Service : constant String := "File_Exists";
      File_On_Disk : String := Path & Filename;
      File_Exists  : Boolean := AD.Exists(File_On_Disk) ;
      Dir          : String := Ad.Containing_Directory(File_On_Disk);
      Dir_Exists   : Boolean := AD.Exists(Dir) ;
      use type AD.File_Size;
    begin
      if not Dir_Exists then
        Ad.Create_Directory(Dir);
      end if;
    --  Log(Object & Service, "Exists: " & Exists'Img);
      if File_Exists then
        File_Exists := AD.Size (File_On_Disk) > 5;
      end if;
      return File_Exists;
    end File_Exists;
    ---------------------------------------------------------------
    procedure Write_To_Disk (Container : in Data_Type; Filename : in String) is
      File         : Ada.Streams.Stream_IO.File_Type with Warnings => Off;
      Stream       : Ada.Streams.Stream_IO.Stream_Access;
      File_On_Disk : String := Path & Filename;
    --  Service : constant String := "Write_To_Disk";
    begin
    --  Log(Object & Service, "write to file '" & Filename & "'");
      Ada.Streams.Stream_IO.Create
        (File => File,
         Name => File_On_Disk,
         Mode => Ada.Streams.Stream_IO.Out_File);
      Stream := Ada.Streams.Stream_IO.Stream (File);
      Data_Type'Write(Stream, Container);
      Ada.Streams.Stream_IO.Close(File);
    --  Log(Object & Service, "Stream written to file " & Filename);
    end Write_To_Disk;
    --------------------------------------------------------
    procedure Read_From_Disk (Container : in out Data_Type; Filename : in String) is
      File         : Ada.Streams.Stream_IO.File_Type with Warnings => Off;
      Stream       : Ada.Streams.Stream_IO.Stream_Access;
      File_On_Disk : String := Path & Filename;
      Service      : constant String := "Read_From_Disk";
    begin
         -- Log(Object & Service, "read from file '" & Filename & "'");
--      loop
--        exit when not Ad.Exists("/dev/shm/block.dat") ;
--        Log(Object & Service, "Blocking file detected - wait");
--        delay 0.2;
--      end loop;

      Ada.Streams.Stream_IO.Open
        (File => File,
         Name => File_On_Disk,
         Mode => Ada.Streams.Stream_IO.In_File);
      Stream := Ada.Streams.Stream_IO.Stream (File);
      Data_Type'Read(Stream, Container);
      Ada.Streams.Stream_IO.Close(File);
    --  Log(Object & Service, "Stream read from file " & Filename);
    end Read_From_Disk;
    --------------------------------------------------------
  end Disk_Serializer;
  ----------------------------------------------------------

  procedure Fill_Data_Maps (Date   : in Calendar2.Time_Type;
                            Animal : in Animal_Type;
                            Rewards : Boolean := True;
  --                          Racetimes : Boolean := True;
                            Race_Prices : Boolean := True) is
  begin
    Log("fill maps with Date " & Date.String_Date_ISO & " for animal " &  Animal'Img);
    Log("fill list with all valid marketids" );
    Read_All_Markets(Date, Animal, Market_With_Data_List);
    Log("Found:" & Market_With_Data_List.Length'Img );

    if Race_Prices then
      Log("fill map with all pricets for a marketid ");
      Fill_Marketid_Pricets_Map(Market_With_Data_List, Date, Animal, Marketid_Pricets_Map);
      Log("Found:" & Marketid_Pricets_Map.Length'Img );

      Log("fill map with map of timestamp list for all marketids ");
      Fill_Marketid_Runners_Pricets_Map (Market_With_Data_List,
                                         Marketid_Pricets_Map,
                                         Date,
                                         Animal,
                                         Marketid_Timestamp_To_Prices_History_Map) ;
      Log("Found:" & Marketid_Timestamp_To_Prices_History_Map.Length'Img );
    end if;

    Log("fill map winners ");
    Fill_Winners_Map(Market_With_Data_List, Date, Animal, Winners_Map );
    Log("Found:" & Winners_Map.Length'Img );

    Log("fill map Prices_Map ");
    Fill_Prices_Map(Market_With_Data_List, Date, Animal, Prices_Map );
    Log("Found:" & Prices_Map.Length'Img );

    Log("fill map Events_Map ");
    Fill_Events_Map(Market_With_Data_List, Date, Animal, Events_Map );
    Log("Found:" & Events_Map.Length'Img );

    Log("fill map Win/Place markets ");
    Fill_Win_Place_Map(Date, Animal, Win_Place_Map);
    Log("Found:" & Win_Place_Map.Length'Img );

    if Rewards then
      Log("fill Rewards map");
      Fill_Rewards_Map(Date, Animal, Back, Rewards_Map);
      Log("Found:" & Rewards_Map.Length'Img );
    end if;

--    if Racetimes then
--      Log("fill Race_Times map");
--      Fill_Race_Times(Animal, Racetime_Map);
--      Log("Found:" & Racetime_Map.Length'Img );
--    end if;

    Log("fill map Wints/Placets timestamps ");
    Fill_Wints_Placets_Map (Date, Animal, Wints_Placets_Map);
    Log("Found:" & Wints_Placets_Map.Length'Img );


  end Fill_Data_Maps;
  ------------------------------------------------------------------


  function Get_Place_Price(Win_Data : Price_Histories.Price_History_Type) return Price_Histories.Price_History_Type is
    Place_Marketid : Marketid_Type := (others => ' ');
  begin
    Place_Marketid := Win_Place_Map(Win_Data.Marketid);
    --Log("Get_Place_Price '" & Place_Marketid & "'");
    if Place_Marketid /= Marketid_Type'(others => ' ') then
      declare
        Timestamp_To_Apriceshistory_Map : Timestamp_To_Prices_History_Maps.Map :=
                                            Marketid_Timestamp_To_Prices_History_Map(Place_Marketid);
      begin
        for Timestamp of Marketid_Pricets_Map(Place_Marketid) loop
          if Timestamp >= Win_Data.Pricets + (0,0,0,1,0) and then  -- after and then within 1 second match must ooccur
            Timestamp <= Win_Data.Pricets + (0,0,0,2,0) then
            declare
              List : Price_Histories.Lists.List :=
                       Timestamp_To_Apriceshistory_Map(Timestamp.To_String);
            begin
              for Data of List loop
                if Data.Selectionid = Win_Data.Selectionid then
                  return Data;
                end if;
              end loop;
            end;
          end if;
        end loop;
      end;
    end if;
    Log("Get_Place_Price, Win/plc marketids '" & Win_Data.Marketid & "/" & Place_Marketid & "' no place prices found");
    return Price_Histories.Empty_Data;
  exception
    when E: Constraint_Error =>
      Stacktrace.Tracebackinfo(E);
      return Price_Histories.Empty_Data;
  end Get_Place_Price;

   ------------------------------------------------------------------


--  procedure Delete_Shared_Mem (Date   : in Calendar2.Time_Type;
--                               Animal : in Animal_Type) is
--    Ani  : String := Lower_Case(Animal'Img);
--    Path : String := "/dev/shm/streamed_objects/" & Ani & "/";
--  begin
--    if Date.Month < Month_Type(10) and Date.Day < Day_Type(10) then
--      declare
--        Dir_On_Disk : String := Path & Trim(Date.Year'Img) & "-0" & Trim(Date.Month'Img) & "-0" & Trim(Date.Day'Img);
--      begin
--        Log("Delete_Shared_Mem","will delete " & Dir_On_Disk );
--        if AD.Exists(Dir_On_Disk) then
--          AD.Delete_Tree(Dir_On_Disk);
--          Log("Delete_Shared_Mem","Deleted " & Dir_On_Disk );
--        end if;
--      end;
--    elsif Date.Month >= Month_Type(10) and Date.Day < Day_Type(10) then
--      declare
--        Dir_On_Disk : String := Path & Trim(Date.Year'Img) & "-" & Trim(Date.Month'Img) & "-0" & Trim(Date.Day'Img);
--      begin
--        Log("Delete_Shared_Mem","will delete " & Dir_On_Disk );
--        if AD.Exists(Dir_On_Disk) then
--          AD.Delete_Tree(Dir_On_Disk);
--          Log("Delete_Shared_Mem","Deleted " & Dir_On_Disk );
--        end if;
--      end;
--    elsif Date.Month < Month_Type(10) and Date.Day >= Day_Type(10) then
--      declare
--        Dir_On_Disk : String := Path & Trim(Date.Year'Img) & "-0" & Trim(Date.Month'Img) & "-" & Trim(Date.Day'Img);
--      begin
--        Log("Delete_Shared_Mem","will delete " & Dir_On_Disk );
--        if AD.Exists(Dir_On_Disk) then
--          AD.Delete_Tree(Dir_On_Disk);
--          Log("Delete_Shared_Mem","Deleted " & Dir_On_Disk );
--        end if;
--      end;
--    elsif Date.Month >= Month_Type(10) and Date.Day >= Day_Type(10) then
--      declare
--        Dir_On_Disk : String := Path & Trim(Date.Year'Img) & "-" & Trim(Date.Month'Img) & "-" & Trim(Date.Day'Img);
--      begin
--        Log("Delete_Shared_Mem","will delete " & Dir_On_Disk );
--        if AD.Exists(Dir_On_Disk) then
--          AD.Delete_Tree(Dir_On_Disk);
--          Log("Delete_Shared_Mem","Deleted " & Dir_On_Disk );
--        end if;
--      end;
--    end if;
--  end Delete_Shared_Mem;
  --------------------------------------

end Sim ;
