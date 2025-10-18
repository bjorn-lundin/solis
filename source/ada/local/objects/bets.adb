with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Process_Io;
with Calendar2; use Calendar2;

with Logging; use Logging;
with Utils;
with Bot_System_Number;
with Bot_Svn_Info;
with Rpc;
with Price_Histories;

package body Bets is
  Me                  : constant String := "Bet.";
  Select_Is_Existing_Marketid_Selectionid,
  Select_Executable_Bets,
  Select_Dry_Run_Bets,
  Select_Real_Bets,
  Update_Betwon_To_Null,
  Select_Exists,
  Select_Profit_Today,
  Select_Ph           : Sql.Statement_Type;


  Global_Sum_Lay : Fixed_Type := 0.0;


  --
  procedure Update_And_Nullify_Betwon(Self : in out Bet_Type; Keep_Timestamp : Boolean := False) is
  begin
    Table_Abets.Data_Type(Self).Update_Withcheck(Keep_Timestamp);
    Self.Nullify_Betwon;
  end Update_And_Nullify_Betwon;
  --------------------------------------------------------
  procedure Insert_And_Nullify_Betwon(Self : in out Bet_Type; Keep_Timestamp : in Boolean := False) is
  begin
    Table_Abets.Data_Type(Self).Insert(Keep_Timestamp);
    Self.Nullify_Betwon;
  end Insert_And_Nullify_Betwon;

  ------------------------------------------------------------
  function Profit_Today(Bet_Name : Betname_Type) return Fixed_Type is
    T                    : Sql.Transaction_Type;
    Eos                  : Boolean := False;
    Start_Date, End_Date : Time_Type := Clock;
    Profit               : Fixed_Type := 0.0;
  begin
    T.Start;
    Start_Date := Calendar2.Clock;
    End_Date := Calendar2.Clock;

    Start_Date.Hour        := 0;
    Start_Date.Minute      := 0;
    Start_Date.Second      := 0;
    Start_Date.Millisecond := 0;

    End_Date.Hour        := 23;
    End_Date.Minute      := 59;
    End_Date.Second      := 59;
    End_Date.Millisecond := 999;

    Select_Profit_Today.Prepare("select sum(PROFIT) " &
                                  "from ABETS " &
                                  "where STARTTS >= :STARTOFDAY " &
                                  "and STARTTS <= :ENDOFDAY " &
                                  "and BETWON is not null " &
                                  "and BETNAME = :BETNAME " );

    Select_Profit_Today.Set("BETNAME", Bet_Name);
    Select_Profit_Today.Set_Timestamp( "STARTOFDAY",Start_Date);
    Select_Profit_Today.Set_Timestamp( "ENDOFDAY",End_Date);
    Select_Profit_Today.Open_Cursor;
    Select_Profit_Today.Fetch(Eos);
    if not Eos then
      Select_Profit_Today.Get(1, Profit);
    else
      Profit := 0.0;
    end if;
    Select_Profit_Today.Close_Cursor;
    T.Commit;
    Log(Me & "Profit_Today", Utils.Trim(Bet_Name) & " :" & " HAS earned " & Utils.F8_Image(Profit) & " today: " & Calendar2.String_Date(Start_Date));
    return Profit;
  end Profit_Today;
  ------------------------------------------------------------
  function Exists(Bet_Name : Betname_Type; Market_Id : Marketid_Type) return Boolean is
    T    : Sql.Transaction_Type;
    Eos  : Boolean := False;
    Abet : Table_Abets.Data_Type;
  begin
    T.Start;
    Select_Exists.Prepare("select * " &
                            "from ABETS " &
                            "where MARKETID = :MARKETID " &
                            "and BETNAME = :BETNAME ");

    Select_Exists.Set("BETNAME",  Bet_Name);
    Select_Exists.Set("MARKETID", Market_Id);

    Select_Exists.Open_Cursor;
    Select_Exists.Fetch( Eos);
    if not Eos then
      Abet := Table_Abets.Get(Select_Exists);
      Log(Me & "Exists", "Bet does already exist " & Table_Abets.To_String(Abet));
    else
      null;
      --        Log(Me & "Exists", "Bet does not exist");
    end if;
    Select_Exists.Close_Cursor;
    T.Commit;
    return not Eos;
  end Exists;
  ------------------------
  function Empty_Data return Bet_Type is
    Ed : Bet_Type;
  begin
    return Ed;
  end Empty_Data;

  ----------------------------------------
  procedure Clear(Self : in out Bet_Type) is
  begin
    Self := Empty_Data;
  end Clear;
  ----------------------------------------
  procedure Check_Outcome(Self   : in out Bet_Type ;
                          Runner : in     Runners.Runner_Type := Runners.Empty_Data) is
    The_Runner : Runners.Runner_Type;
    Eos        : Boolean := False;
    use type Runners.Runner_Type;
  begin
    if Self.Pricematched < 0.5 then
      Self.Profit := 0.0;
      return;
    end if;

    if Runner = Runners.Empty_Data then
      The_Runner.Marketid := Self.Marketid;
      The_Runner.Selectionid := Self.Selectionid;
      The_Runner.Read(Eos);
      if Eos then
        Log(Me & "Check_Outcome", "Runner does not exist: " & The_Runner.To_String);
        return;
      end if;
    else
      The_Runner := Runner;
    end if;

    if The_Runner.Status(1..7) = "REMOVED" then
      Self.Status(1..7) := "REMOVED";
      Self.Betwon := True;
      Self.Profit := 0.0;
      return;
    end if;

    --Self.Runnername := The_Runner.Runnernamestripped;
    if Self.Side(1..4) = "BACK" then
      if The_Runner.Status(1..6) = "WINNER" then
        Self.Betwon := True;
      elsif The_Runner.Status(1..5) = "LOSER" then
        Self.Betwon := False;
      end if;

      if Self.Betwon then
        Self.Profit := (1.0 - Commission) * Self.Sizematched * (Self.Pricematched - 1.0);
      else
        Self.Profit := -Self.Sizematched;
      end if;

    elsif Self.Side(1..3) = "LAY" then
      if The_Runner.Status(1..6) = "WINNER" then
        Self.Betwon := False;
      elsif The_Runner.Status(1..5) = "LOSER" then
        Self.Betwon := True;
      end if;

      if Self.Betwon then
        Self.Profit := (1.0 - Commission) * Self.Sizematched;
      else
        Self.Profit := -Self.Sizematched * (Self.Pricematched - 1.0);
      end if;
    end if;
  end Check_Outcome;




  ------------------------

  procedure Match_Directly(Self : in out Bet_Type; Value : Boolean ) is
  begin
    if Value then
      Self.Powerdays := Integer_4(1);
    else
      Self.Powerdays := Integer_4(0);
    end if;
  end Match_Directly;
  ------------------------
  function Match_Directly(Self : in out Bet_Type) return Boolean is
  begin
    return Self.Powerdays /= Integer_4(0);
  end Match_Directly;

  ------------------------

  function Create(Name   : Betname_Type;
                  Side   : Bet_Side_Type;
                  Size   : Bet_Size_Type;
                  Price  : Price_Type;
                  Placed : Calendar2.Time_Type;
                  Runner : Runners.Runner_Type;
                  Market : Markets.Market_Type) return Bet_Type is

    Now        : Calendar2.Time_Type      := Calendar2.Clock;
    Self       : Bet_Type;
    Local_Side : String (Self.Side'Range) := (others => ' ');

  begin
    Move (Side'Img,Local_Side);
    Self := (
             Betid          => Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid)),
             Marketid       => Market.Marketid,
             Betmode        => Bot_Mode(Simulation),
             Powerdays      => 0,
             Selectionid    => Runner.Selectionid,
             Reference      => (others         => '-'),
             Size           => Fixed_Type(Size),
             Price          => Fixed_Type(Price),
             Side           => Local_Side,
             Betname        => Name,
             Betwon         => False,
             Profit         => 0.0,
             Status         => "UNMATCHED                                         ",
             Exestatus      => (others         => ' '),
             Exeerrcode     => (others         => ' '),
             Inststatus     => (others         => ' '),
             Insterrcode    => (others         => ' '),
             Startts        => Market.Startts,
             Betplaced      => Placed,
             Pricematched   => Fixed_Type(0.0),
             Sizematched    => Fixed_Type(Size),
             Runnername     => Runner.Runnernamestripped,
             Fullmarketname => Market.Marketname,
             Svnrevision    => Bot_Svn_Info.Revision,
             Ixxlupd        => (others         => ' '), --set by insert
             Ixxluts        => Now              --set by insert
            );
    return Self;
  end Create;
  -------------------------
  procedure Check_Matched(Self : in out Bet_Type) is
    List : Price_Histories.Lists.List;
  begin
    Select_Ph.Prepare(
                      "select * " &
                        "from APRICESHISTORY " &
                        "where MARKETID = :MARKETID " &
                        "and SELECTIONID = :SELECTIONID " &
                        "and PRICETS >= :PRICETS1 " &
                        "and PRICETS <= :PRICETS2 " &
                        "order by PRICETS"
                     );

    Select_Ph.Set("MARKETID", Self.Marketid);
    Select_Ph.Set("SELECTIONID", Self.Selectionid);
    Select_Ph.Set("PRICETS1", Self.Betplaced + (0,0,0,1,0)); -- 1 s
    if Self.Match_Directly then
      Select_Ph.Set("PRICETS2", Self.Betplaced + (0,0,0,2,0)); -- data 1s..2s from betplaced
    else -- get the whole race, assume shorter than 9 days
      Select_Ph.Set("PRICETS2", Self.Betplaced + (9,0,0,0,0)); -- data 1s .. 9 days from betplaced
    end if;
    Price_Histories.Read_List(Select_Ph,List);

    for Ph of List loop
      if Self.Side(1..4) = "BACK" then
        if Ph.Backprice >= Self.Price and then -- Match ok
          Ph.Backprice <= Fixed_Type(1000.0) then -- Match ok
          Self.Pricematched := Ph.Backprice;
          Move("MATCHED",Self.Status);
        end if;
      elsif Self.Side(1..3) = "LAY" then
        if Ph.Layprice <= Self.Price and then -- Match ok
          Ph.Layprice >= Fixed_Type(1.01) then
          Self.Pricematched := Ph.Layprice;
          Move("MATCHED",Self.Status);
        end if;
      end if;
      exit when Self.Match_Directly or else -- match directly
        Self.Status(1..7) = "MATCHED";     -- matched
    end loop;
    if Self.Status(1) /= 'M' then
      Move("LAPSED",Self.Status);
      Self.Pricematched := Fixed_Type(0.0);
      Self.Profit := 0.0;
    end if;
  end Check_Matched;
  ----------------------------------

  procedure Read_List(Stm  : in     Sql.Statement_Type;
                      List : in out Lists.List;
                      Max  : in     Integer_4 := Integer_4'Last) is
    Ab_List :Table_Abets.Abets_List_Pack2.List;
    B       : Bet_Type;
  begin
    Table_Abets.Read_List(Stm,Ab_List,Max);
    for I of Ab_List loop
      B := (
            Betid                   => I.Betid,
            Marketid                => I.Marketid,
            Betmode                 => I.Betmode,
            Powerdays               => I.Powerdays,
            Selectionid             => I.Selectionid,
            Reference               => I.Reference,
            Size                    => I.Size,
            Price                   => I.Price,
            Side                    => I.Side,
            Betname                 => I.Betname,
            Betwon                  => I.Betwon,
            Profit                  => I.Profit,
            Status                  => I.Status,
            Exestatus               => I.Exestatus,
            Exeerrcode              => I.Exeerrcode,
            Inststatus              => I.Inststatus,
            Insterrcode             => I.Insterrcode,
            Startts                 => I.Startts,
            Betplaced               => I.Betplaced,
            Pricematched            => I.Pricematched,
            Sizematched             => I.Sizematched,
            Runnername              => I.Runnername,
            Fullmarketname          => I.Fullmarketname,
            Svnrevision             => I.Svnrevision,
            Ixxlupd                 => I.Ixxlupd,
            Ixxluts                 => I.Ixxluts
           );
      List.Append(B);
    end loop;
  end Read_List;
  ----------------------------------------
  procedure Nullify_Betwon(Self : in out Bet_Type) is
  begin
    Log(Me & "Nullify_Betwon", Self.To_String);
    Update_Betwon_To_Null.Prepare(
                                  "update ABETS set BETWON = null, IXXLUPD=:UPDATER, IXXLUTS= :IXXLUTS " &
                                    "where BETID = :BETID " &
                                    "and IXXLUPD = :OLDIXXLUPD " &
                                    "and IXXLUTS = :OLDIXXLUTS "
                                 );
    Update_Betwon_To_Null.Set("UPDATER", Process_Io.This_Process.Name);
    Update_Betwon_To_Null.Set_Timestamp("IXXLUTS", Calendar2.Clock);
    Update_Betwon_To_Null.Set("OLDIXXLUPD", Self.Ixxlupd);
    Update_Betwon_To_Null.Set_Timestamp("OLDIXXLUTS", Self.Ixxluts);
    Update_Betwon_To_Null.Set("BETID", Self.Betid);
    Update_Betwon_To_Null.Execute;
  end Nullify_Betwon;
  ----------------------------------------
  procedure Check_Bets is
    use Utils;
    Bet_List               : Bets.Lists.List;
    Bet,Bet_From_List      : Bets.Bet_Type;
    T                      : Sql.Transaction_Type;
    Illegal_Data           : Boolean := False;
    Illegal_Data2          : Boolean := False;
    Side                   : Bet_Side_Type;
    Runner                 : Runners.Runner_Type;
    type Eos_Type is (Arunner ,
                      Abets);
    Eos                    : array (Eos_Type'Range) of Boolean := (others => False);
    Selection_In_Winners,
    Bet_Won                : Boolean := False;
    Profit                 : Fixed_Type := 0.0;
    Start_Ts               : Calendar2.Time_Type := Calendar2.Time_Type_First;
    Stop_Ts                : Calendar2.Time_Type := Calendar2.Time_Type_Last;

    Rpc_Status : Rpc.Result_Type;
    Do_Update  : Boolean := True;
  begin
    Log(Me & "Check_Bets", "start");

    -- update ARUNNERS.STATUS with real result...
    -- is done by Winners_Fetcher_Json
    --Get result, set status=PRELIMINARY for real bets

    T.Start;
    -- check the dry run bets
    Select_Dry_Run_Bets.Prepare(
                                "select B.* from ABETS B, AMARKETS M " &
                                  "where B.MARKETID = M.MARKETID " &
                                  "and B.BETWON is null " & -- will be not null if updated
                                --      "and B.BETID < 1000000000 " & -- ALL BETS
                                  "and M.STATUS in ('SUSPENDED','SETTLED','CLOSED') " & -- does 'SETTLED' exist?
                                  "and exists (select 'a' from ARUNNERS where ARUNNERS.MARKETID = B.MARKETID and ARUNNERS.STATUS = 'WINNER')" ); -- must have had time to check ...
    Bets.Read_List(Select_Dry_Run_Bets, Bet_List);
    T.Commit;

    for B of Bet_List loop
      Log(Me & "Check_Bets", "betlist " & B.To_String);
    end loop;

    Inner        : for B of Bet_List  loop
      Bet := B;
      Illegal_Data := False;
      Log(Me & "Check_Bets", "Check bet " & Bet.To_String);
      if Trim(Bet.Side) = "BACK" then
        Side := Back;
      elsif Trim(Bet.Side(1..3)) = "LAY" then --lay + lay1-lay6
        Side := Lay;
      else
        Illegal_Data := True;
        Log(Me & "Check_Bets", "Illegal_Data ! side -> " &  Trim(Bet.Side));
      end if;
      if not Illegal_Data then
        Runner.Marketid := Bet.Marketid;
        Runner.Selectionid := Bet.Selectionid;
        Runner.Read(Eos(Arunner));

        if not Eos(Arunner) then
          -- do we have a non-runner?
          if Runner.Status(1..7) = "REMOVED" then
            -- non -runner - void the bet
            Bet.Betwon := True;
            Bet.Profit := 0.0;
            begin
              T.Start;
              Log(Me & "Check_Bets", " 1 " & Bet.To_String);
              Bet.Update_Withcheck;
              Log(Me & "Check_Bets", " 2 " & Bet.To_String);
              T.Commit;
            exception
              when Sql.No_Such_Row =>
                T.Rollback; -- let the other one do the update
                exit Inner;
            end ;
          elsif Runner.Status(1..6) = "WINNER" then
            -- this one won
            Selection_In_Winners := True;
          elsif Runner.Status(1..5) = "LOSER" then
            -- this one won
            Selection_In_Winners := False;
          else
            Log(Me & "Check_Bets", "unknown runner status, exit '" & Runner.Status & "'");
            Illegal_Data2 := True;
          end if;
        else
          Log(Me & "Check_Bets", "runner not found ?? " & Runner.To_String);
          Illegal_Data2 := True;
        end if;

        if not Illegal_Data2 then
          case Side is
            when Back    => Bet_Won := Selection_In_Winners;
            when Lay     => Bet_Won := not Selection_In_Winners;
          end case;

          if Bet_Won then
            case Side is     -- Betfair takes 5% provision on winnings, but 5% per market,
                -- so it won't do to calculate per bet. leave that to the sql-script summarising
              when Back    => Profit := 1.0 * Bet.Sizematched * (Bet.Pricematched - 1.0);
              when Lay     => Profit := 1.0 * Bet.Sizematched;
            end case;
          else -- lost :-(
            case Side is
              when Back    => Profit := - Bet.Sizematched;
              when Lay     => Profit := - Bet.Sizematched * (Bet.Pricematched - 1.0);
            end case;
          end if;

          Bet.Betwon := Bet_Won;
          Bet.Profit := Profit;
          if Bet.Betid > 1_000_000_000 then -- a real bet
            Bet.Status := (others => ' ');
            Move("PRELIMINARY", Bet.Status);
          end if;

          begin
            T.Start;
            Log(Me & "Check_Bets", " 3 " & Bet.To_String);
            Bet.Update_Withcheck;
            Log(Me & "Check_Bets", " 4 " & Bet.To_String);
            T.Commit;
          exception
            when Sql.No_Such_Row =>
              T.Rollback; -- let the other one do the update
              exit Inner;
          end ;
        else
          Log(Me & "Check_Bets", "Illegal_Data2 !!");
        end if; --Illegal data2
      else
        Log(Me & "Check_Bets", "Illegal_Data !!");
      end if; -- Illegal data
    end loop Inner;

    -- check the real bets
    -- BET_STATUS='PRELIMINARY'
    T.Start;
    Select_Real_Bets.Prepare("select min(STARTTS) from ABETS where STATUS = 'PRELIMINARY' ");

    Select_Real_Bets.Open_Cursor;
    Select_Real_Bets.Fetch(Eos(Abets));
    if not Eos(Abets) then
      Select_Real_Bets.Get_Timestamp(1, Start_Ts);
      Stop_Ts := Start_Ts + (1,0,0,0,0); -- 1 day
    end if;
    Select_Real_Bets.Close_Cursor;
    T.Commit;

    if Start_Ts = Calendar2.Time_Type_First then
      Eos(Abets) := True;
    end if;

    declare --remove bets with betid < 100_000_000
      Bet_List_2 : Bets.Lists.List := Bet_List.Copy;
    begin
      Bet_List.Clear;
      for B of Bet_List_2 loop
        if B.Betid > 100_000_000 then
          Bet_List.Append(B);
        end if;
      end loop;
    end;

    if not Eos(Abets) then
      for I in Cleared_Bet_Status_Type'Range loop
        Rpc.Get_Cleared_Bet_Info_List(Bet_Status     => I,
                                      Settled_From   => Start_Ts,
                                      Settled_To     => Stop_Ts,
                                      Betfair_Result => Rpc_Status,
                                      Bet_List       => Bet_List) ;
      end loop;

      for B of Bet_List loop
        Bet_From_List := B;
        -- Call Betfair here ! Profit & Loss
        Bet := Bets.Empty_Data;
        Bet.Betid := Bet_From_List.Betid;
        Bet.Read(Eos(Abets));
        Log(Me & "Check_Bets", "Betid" & Bet.Betid'Img & " Eos(Abets) = " & Eos(Abets)'Img);
        if not Eos(Abets) then
          Bet.Pricematched := Bet_From_List.Pricematched;
          Bet.Sizematched  := Bet_From_List.Sizematched;
          Bet.Profit       := Bet_From_List.Profit;
          Bet.Status       := Bet_From_List.Status;
          Bet.Betwon       := Bet.Profit >= 0.0;
          Log(Me & "Check_Bets", "Betid" & Bet.Betid'Img & " status = " & Bet.Status);

          Do_Update := Bet.Status(1..6) /= "VOIDED";
          if not Do_Update then
            Do_Update := Bet.Status(1..9) /= "CANCELLED";
          end if;
          if not Do_Update then
            Do_Update := Bet.Status(1..6) /= "LAPSED";
          end if;
          if not Do_Update then
            Do_Update := Bet.Status(1..7) /= "SETTLED";
          end if;

          Log(Me & "Check_Bets", "Betid" & Bet.Betid'Img & " Do_Update = " & Do_Update'Img);

          if Do_Update then
            begin
              T.Start;
              Bet.Update_Withcheck;
              T.Commit;
              Log(Me & "Check_Bets", "Betid" & Bet.Betid'Img & " updated status to " &  Bet.Status);
            exception
              when Sql.No_Such_Row =>
                T.Rollback; -- let the other one do the update
                Log(Me & "Check_Bets", "No_Such_Row!! " & Bet.To_String);
            end ;
          end if;
        else
          Log(Me & "Check_Bets", "EOS!! " & Bet.To_String);
        end if;
      end loop;

    end if;
    Log(Me & "Check_Bets", "stop");
  end Check_Bets;
  ------------------------------------------------------------------------------
  procedure Check_If_Bet_Accepted is
    T                 : Sql.Transaction_Type with Warnings => Off;
    Bet_List          : Bets.Lists.List;
    Bet               : Bets.Bet_Type;
    Avg_Price_Matched : Bet_Price_Type := 0.0;
    Is_Matched,
    Is_Removed        : Boolean        := False;
    Size_Matched      : Bet_Size_Type  := 0.0;
  begin
    Log(Me & "Check_If_Bet_Accepted", "start");
    T.Start;
    -- check the real bets
    Select_Executable_Bets.Prepare(
                                   "select B.* from ABETS B, AMARKETS M " &
                                     "where B.MARKETID = M.MARKETID " & -- all bets, until profit and loss are fixed in API-NG
                                     "and M.STATUS in ('CLOSED','SETTLED','SUSPENDED') " & -- This is not updated!!!
                                     "and B.BETWON is null " & -- all bets, until profit and loss are fixed in API-NG
                                     "and B.BETID > 1000000000 " & -- no dry_run bets
                                   --      "and B.IXXLUPD = :BOTNAME " & --only fix my bets, so no rollbacks ...
                                     "and B.STATUS = 'EXECUTABLE' "); --only not acctepted bets ...

    Bets.Read_List(Select_Executable_Bets, Bet_List);

    for B of Bet_List loop
      Bet := B;
      Log(Me & "Check_If_Bet_Accepted", "Check bet " & Bet.To_String);

      Rpc.Bet_Is_Matched(Bet.Betid, Is_Removed, Is_Matched, Avg_Price_Matched, Size_Matched);

      if Is_Matched then
        Log(Me & "Check_If_Bet_Accepted", "update bet " & Bet.To_String);
        Bet.Status := (others => ' ');
        if Is_Removed then
          Move("EXECUTABLE_NO_MATCH", Bet.Status); --?
        else
          Move("EXECUTION_COMPLETE", Bet.Status);
          Bet.Pricematched := Fixed_Type(Avg_Price_Matched);
          Bet.Sizematched := Fixed_Type(Size_Matched);
        end if;
        Log(Me & "Check_If_Bet_Accepted", "update bet " & Bet.To_String);
        Bet.Update_Withcheck;
        Bet.Nullify_Betwon;
      end if;
    end loop;
    T.Commit;
    Log(Me & "Check_If_Bet_Accepted", "stop");
  end Check_If_Bet_Accepted;
  ---------------------------------------------------------------------------------
  function Is_Matched(Self : in out Bet_Type) return Boolean is
    Is_Removed        : Boolean := False;
    Is_Matched        : Boolean := False;
    Avg_Price_Matched : Bet_Price_Type := 0.0;
    Size_Matched      : Bet_Size_Type := 0.0;
    Is_Updated        : Boolean := False;
    use Utils;

  begin
    Rpc.Bet_Is_Matched(Betid             => Self.Betid,
                       Is_Removed        => Is_Removed,
                       Is_Matched        => Is_Matched,
                       Avg_Price_Matched => Avg_Price_Matched,
                       Size_Matched      => Size_Matched) ;

    Log(Me & "Is_Matched", "Betid=" & Self.Betid'Img &
          " Is_Matched=" & Is_Matched'Img &
          " Is_Removed=" & Is_Removed'Img &
          " Avg_Price_Matched=" & F8_Image(Fixed_Type(Avg_Price_Matched)) &
          " Size_Matched=" & F8_Image(Fixed_Type(Size_Matched)));

    if Is_Matched and then
      Self.Status(1..18) /= "EXECUTION_COMPLETE" then -- dont update if already matched
      if Is_Removed then
        -- Move("EXECUTABLE_NO_MATCH", Self.Status);
        Move("EXECUTION_COMPLETE", Self.Status);
        Is_Updated := True;
      else
        --        if abs(Self.Size - Fixed_Type(Size_Matched)) < 0.0001 then
        if Self.Size = Fixed_Type(Size_Matched) then
          Move("EXECUTION_COMPLETE", Self.Status);
          Self.Pricematched := Fixed_Type(Avg_Price_Matched);
          Self.Sizematched := Fixed_Type(Size_Matched);
          Is_Updated := True;
        end if;
      end if;
      if Is_Updated then
        Log(Me & "Is_Matched", "update bet " & Self.To_String);
        Self.Update_And_Nullify_Betwon;
      end if;
    end if;

    return Is_Matched;
  end Is_Matched;
  -------------------------------------------------------------

  procedure Read_Marketid( Data  : in out Bet_Type'Class;
                           List  : in out Lists.List;
                           Order : in     Boolean := False;
                           Max   : in     Integer_4 := Integer_4'Last) is

    Old_List : Table_Abets.Abets_List_Pack2.List;
    New_Data : Bet_Type;
  begin
    Table_Abets.Read_Marketid(Data, Old_List, Order, Max);
    for I of Old_List loop
      New_Data := (
                   Betid          => I.Betid,
                   Marketid       => I.Marketid,
                   Betmode        => I.Betmode,
                   Powerdays      => I.Powerdays,
                   Selectionid    => I.Selectionid,
                   Reference      => I.Reference,
                   Size           => I.Size,
                   Price          => I.Price,
                   Side           => I.Side,
                   Betname        => I.Betname,
                   Betwon         => I.Betwon,
                   Profit         => I.Profit,
                   Status         => I.Status,
                   Exestatus      => I.Exestatus,
                   Exeerrcode     => I.Exeerrcode,
                   Inststatus     => I.Inststatus,
                   Insterrcode    => I.Insterrcode,
                   Startts        => I.Startts,
                   Betplaced      => I.Betplaced,
                   Pricematched   => I.Pricematched,
                   Sizematched    => I.Sizematched,
                   Runnername     => I.Runnername,
                   Fullmarketname => I.Fullmarketname,
                   Svnrevision    => I.Svnrevision,
                   Ixxlupd        => I.Ixxlupd,
                   Ixxluts        => I.Ixxluts
                  );
      List.Append(New_Data);
    end loop;
  end Read_Marketid;
  ----------------------------------------

  function Is_Existing_Marketid_Selectionid(Self : in out Bet_Type) return Boolean is
    T   : Sql.Transaction_Type;
    Eos : Boolean := False;

  begin
    T.Start;
    Select_Is_Existing_Marketid_Selectionid.Prepare("select * from ABETS where MARKETID = :MARKETID and SELECTIONID = :SELECTIONID");
    Select_Is_Existing_Marketid_Selectionid.Set("MARKETID", Self.Marketid);
    Select_Is_Existing_Marketid_Selectionid.Set("SELECTIONID", Self.Selectionid);

    Select_Is_Existing_Marketid_Selectionid.Open_Cursor;
    Select_Is_Existing_Marketid_Selectionid.Fetch(Eos);
    Select_Is_Existing_Marketid_Selectionid.Close_Cursor;
    T.Commit;
    return not Eos;
  end Is_Existing_Marketid_Selectionid;


  procedure Reset_Sum_Laybets is
  begin
    Global_Sum_Lay := 0.0;
  end Reset_Sum_Laybets;

  procedure Sum_Laybets(List : Lists.List; Threshold : Fixed_Type) is
  begin
    for B of List loop
      if B.Side(1..3)= "LAY" then
        Global_Sum_Lay := Global_Sum_Lay + B.Profit;
      end if;
    end loop;

    if Global_Sum_Lay < Threshold then
      raise Constraint_Error with "profit below threshold " & Global_Sum_Lay'Img & " vs " & Threshold'Img;
    end if;

  end Sum_Laybets;
  ------------------------------------------------------------

end Bets;
