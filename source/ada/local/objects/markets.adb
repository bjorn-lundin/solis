with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Runners;
with Rpc;
with Bot_Config;
with Logging; use Logging;
with Utils;

package body Markets is
  Me : constant String := "Markets.";

  Find_Plc_Market,
  Find_Win_Market,
  Select_Unsettled_Markets,
  Select_Ongoing_Markets : Sql.Statement_Type;

  function Empty_Data return Market_Type is
    Ed : Market_Type;
  begin
    return Ed;
  end Empty_Data;

  ----------------------------------------

  procedure Corresponding_Place_Market(Self         : in out Market_Type;
                                       Place_Market :    out Market_Type;
                                       Found        :    out Boolean) is
    T   : Sql.Transaction_Type with Warnings => Off;
    Eos : Boolean := False;
  begin
    T.Start;
    Find_Plc_Market.Prepare(
                            "select MP.* from AMARKETS MW, AMARKETS MP " &
                              "where MW.EVENTID = MP.EVENTID " &
                              "and MW.STARTTS = MP.STARTTS " &
                              "and MW.MARKETID = :WINMARKETID " &
                              "and MP.MARKETTYPE = 'PLACE' " &
                              "and MP.NUMWINNERS = :NUM " &
                              "and MW.MARKETTYPE = 'WIN'");

    Find_Plc_Market.Set("NUM", Integer_4(3));
    Find_Plc_Market.Set("WINMARKETID", Self.Marketid);
    Find_Plc_Market.Open_Cursor;
    Find_Plc_Market.Fetch (Eos);
    if not Eos then
      Place_Market := Markets.Get(Find_Plc_Market);
      Found := True;
    else
      --  Log (Me & "Corresponding_Place_Market", "no PLACE market found");
      Found := False;
    end if;
    Find_Plc_Market.Close_Cursor;
    T.Commit;
  end Corresponding_Place_Market;


  ----------------------------------------

  procedure Corresponding_Win_Market(Self       : in out Market_Type;
                                     Win_Market :    out Market_Type;
                                     Found      :    out Boolean) is
    T   : Sql.Transaction_Type with Warnings => Off;
    Eos : Boolean := False;
  begin
    T.Start;
    Find_Win_Market.Prepare(
                            "select MW.* from AMARKETS MW, AMARKETS MP " &
                              "where MW.EVENTID = MP.EVENTID " &
                              "and MW.STARTTS = MP.STARTTS " &
                              "and MP.MARKETID = :PLACEMARKETID " &
                              "and MP.MARKETTYPE = 'PLACE' " &
                            -- "and MP.NUMWINNERS = :NUM " &
                              "and MW.MARKETTYPE = 'WIN'");

    -- Find_Win_Market.Set("NUM", Integer_4(1));
    Find_Win_Market.Set("PLACEMARKETID", Self.Marketid);
    Find_Win_Market.Open_Cursor;
    Find_Win_Market.Fetch (Eos);
    if not Eos then
      Win_Market := Markets.Get(Find_Win_Market);
      Found := True;
    else
      --  Log (Me & "Corresponding_Win_Market", "no WIN market found");
      Found := False;
    end if;
    Find_Win_Market.Close_Cursor;
    T.Commit;
  end Corresponding_Win_Market;


  ----------------------------------------

  procedure Read_List(Stm  : in     Sql.Statement_Type;
                      List : in out Lists.List;
                      Max  : in     Integer_4 := Integer_4'Last) is
    Old_List :Table_Amarkets.Amarkets_List_Pack2.List;
    New_Data : Market_Type;
  begin
    Table_Amarkets.Read_List(Stm,Old_List,Max);
    for I of Old_List loop
      New_Data := (
                   Marketid         => I.Marketid,
                   Marketname       => I.Marketname,
                   Startts          => I.Startts,
                   Eventid          => I.Eventid,
                   Markettype       => I.Markettype,
                   Status           => I.Status,
                   Betdelay         => I.Betdelay,
                   Numwinners       => I.Numwinners,
                   Numrunners       => I.Numrunners,
                   Numactiverunners => I.Numactiverunners,
                   Totalmatched     => I.Totalmatched,
                   Totalavailable   => I.Totalavailable,
                   Ixxlupd          => I.Ixxlupd,
                   Ixxluts          => I.Ixxluts
                  );
      List.Append(New_Data);
    end loop;
  end Read_List;
  ----------------------------------------


  procedure Read_Eventid(  Data  : in out Market_Type'Class;
                           List  : in out Lists.List;
                           Order : in     Boolean := False;
                           Max   : in     Integer_4 := Integer_4'Last) is

    Old_List :Table_Amarkets.Amarkets_List_Pack2.List;
    New_Data : Market_Type;
  begin
    New_Data.Eventid := Data.Eventid;
    Table_Amarkets.Read_Eventid(Data, Old_List, Order, Max);
    for I of Old_List loop
      New_Data := (
                   Marketid         => I.Marketid,
                   Marketname       => I.Marketname,
                   Startts          => I.Startts,
                   Eventid          => I.Eventid,
                   Markettype       => I.Markettype,
                   Status           => I.Status,
                   Betdelay         => I.Betdelay,
                   Numwinners       => I.Numwinners,
                   Numrunners       => I.Numrunners,
                   Numactiverunners => I.Numactiverunners,
                   Totalmatched     => I.Totalmatched,
                   Totalavailable   => I.Totalavailable,
                   Ixxlupd          => I.Ixxlupd,
                   Ixxluts          => I.Ixxluts
                  );
      List.Append(New_Data);
    end loop;
  end Read_Eventid;
  ----------------------------------------

  procedure Check_Unsettled_Markets(Inserted_Winner : in out Boolean) is
    T           : Sql.Transaction_Type;
    Db_Runner   : Runners.Runner_Type;
    Runner_List : Runners.Lists.List;
    Market_List : Markets.Lists.List;
    type Eos_Type is ( Arunners);
    Eos         : array (Eos_Type'Range) of Boolean := (others => False);
  begin
    Log (Me & "Check_Unsettled_Markets", "Check_Unsettled_Markets start");
    Inserted_Winner := False;
    T.Start;
    Select_Unsettled_Markets.Prepare(
                                     "select * from AMARKETS where MARKETID in ( " &
                                       "select distinct(M.MARKETID) " &
                                       "from AMARKETS M, ARUNNERS R " &
                                       "where M.MARKETID = R.MARKETID " &
                                       "and M.STATUS in ('SETTLED','CLOSED') " &
                                       "and R.STATUS in ('', 'NOT_SET_YET') ) " &
                                       "order by STARTTS" );

    Markets.Read_List(Select_Unsettled_Markets, Market_List);

    Market_Loop : for Market of Market_List loop
      Rpc.Check_Market_Result(Market_Id   => Market.Marketid,
                              Runner_List => Runner_List);

      Runner_Loop      : for List_Runner of Runner_List loop
        Db_Runner := List_Runner;
        Db_Runner.Read( Eos(Arunners));
        if Eos(Arunners) then
          Log (Me & "Check_Unsettled_Markets", "missing runner in db !! " & Db_Runner.To_String);
        else
          Db_Runner.Status := List_Runner.Status;
          if Db_Runner.Status(1..2) = "WI" then
            Log (Me & "Check_Unsettled_Markets", "Got winner : " & Db_Runner.To_String);
            Inserted_Winner := True;
          end if;
          Db_Runner.Update_Withcheck;
        end if;

      end loop Runner_Loop;
    end loop Market_Loop;
    T.Commit;
    Log (Me & "Check_Unsettled_Markets", "Check_Unsettled_Markets stop");
  exception
    when Sql.Duplicate_Index =>
      Sql.Rollback(T);
      Log (Me & "Check_Unsettled_Markets", "Check_Unsettled_Markets Duplicate index");
      Inserted_Winner := False;
  end Check_Unsettled_Markets;
  ---------------------------------------------------------------------------------

  procedure Check_Market_Status is
    T           : Sql.Transaction_Type;
    Market_List : Markets.Lists.List;
    Market      : Markets.Market_Type;
    Is_Changed  : Boolean        := False;

  begin
    Log(Me & "Check_Market_Status", "start");

    case Bot_Config.Config.System_Section.Bot_Mode is
      when Real =>
        loop
          begin
            T.Start;
            Select_Ongoing_Markets.Prepare(
                                           "select M.* from AMARKETS M " &
                                             "where M.STATUS <> 'CLOSED' order by M.STARTTS");
            Markets.Read_List(Select_Ongoing_Markets, Market_List);

            for M of Market_List loop
              Log(Me & "Check_Market_Status", Market_List.Length'Img & " market left to check");
              Market := M;
              Log(Me & "Check_Market_Status", "checking " & Market.Marketid); --Table_Amarkets.To_String(Market));
              Rpc.Market_Status_Is_Changed(Market, Is_Changed);

              if Is_Changed then
                Log(Me & "Check_Market_Status", "update market " & Market.To_String);
                Market.Update_Withcheck;
              end if;
            end loop;
            T.Commit;
            exit;
          exception
            when Sql.No_Such_Row =>
              Log(Me & "Check_Market_Status", "trf conflict update market " & Market.To_String);
              T.Rollback;
              Market_List.Clear;
          end;
        end loop;

      when Simulation => null;
    end case;
    Log(Me & "Check_Market_Status", "stop");
  end Check_Market_Status;
  ---------------------------------------------------------------------------------

  function Marketname_Ok(Self : Market_Type) return Boolean is
  begin
    return
      Self.Marketname(1.. 9) = "5f Hcap  "   or else
      Self.Marketname(1.. 9) = "6f Hcap  "   or else
      Self.Marketname(1.. 9) = "7f Hcap  "   or else
      Self.Marketname(1.. 9) = "1m Hcap  "   or else
      Self.Marketname(1..11) = "1m1f Hcap  " or else
      Self.Marketname(1..11) = "1m2f Hcap  " or else
      Self.Marketname(1..11) = "1m3f Hcap  " or else
      Self.Marketname(1..11) = "1m4f Hcap  " or else
      Self.Marketname(1..11) = "1m5f Hcap  " or else
      Self.Marketname(1..11) = "1m6f Hcap  " or else
      Self.Marketname(1..11) = "1m7f Hcap  " or else
      Self.Marketname(1.. 9) = "2m Hcap  "   or else
      Self.Marketname(1..11) = "2m1f Hcap  " or else
      Self.Marketname(1..11) = "2m2f Hcap  " or else
      Self.Marketname(1..11) = "2m3f Hcap  " or else
      Self.Marketname(1..11) = "2m4f Hcap  " or else
      Self.Marketname(1..11) = "2m5f Hcap  " or else
      Self.Marketname(1..11) = "2m6f Hcap  " or else
      Self.Marketname(1..11) = "2m7f Hcap  " or else
      Self.Marketname(1.. 9) = "3m Hcap  "   or else
      Self.Marketname(1..11) = "3m1f Hcap  " or else
      Self.Marketname(1..11) = "3m2f Hcap  " or else
      Self.Marketname(1..11) = "3m3f Hcap  " or else
      Self.Marketname(1..11) = "3m4f Hcap  ";
  end Marketname_Ok;
  ------------------------------------------------------------
  function Marketname_Ok2(Self : Market_Type; Allow_Chase : Boolean := True; Allow_Hurdle : Boolean := True) return Boolean is
  begin
    return
      Self.Marketname(1..9)  = "5f Hcap  "   or else
      Self.Marketname(1..9)  = "6f Hcap  "   or else
      Self.Marketname(1..9)  = "7f Hcap  "   or else
      Self.Marketname(1..9)  = "1m Hcap  "   or else
      Self.Marketname(1..11) = "1m1f Hcap  " or else
      Self.Marketname(1..11) = "1m2f Hcap  " or else
      Self.Marketname(1..11) = "1m3f Hcap  " or else
      Self.Marketname(1..11) = "1m4f Hcap  " or else
      Self.Marketname(1..11) = "1m5f Hcap  " or else
      Self.Marketname(1..11) = "1m6f Hcap  " or else
      Self.Marketname(1..11) = "1m7f Hcap  " or else
      Self.Marketname(1..9)  = "2m Hcap  "   or else
      Self.Marketname(1..11) = "2m1f Hcap  " or else
      Self.Marketname(1..11) = "2m2f Hcap  " or else
      Self.Marketname(1..11) = "2m3f Hcap  " or else
      Self.Marketname(1..11) = "2m4f Hcap  " or else
      Self.Marketname(1..11) = "2m5f Hcap  " or else
      Self.Marketname(1..11) = "2m6f Hcap  " or else
      Self.Marketname(1..11) = "2m7f Hcap  " or else
      Self.Marketname(1..9) = "3m Hcap  "    or else
      Self.Marketname(1..11) = "3m1f Hcap  " or else
      Self.Marketname(1..11) = "3m2f Hcap  " or else
      Self.Marketname(1..11) = "3m3f Hcap  " or else
      Self.Marketname(1..11) = "3m4f Hcap  " or else

      (Self.Marketname(1..13)  = "5f Hcap Chs  "   and Allow_Chase) or else
      (Self.Marketname(1..13)  = "6f Hcap Chs  "   and Allow_Chase) or else
      (Self.Marketname(1..13)  = "7f Hcap Chs  "   and Allow_Chase) or else
      (Self.Marketname(1..13)  = "1m Hcap Chs  "   and Allow_Chase) or else
      (Self.Marketname(1..15)  = "1m1f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "1m2f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "1m3f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "1m4f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "1m5f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "1m6f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "1m7f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..13)  = "2m Hcap Chs  "   and Allow_Chase) or else
      (Self.Marketname(1..15)  = "2m1f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "2m2f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "2m3f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "2m4f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "2m5f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "2m6f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "2m7f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..13)  = "3m Hcap Chs  "   and Allow_Chase) or else
      (Self.Marketname(1..15)  = "3m1f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "3m2f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "3m3f Hcap Chs  " and Allow_Chase) or else
      (Self.Marketname(1..15)  = "3m4f Hcap Chs  " and Allow_Chase) or else

      (Self.Marketname(1..13)  = "5f Hcap Hrd  "   and Allow_Hurdle) or else
      (Self.Marketname(1..13)  = "6f Hcap Hrd  "   and Allow_Hurdle) or else
      (Self.Marketname(1..13)  = "7f Hcap Hrd  "   and Allow_Hurdle) or else
      (Self.Marketname(1..13)  = "1m Hcap Hrd  "   and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "1m1f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "1m2f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "1m3f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "1m4f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "1m5f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "1m6f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "1m7f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..13)  = "2m Hcap Hrd  "   and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "2m1f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "2m2f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "2m3f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "2m4f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "2m5f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "2m6f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "2m7f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..13)  = "3m Hcap Hrd  "   and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "3m1f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "3m2f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "3m3f Hcap Hrd  " and Allow_Hurdle) or else
      (Self.Marketname(1..15)  = "3m4f Hcap Hrd  " and Allow_Hurdle);
  end Marketname_Ok2;


  function Marketname_Ok3(Self : Market_Type) return Boolean is
  begin  -- for lay strategy 19-29 at start
    return
      Self.Marketname(1..9)  = "5f Hcap  "   or else
      Self.Marketname(1..9)  = "6f Hcap  "   or else
      Self.Marketname(1..9)  = "7f Hcap  "   or else
      Self.Marketname(1..9)  = "1m Hcap  "   or else
      Self.Marketname(1..11) = "1m1f Hcap  ";
  end Marketname_Ok3;




  function Distance(Self : in out Market_Type) return Integer_4 is
    Name      : Distancename_Type := (others => ' ');
    Mile      : constant Fixed_Type := 1609.344; --m
    Furlong   : constant Fixed_Type := 201.168; --m
    Furlongs,
    Miles     : Fixed_Type := 0.0;
  begin

    Name := Self.Distance_Name;
    if Name(3..3) /= " " then
      Furlongs := Fixed_Type'Value(Name(3..3));
    end if;
    Miles  := Fixed_Type'Value(Name(1..1));

    return Integer_4(Mile*Miles) + Integer_4(Furlong*Furlongs);
  end Distance;



  function Distance_Name(Self : in out Market_Type) return Distancename_Type is
    Idx  : Integer := 0;
    Name : Distancename_Type := (others => ' ');
  begin
    for I in Self.Marketname'Range loop
      case Self.Marketname(I) is
        when ' ' => Idx := I; exit;
          when others => null;
      end case;
    end loop;
    Move(Self.Marketname(1 .. Idx-1), Name);
    return Name;
  end Distance_Name;


  function Market_Subtype(Self : in out Market_Type) return Market_Subtype_Type is
  begin
    -- expects markets that are ok eg '2m5f Hcap Hrd'
    if Utils.Position(Self.Marketname, "Hrd") > Self.Marketname'First then
      return Hurdle;
    elsif Utils.Position(Self.Marketname, "Chs") > Self.Marketname'First then
      return Chase;
    else
      return Plain;
    end if;
  end Market_Subtype;




end Markets;
