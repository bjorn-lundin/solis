with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
with Bot_Messages;
with Rpc;
with Lock ;
with Posix;
with Ini;
with Logging; use Logging;
with Process_Io;
with Core_Messages;
with Markets;
with Events;
with Prices;
with Balances;
with Bot_Svn_Info;
with Bets;
with Config;
with Utils; use Utils;
--with Sim;
--with Tics;

with Aws;
with Aws.Headers;
--with Aws.Headers.Set;
with Aws.Response;
with Aws.Client;

with botcoll.Json; use botcoll.Json;



procedure Poll is
  package Ev renames Ada.Environment_Variables;
  -- use type Rpc.Result_Type;

  Me              : constant String := "Poll.";
  Timeout         : Duration := 1200.0;
  My_Lock         : Lock.Lock_Type;
  Msg             : Process_Io.Message_Type;
  Find_Plc_Market : Sql.Statement_Type;

  Sa_Par_Bot_User : aliased Gnat.Strings.String_Access;
  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Ba_Daemon       : aliased Boolean := False;
  Cmd_Line        : Command_Line_Configuration;
  Now             : Calendar2.Time_Type;
  Ok,
  Is_Time_To_Exit : Boolean := False;
  Cfg             : Config.Config_Type;
  use Config;

  type Market_Type is (Win, Place);
  type Best_Runners_Array_Type is array (1 .. 16) of Prices.Price_Type ;

  Global_Bet_Placer    : Integer := 0;

  Data            : Bot_Messages.Poll_State_Record ;
  This_Process    : Process_Io.Process_Type := Process_Io.This_Process;
  Markets_Fetcher : Process_Io.Process_Type := (("markets_fetcher"), (others => ' '));
  Bad_Selection_Id : constant Integer_4 := 0;

  -------------------------------------------------------------
  -- type-of-bet_bet-number_placement-in-race-at-time-of-bet
  --Back_1_40_30_1_4_PLC_1_01 : back leader when leader <=1.4 and 4th >=30 min price= 1.01
  --Back_1_40_30_1_4_PLC_1_02 : back leader when leader <=1.4 and 2nd >=30 min price= 1.02

  type Allowed_Type is record
    Bet_Name             : Betname_Type := (others => ' ');
    Bet_Size             : Bet_Size_Type := 0.0;
    Is_Allowed_To_Bet    : Boolean       := False;
    Has_Betted           : Boolean       := False;
    Max_Loss_Per_Day     : Bet_Size_Type := 0.0;
    Max_Earnings_Per_Day : Bet_Size_Type := 0.0;
    Bet_Size_Portion     : Bet_Size_Portion_Type := 1.0;
  end record;

  Bets_Allowed : array (Bet_Type'Range) of Allowed_Type;
  --------------------------------------------------------------
  function Get_Bet_Placer(Bettype : Config.Bet_Type) return Process_Io.Process_Type is
  begin
    Global_Bet_Placer := Global_Bet_Placer + 1;
    -- we start 10 bet_placers
    if Global_Bet_Placer > Integer(10) then
      Global_Bet_Placer := 1;
    end if;

    case Global_Bet_Placer is
      when   1 ..   9 => return Process_Io.To_Process_Type("bet_placer_00" & Trim(Global_Bet_Placer'Img, Both));
      when  10 ..  99 => return Process_Io.To_Process_Type("bet_placer_0"  & Trim(Global_Bet_Placer'Img, Both));
      when others     => raise Constraint_Error with "No bet_placer found " & Bettype'Img & " " & Global_Bet_Placer'Img;
    end case;

  end Get_Bet_Placer;

  ----------------------------------------------------------

  procedure Set_Bet_Names is
  begin
    for I in Bet_Type'Range loop
      case I is
        when others => Move(I'Img, Bets_Allowed(I).Bet_Name);
      end case;
    end loop;
  end Set_Bet_Names;

  ----------------------------------------------------------------------------

  procedure Send_Back_Bet(Selectionid    : Integer_4;
                          Main_Bet       : Bet_Type;
                          Min_Price      : Back_Price_Type;
                          Marketid       : Marketid_Type;
                          Match_Directly : Boolean := False;
                          Back_Size      : Fixed_Type := 0.0;
                          Fill_Or_Kill   : Boolean := False) is


    Pbb             : Bot_Messages.Place_Back_Bet_Record;
    Did_Bet         : array(1 .. 1) of Boolean := (others => False);
    Receiver        : Process_Io.Process_Type := Get_Bet_Placer(Main_Bet);
    Local_Back_Size : Fixed_Type := Back_Size;
  begin

    declare
      -- only bet on allowed days
      Now : Time_Type := Clock;
      Day : Week_Day_Type := Week_Day_Of(Now);
    begin
      if not Cfg.Bet(Main_Bet).Allowed_Days(Day) then
        Log("No bet layed, bad weekday for this bet" );
        return;
      end if;
    end;

    if not Cfg.Bet(Main_Bet).Enabled then
      Log("Not enbled bet in poll.ini " & Main_Bet'Img );
      return;
    end if;

    if Selectionid = Bad_Selection_Id then
      Log("Bad selectionid, = 0 ");
      return;
    end if;

    case Match_Directly is
      when False => Pbb.Match_Directly := 0;
      when True  => Pbb.Match_Directly := 1;
    end case;

    case Fill_Or_Kill is
      when False => Pbb.Fill_Or_Kill := 0;
      when True  => Pbb.Fill_Or_Kill := 1;
    end case;

    Pbb.Bet_Name := Bets_Allowed(Main_Bet).Bet_Name;
    Move(Marketid, Pbb.Market_Id);

    if Local_Back_Size = 0.0 then
      Move(F8_Image(Fixed_Type(Bets_Allowed(Main_Bet).Bet_Size)), Pbb.Size);
    else
      Move(F8_Image(Local_Back_Size), Pbb.Size);
    end if;

    Move(F8_Image(Fixed_Type(Min_Price)), Pbb.Price); --abs max
    Pbb.Selection_Id := Selectionid;

    if not Bets_Allowed(Main_Bet).Has_Betted and then
      Bets_Allowed(Main_Bet).Is_Allowed_To_Bet then
      Bot_Messages.Send(Receiver, Pbb);

      case Main_Bet is
      -- checks in calling proc - may be several bets
        when others => Bets_Allowed(Main_Bet).Has_Betted := True;
      end case;

      Did_Bet(1) := True;
    end if;

    if Did_Bet(1) then
      Log("Send_Back_Bet called with " &
            " Selectionid=" & Selectionid'Img &
            " Main_Bet=" & Main_Bet'Img &
            " Marketid= '" & Marketid & "'" &
            " Receiver= '" & Receiver.Name & "'");
      Log("pinged '" &  Trim(Receiver.Name) & "' with bet '" & Trim(Pbb.Bet_Name) & "' sel.id:" &  Pbb.Selection_Id'Img );
    end if;

  end Send_Back_Bet;
  ----------------------------------------------------------------------------

  procedure Send_Lay_Bet(Selectionid    : Integer_4;
                         Main_Bet       : Bet_Type;
                         Max_Price      : Lay_Price_Type;
                         Marketid       : Marketid_Type;
                         Match_Directly : Boolean := False;
                         Size           : Fixed_Type := 0.0;
                         Fill_Or_Kill   : Boolean := False) is


    Plb             : Bot_Messages.Place_Lay_Bet_Record;
    Did_Bet         : array(1 .. 1) of Boolean := (others => False);
    Receiver        : Process_Io.Process_Type := Get_Bet_Placer(Main_Bet);
    Local_Size      : Fixed_Type := Size;
  begin
    declare
      -- only bet on allowed days
      Now : Time_Type := Clock;
      Day : Week_Day_Type := Week_Day_Of(Now);
    begin
      if not Cfg.Bet(Main_Bet).Allowed_Days(Day) then
        Log("No bet layed, bad weekday for this bet" );
        return;
      end if;
    end;

    if not Cfg.Bet(Main_Bet).Enabled then
      Log("Not enabled bet in poll.ini " & Main_Bet'Img );
      return;
    end if;

    if Selectionid = Bad_Selection_Id then
      Log("Bad selectionid, = 0 ");
      return;
    end if;

    case Match_Directly is
      when False => Plb.Match_Directly := 0;
      when True  => Plb.Match_Directly := 1;
    end case;

    case Fill_Or_Kill is
      when False => Plb.Fill_Or_Kill := 0;
      when True  => Plb.Fill_Or_Kill := 1;
    end case;

    Plb.Bet_Name := Bets_Allowed(Main_Bet).Bet_Name;
    Move(Marketid, Plb.Market_Id);

    if Local_Size = 0.0 then
      Move(F8_Image(Fixed_Type(Bets_Allowed(Main_Bet).Bet_Size)), Plb.Size);
    else
      Move(F8_Image(Local_Size), Plb.Size);
    end if;

    Move(F8_Image(Fixed_Type(Max_Price)), Plb.Price); --abs max
    Plb.Selection_Id := Selectionid;

    if not Bets_Allowed(Main_Bet).Has_Betted and then
      Bets_Allowed(Main_Bet).Is_Allowed_To_Bet then
      Bot_Messages.Send(Receiver, Plb);

      case Main_Bet is
      -- checks in calling proc - may be several bets
      -- when Horse_Lay_19_00_29_00_At_Start_1_Win => null;
        when others => Bets_Allowed(Main_Bet).Has_Betted := True;
      end case;

      Did_Bet(1) := True;
    end if;

    if Did_Bet(1) then
      Log("Send_Lay_Bet called with " &
            " Selectionid=" & Selectionid'Img &
            " Main_Bet=" & Main_Bet'Img &
            " Marketid= '" & Marketid & "'" &
            " Receiver= '" & Receiver.Name & "'");
      Log("pinged '" &  Trim(Receiver.Name) & "' with bet '" & Trim(Plb.Bet_Name) & "' sel.id:" &  Plb.Selection_Id'Img );
    end if;

  end Send_Lay_Bet;

  --------------------------------------------------------------------

  ------------------------------------------------------

  procedure Try_To_Make_Back_Bet_Ai(Bettype         : Config.Bet_Type;
                                    Br              : Best_Runners_Array_Type;
                                    Marketid        : Marketid_Type;
                                    Win_Marketname  : Marketname_Type;
                                    Match_Directly  : Boolean := False) is

    Price           : Fixed_Type;

    Image           : String := Bettype'Img;
    --Tic             : Tics.Tics_Type;
    --use type Tics.Tics_Type;

    Req             : Json_Value := Create_Object;
    Reply           : Json_Value := Create_Object;
    Result          : Json_Value := Create_Object;
    Error           : Json_Value := Create_Object;
    Params          : Json_Value := Create_Object;
    Odds            : Json_Array := Empty_Array;
    Aws_Reply       : Aws.Response.Data;
    Http_Headers    : Aws.Headers.List := Aws.Headers.Empty_List;

    S_Lr            : String(1..4) := Image(31..34);

    Side            : Bet_Side_Type := Back;
    Idx             : Long_Long_Integer := 100;

    Market_Name     : String := Trim(Win_Marketname);
    Use_Win_Market_Name : Boolean := False;

  begin
    --1         2         3         4
    --  1234567890123456789012345678901234567890123
    --  Horse_Back_AI_Nfl_1_Hn_100_Lr_0p10_E_12_Plc


    Req.Set_Field (Field_Name => "method", Field => "AI");
    Req.Set_Field (Field_Name => "id", Field => 15);
    Req.Set_Field (Field_Name => "jsonrpc", Field => "2.0");


    if Utils.Position(Image, "PLC") > Integer(0) then
      Params.Set_Field (Field_Name => "betType", Field => "place");
    else
      Params.Set_Field (Field_Name => "betType", Field => "win");
    end if;

    if Utils.Position(Image, "BACK") > Integer(0) then
      Params.Set_Field (Field_Name => "side", Field => "back");
      Side := Back;

      for Unsorted_Runner of Br loop
        Price := Unsorted_Runner.Backprice;
--        if Unsorted_Runner.Backprice > Price then
--          Tic := Tics.Get_Tic_Index(Unsorted_Runner.Backprice);
--          Price := Tics.Get_Tic_Price(Tic -1);
--        end if;
        Append(Odds,Create(Float'Value(Utils.F8_Image(Price))));
      end loop;

    else
      Params.Set_Field (Field_Name => "side", Field => "lay");
      Side := Lay;

      for Unsorted_Runner of Br loop
        Price := Unsorted_Runner.Layprice;
--        if Unsorted_Runner.Layprice > Price then
--          Tic := Tics.Get_Tic_Index(Unsorted_Runner.Layprice);
--          Price := Tics.Get_Tic_Price(Tic +1);
--        end if;
        Append(Odds,Create(Float'Value(Utils.F8_Image(Price))));
      end loop;
    end if;

    for i in Market_Name'range loop
      case Market_Name(i) is
        when ' '    => Market_Name(i) := '_' ;
        when others => null;
      end case;
    end loop;


--    Use_Win_Market_Name := Bettype in Horse_Back_AI_nfl_0_hn_300_lr_1p00_E_12_Plc ..
--                                     Horse_Back_AI_NFL_0_HN_300_LR_1p00_E_12_Win;
    pragma compile_time_warning(true, "uncomment if to be reused");

--    Params.Set_Field (Field_Name => "winMarketName", Field => Market_Name);
--    Params.Set_Field (Field_Name => "useWinMarketName", Field => Use_Win_Market_Name);

    Params.Set_Field (Field_Name => "hiddenNodes", Field => Long_Long_Integer'Value(Image(24..26)));
    S_Lr(2) := '.'; -- Get rid of the 'p'
    Params.Set_Field (Field_Name => "learningRate", Field => Float'Value(S_Lr));
    Params.Set_Field (Field_Name => "numFromLeader", Field => Long_Long_Integer'Value(Image(19..19)));
    Params.Set_Field (Field_Name => "epochs", Field => Long_Long_Integer'Value(Image(38..39)));

    Params.Set_Field (Field_Name => "input", Field => Odds);
    Params.Set_Field (Field_Name => "marketid", Field => Marketid);
    Params.Set_Field (Field_Name => "betname", Field => Trim(Image));

    Req.Set_Field (Field_Name => "params", Field => Params);

    Log("Try_To_Make_Back_Bet_ai", Req.Write);


    Aws.Headers.Add (Http_Headers, "Accept", "application/json");

    Aws_Reply := Aws.Client.Post (Url          => "http://127.0.0.1:12345/AI",
                                  Data         => Req.Write,
                                  Content_Type => "application/json",
                                  Headers      => Http_Headers,
                                  Timeouts     => Aws.Client.Timeouts (Each => 30.0));

    Log("Try_To_Make_Back_Bet_ai", Aws.Response.Message_Body(Aws_Reply));

    Reply := Read (Strm => Aws.Response.Message_Body(Aws_Reply), Filename => "");

    if Reply.Has_Field("error") then
      Error := Reply.Get("error");
      if Error.Has_Field("message") then
        Log("Try_To_Make_Back_Bet_Ai -error.message " , Error.Get("message"));
      end if;
      Log("Try_To_Make_Back_Bet_Ai " , "no bet placed");
      return;
    end if;

    if Reply.Has_Field("result") then
      Result := Reply.Get("result");
      if Result.Has_Field("bestRunner") then
        Idx:= Result.Get("bestRunner");
        Idx := Idx +1;  --convert 0-based idx to 1-based
      end if;
    end if;

    if not Cfg.Bet(Bettype).Enabled then
      Log("Not enbled bet in poll.ini " & Bettype'Img );
      return;
    end if;

    if Br(Integer(Idx)).Selectionid = Bad_Selection_Id then
      Log("Bad selectionid, = 0 ");
      return;
    end if;

    if Idx < 100 then
      case Side is
        when Back =>

          Send_Back_Bet(Selectionid     => Br(Integer(Idx)).Selectionid,
                        Main_Bet        => Bettype,
                        Marketid        => Marketid,
                    --    Min_Price       => Back_Price_Type(Br(Integer(Idx)).Backprice),
                        Min_Price       => Back_Price_Type(1.01),
                        Match_Directly  => Match_Directly);

        when Lay =>
          Send_Lay_Bet(Selectionid     => Br(Integer(Idx)).Selectionid,
                       Main_Bet        => Bettype,
                       Marketid        => Marketid,
                       Max_Price       => Lay_Price_Type(20.0), --Lay_Price_Type(Br(Integer(Idx)).Layprice),
                       Match_Directly  => Match_Directly);
      end case;
    else
      Log("Try_To_Make_Back_Bet_ai", "bad idx - no bet");

    end if;


  end Try_To_Make_Back_Bet_Ai;
  ------------------------------------------------------



  procedure Try_To_Make_Back_Bet(Bettype         : Config.Bet_Type;
                                 Br              : Best_Runners_Array_Type;
                                 Marketid        : Marketid_Type;
                                 Match_Directly  : Boolean := False) is

    Max_Backprice_1 : Fixed_Type;
    Min_Backprice_1 : Fixed_Type;
    Min_Backprice_N : Fixed_Type;
    Backed_Num      : Integer;
    Next_Num        : Integer;
    Tmp             : String (1 .. 5) := (others => ' ');
    Image           : String := Bettype'Img;
    Min_Price       : String (1 .. 4) := (others => '.');

  begin          --1         2       3
    --  12345678901234567890123456789012345
    --  HORSE_Back_1_10_20_1_4_WIN_1_02
    Tmp(1) := Image(12);
    Tmp(2) := '.';
    Tmp(3 .. 4) := Image(14 .. 15);
    Max_Backprice_1 := Fixed_Type'Value(Tmp);

    Min_Backprice_N := Fixed_Type'Value(Image(17 .. 18));
    Backed_Num := Integer'Value(Image(20 .. 20));
    Next_Num := Integer'Value(Image(22 .. 22));

    Min_Price(1)    := Image(28);
    Min_Price(3 .. 4) := Image(30 .. 31);

    case Bettype is
      when others  => Min_Backprice_1 := 1.01;
    end case;

    if Min_Backprice_1 <= Br(Backed_Num).Backprice and then Br(Backed_Num).Backprice <= Max_Backprice_1 and then
      Br(Next_Num).Backprice >= Min_Backprice_N and then
      Br(3).Backprice <  Fixed_Type(10_000.0) then  -- so it exists
      -- Back The leader in PLC market...

      Send_Back_Bet(Selectionid     => Br(Backed_Num).Selectionid,
                    Main_Bet        => Bettype,
                    Marketid        => Marketid,
                    Min_Price       => Back_Price_Type'Value(Min_Price),
                    Match_Directly  => Match_Directly);
    end if;
  end Try_To_Make_Back_Bet;
  ------------------------------------------------------


  -----------------------------------------------------------------------


  procedure Run(Market_Notification : in Bot_Messages.Market_Notification_Record) is
    Market     : Markets.Market_Type;
    Event      : Events.Event_Type;
    Price_List : Prices.Lists.List;
    --------------------------------------------
    function "<" (Left, Right : Prices.Price_Type) return Boolean is
    begin
      return Left.Backprice < Right.Backprice;
    end "<";
    --------------------------------------------
    package Backprice_Sorter is new Prices.Lists.Generic_Sorting("<");
    Animal            : Animal_Type := Human;
    Price             : Prices.Price_Type;
    Has_Been_In_Play,
    In_Play           : Boolean := False;
    First_Poll        : Boolean := True;
    Best_Runners      : Best_Runners_Array_Type := (others => Prices.Empty_Data);
    Unsorted_Runners  : Best_Runners_Array_Type := (others => Prices.Empty_Data);

    Eos               : Boolean := False;
    type Markets_Array_Type is array(Market_Type'Range) of Markets.Market_Type;
    Markets_Array     : Markets_Array_Type;
    Found_Place       : Boolean := True;
    T                 : Sql.Transaction_Type;
    Current_Turn_Not_Started_Race : Integer_4 := 0;
    Betfair_Result    : Rpc.Result_Type := Rpc.Result_Type'First;
    Saldo             : Balances.Balance_Type;
    Match_Directly    : Boolean := False;
    --Ts_In_Play        : Calendar2.Time_Type := Calendar2.Time_Type_First;
  begin
    Log(Me & "Run", "Treat market: " &  Market_Notification.Market_Id);
    Market.Marketid := Market_Notification.Market_Id;

    Set_Bet_Names;

    --set values from cfg
    for I in Bets_Allowed'Range loop
      Bets_Allowed(I).Bet_Size   := Cfg.Bet(I).Size;
      Bets_Allowed(I).Has_Betted := False;
      Bets_Allowed(I).Max_Loss_Per_Day := Bet_Size_Type(Cfg.Bet(I).Max_Loss_Per_Day);
      Bets_Allowed(I).Max_Earnings_Per_Day := Bet_Size_Type(Cfg.Bet(I).Max_Earnings_Per_Day);
    end loop;

    declare
      -- only bet on allowed days
      Now : Time_Type := Clock;
      Day : Week_Day_Type := Week_Day_Of(Now);
    begin
      if not Cfg.Allowed_Days(Day) then
        Log("No bet layed, bad weekday globally" );
        return;
      end if;
    end;

    -- check if ok to bet and set bet size
    Rpc.Get_Balance(Betfair_Result => Betfair_Result, Saldo => Saldo);

    if abs(Saldo.Exposure) > Cfg.Max_Exposure then
      Log(Me & "Run", "Too much exposure - skip this race " & Saldo.To_String);
      Log(Me & "Run", "max exposure is " & F8_Image(Cfg.Max_Exposure));
      return;
    end if;

    for I in Bets_Allowed'Range loop
      if 0.0 < Bets_Allowed(I).Bet_Size and then Bets_Allowed(I).Bet_Size < 1.0 then
        -- to have the size = a portion of the saldo.
        Bets_Allowed(I).Bet_Size := Bets_Allowed(I).Bet_Size * Saldo.Balance * Bets_Allowed(I).Bet_Size_Portion;
        if Bets_Allowed(I).Bet_Size < 30.0 then
          Log(Me & "Run", "Bet_Size too small, set to 30.0, was " & F8_Image(Fixed_Type( Bets_Allowed(I).Bet_Size)) & " " & Saldo.To_String);
          Bets_Allowed(I).Bet_Size := 30.0;
        end if;
      end if;
      Log(Me & "Run", "Bet_Size " & F8_Image(Fixed_Type( Bets_Allowed(I).Bet_Size)) & " " & Saldo.To_String);

      if -10.0 <= Bets_Allowed(I).Max_Loss_Per_Day and then Bets_Allowed(I).Max_Loss_Per_Day < 0.0 then
        Bets_Allowed(I).Max_Loss_Per_Day := Bets_Allowed(I).Max_Loss_Per_Day * Bets_Allowed(I).Bet_Size;
      end if;

      declare
        Todays_Profit : Fixed_Type := Bets.Profit_Today(Bets_Allowed(I).Bet_Name);
      begin
        Bets_Allowed(I).Is_Allowed_To_Bet := Todays_Profit >= Fixed_Type(Bets_Allowed(I).Max_Loss_Per_Day);
        Log(Me & "Run", Trim(Bets_Allowed(I).Bet_Name) & " max allowed loss set to " & F8_Image(Fixed_Type(Bets_Allowed(I).Max_Loss_Per_Day)));
        if not Bets_Allowed(I).Is_Allowed_To_Bet then
          Log(Me & "Run", Trim(Bets_Allowed(I).Bet_Name) & " is BACK bet OR has lost too much today, max loss is " & F8_Image(Fixed_Type(Bets_Allowed(I).Max_Loss_Per_Day)));
        end if;

        if Bets_Allowed(I).Is_Allowed_To_Bet then
          declare
            Max_Daily : Fixed_Type := Fixed_Type(Bets_Allowed(I).Max_Earnings_Per_Day);
          begin
            if Max_Daily <= Fixed_Type(5.0) then
              -- below 5 the amount is a percentage of bet_size
              Max_Daily := Fixed_Type(Bets_Allowed(I).Max_Earnings_Per_Day) * Fixed_Type(Bets_Allowed(I).Bet_Size);
            end if;

            Bets_Allowed(I).Is_Allowed_To_Bet := Todays_Profit <= Max_Daily;

            Log(Me & "Run", Trim(Bets_Allowed(I).Bet_Name) & " max allowed earnings (greed) set to " & F8_Image(Max_Daily));
            if not Bets_Allowed(I).Is_Allowed_To_Bet then
              Log(Me & "Run", Trim(Bets_Allowed(I).Bet_Name) & " has won too much today, limit is " & F8_Image(Max_Daily));
            end if;
          end;
        end if;
      end;
    end loop;

    --check for total loss today
    declare
      Todays_Profit_So_Far : Fixed_Type := 0.0;
    begin
      for I in Bets_Allowed'Range loop
        Todays_Profit_So_Far := Todays_Profit_So_Far + Bets.Profit_Today(Bets_Allowed(I).Bet_Name);
        if Todays_Profit_So_Far < Cfg.Max_Total_Loss_Per_Day then
          Log(Me & "Run", "today's profit is  " & F8_Image(Todays_Profit_So_Far));
          Log(Me & "Run", "we have lost too much today, max total loss is " & F8_Image(Cfg.Max_Total_Loss_Per_Day));
          Log(Me & "Run", "No more bets today ...");
          return;
        end if;

      end loop;
    end;

    Market.Read(Eos);
    if not Eos then
      if Market.Markettype(1 .. 3) /= "WIN"  then
        Log(Me & "Run", "not a WIN market: " &  Market_Notification.Market_Id);
        return;
      elsif not Market.Marketname_Ok2 then -- check Hrd/Chs in each bet
        Log(Me & "Run", "not an OK Marketname: " & Market.To_String);
        return;
      else
        Event.Eventid := Market.Eventid;
        Event.Read( Eos);
        if not Eos then
          if Event.Eventtypeid = Integer_4(7) then
            Animal := Horse;
          elsif Event.Eventtypeid = Integer_4(4339) then
            Animal := Hound;
          else
            Log(Me & "Run", "not a HORSE, nor a HOUND market: " &  Market_Notification.Market_Id);
            return;
          end if;

          if not Cfg.Country_Is_Ok(Event.Countrycode) then
            Log(Me & "Run", "not an OK country,  market: " &  Market_Notification.Market_Id);
            return;
          end if;
        else
          Log(Me & "Run", "no event found");
          return;
        end if;
      end if;
    else
      Log(Me & "Run", "no market found");
      return;
    end if;
    Markets_Array(Win) := Market;

    T.Start;
    Find_Plc_Market.Prepare(
                            "select MP.* from AMARKETS MW, AMARKETS MP " &
                              "where MW.EVENTID = MP.EVENTID " &
                              "and MW.STARTTS = MP.STARTTS " &
                              "and MW.MARKETID = :WINMARKETID " &
                              "and MP.MARKETTYPE = 'PLACE' " &
                              "and MP.NUMWINNERS = :NUM " &
                              "and MW.MARKETTYPE = 'WIN' " &
                              "and MP.STATUS = 'OPEN'" );

    case Animal is
      when Horse => Find_Plc_Market.Set("NUM", Integer_4(3));
      when Hound => Find_Plc_Market.Set("NUM", Integer_4(2));
      when Human => Find_Plc_Market.Set("NUM", Integer_4(1));
    end case;

    Find_Plc_Market.Set("WINMARKETID", Markets_Array(Win).Marketid);
    Find_Plc_Market.Open_Cursor;
    Find_Plc_Market.Fetch(Eos);
    if not Eos then
      Markets_Array(Place) := Markets.Get(Find_Plc_Market);
      if Markets_Array(Win).Startts /= Markets_Array(Place).Startts then
        Log(Me & "Make_Bet", "Wrong PLACE market found, give up");
        Found_Place := False;
      end if;
    else
      Log(Me & "Make_Bet", "no PLACE market found with 3 winners - exit poll loop");
      Found_Place := False;
    end if;
    Find_Plc_Market.Close_Cursor;
    T.Commit;

    -- do the poll
    Poll_Loop : loop

      exit Poll_Loop when not Found_Place;

      Price_List.Clear;
      Rpc.Get_Market_Prices(Market_Id  => Market_Notification.Market_Id,
                            Market     => Market,
                            Price_List => Price_List,
                            In_Play    => In_Play);

      exit Poll_Loop when Market.Status(1 .. 4) /= "OPEN" and then Has_Been_In_Play;

      if not Has_Been_In_Play then
        -- toggle the first time we see in-play=true
        -- makes us insensible to Betfair toggling bug
        Has_Been_In_Play := In_Play;
     --   Ts_In_Play := Price_List.First_Element.Pricets;
      end if;

      if not Has_Been_In_Play then
        if Current_Turn_Not_Started_Race >= Cfg.Max_Turns_Not_Started_Race then
          Log(Me & "Make_Bet", "Market took too long time to start, give up");
          exit Poll_Loop;
        else
          Current_Turn_Not_Started_Race := Current_Turn_Not_Started_Race + 1;
          delay 5.0; -- no need for heavy polling before start of race
        end if;
      else
        -- delay 0.05; -- to avoid more than 20 polls/sec
        delay 0.0;  -- pi need all cpu time it can get ... and network slow ..
      end if;


      --before sorting - for AI
      if First_Poll then
        declare
          Idx : Integer := 0;
        begin
          for Tmp of Price_List loop
            if Tmp.Status(1 .. 6) = "ACTIVE" then
              Idx := Idx + 1;
              exit when Idx > Unsorted_Runners'Last;
              Unsorted_Runners(Idx) := Tmp;
            end if;
          end loop;
        end ;
      end if;


      -- ok find the runner with lowest backprice:
      Backprice_Sorter.Sort(Price_List);

      Price.Backprice := 10_000.0;
      Best_Runners := (others => Price);

      declare
        Idx : Integer := 0;
      begin
        for Tmp of Price_List loop
          if Tmp.Status(1 .. 6) = "ACTIVE" then
            Idx := Idx + 1;
            exit when Idx > Best_Runners'Last;
            Best_Runners(Idx) := Tmp;
          end if;
        end loop;
      end ;

      for I in Best_Runners'Range loop
        Log("Best_Runners(i)" & I'Img & " " & Best_Runners(I).To_String);
      end loop;

      if Best_Runners(1).Backprice >= Fixed_Type(1.01) then
        for I in Bet_Type'Range loop
          Log("Animal " & Animal'Img & " betname " & I'Img & " First_Poll " & First_Poll'Img);
          case Animal is
            when Horse =>
              case I is
                when Horse_Back_1_10_07_1_2_Plc_1_01 .. Horse_Back_1_56_00_1_4_Plc_1_01 =>
                  declare
                    M_Type     : Market_Type := Win;
                    Image      : String := I'Img;
                    Do_Try_Bet : Boolean := True;
                    use Markets;
                  begin
                    --  12345678901234567890
                    --  Back_1_10_20_1_4_WIN
                    if Utils.Position(Image, "PLC") > Integer(0) then
                      M_Type := Place;
                      Do_Try_Bet := Found_Place and then Markets_Array(Place).Numwinners >= Integer_4(3) ;
                      Match_Directly := False;
                    elsif Utils.Position(Image, "WIN") > Integer(0) then
                      Do_Try_Bet := Markets_Array(Place).Numwinners >= Integer_4(3) ;
                      M_Type         := Win;
                      Match_Directly := False;
                    end if;

                    if Do_Try_Bet then
                      case Markets_Array(Win).Market_Subtype is
                        when Plain  => Do_Try_Bet := not (Cfg.Bet(I).Chase_Allowed or Cfg.Bet(I).Hurdle_Allowed);
                        when Chase  => Do_Try_Bet := Cfg.Bet(I).Chase_Allowed;
                        when Hurdle => Do_Try_Bet := Cfg.Bet(I).Hurdle_Allowed;
                      end case;
                    end if;


                    if Do_Try_Bet and then
                      Has_Been_In_Play then

                      Try_To_Make_Back_Bet(Bettype         => I,
                                           Br              => Best_Runners,
                                           Marketid        => Markets_Array(M_Type).Marketid,
                                           Match_Directly  => Match_Directly);
                    end if;
                  end;

--                when Horse_Back_AI_Nfl_1_Hn_100_Lr_0p10_E_12_Plc .. Horse_Back_AI_NFL_0_HN_300_LR_1p00_E_12_Win =>
--                  if First_Poll then
--                    declare
--                      M_Type     : Market_Type := Win;
--                      Image      : String := I'Img;
--                      Do_Try_Bet : Boolean := True;
--                      use Markets;
--                    begin
--                      if Utils.Position(Image, "PLC") > Integer(0) then
--                        M_Type := Place;
--                        Do_Try_Bet := Found_Place and then Markets_Array(Place).Numwinners = Integer_4(3) ;
--                        Match_Directly := True;
--                      elsif Utils.Position(Image, "WIN") > Integer(0) then
--                        Do_Try_Bet := Markets_Array(Place).Numwinners = Integer_4(3) ;
--                        M_Type         := Win;
--                        Match_Directly := True;
--                      end if;
--
--                      if Do_Try_Bet then
--                        case Markets_Array(Win).Market_Subtype is
--                          when Plain  => Do_Try_Bet := not (Cfg.Bet(I).Chase_Allowed or Cfg.Bet(I).Hurdle_Allowed);
--                          when Chase  => Do_Try_Bet := Cfg.Bet(I).Chase_Allowed;
--                          when Hurdle => Do_Try_Bet := Cfg.Bet(I).Hurdle_Allowed;
--                        end case;
--                      end if;
--
--                      if Do_Try_Bet then
--                        Try_To_Make_Back_Bet_AI(Bettype         => I,
--                                                Br              => Unsorted_Runners,
--                                                Marketid        => Markets_Array(M_Type).Marketid,
--                                                Win_Marketname  => Market.Marketname,
--                                                Match_Directly  => Match_Directly);
--                    end if;
--                  end;
--                  end if;
              end case;

            when Hound => null;
            when Human => null;
          end case;
        end loop;

      end if; -- Best_Runner(1).Backodds >= 1.01

      First_Poll := False;
    end loop Poll_Loop;

  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
  ------------------------------ main start -------------------------------------

begin

  Define_Switch
    (Cmd_Line,
     Sa_Par_Bot_User'Access,
     Long_Switch => "--user=",
     Help        => "user of bot");

  Define_Switch
    (Cmd_Line,
     Ba_Daemon'Access,
     Long_Switch => "--daemon",
     Help        => "become daemon at startup");

  Define_Switch
    (Cmd_Line,
     Sa_Par_Inifile'Access,
     Long_Switch => "--inifile=",
     Help        => "use alternative inifile");

  Getopt(Cmd_Line);  -- process the command line

  if Ba_Daemon then
    Posix.Daemonize;
  end if;

  --must take lock AFTER becoming a daemon ...
  --The parent pid dies, and would release the lock...
  My_Lock.Take(Ev.Value("BOT_NAME"));

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Ev.Value("BOT_NAME") & ".log");

  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Cfg := Config.Create(Ev.Value("BOT_HOME") & "/" & Sa_Par_Inifile.all);
  Log(Cfg.To_String);
  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");

  Log(Me, "Login betfair");
  Rpc.Init (
            Username   => Ini.Get_Value("betfair", "username", ""),
            Password   => Ini.Get_Value("betfair", "password", ""),
            Product_Id => Ini.Get_Value("betfair", "product_id", ""),
            Vendor_Id  => Ini.Get_Value("betfair", "vendor_id", ""),
            App_Key    => Ini.Get_Value("betfair", "appkey", "")
           );
  Rpc.Login;
  Log(Me, "Login betfair done");



  if Cfg.Enabled then
    Cfg.Enabled := Ev.Value("BOT_MACHINE_ROLE") = "PROD";
  end if;

--  Sim.Fill_Race_Times(Horse, Sim.Racetime_Map);

  Main_Loop : loop

    --notfy markets_fetcher that we are free
    Data := (Free => 1, Name => This_Process.Name , Node => This_Process.Node);
    Bot_Messages.Send(Markets_Fetcher, Data);

    begin
      Log(Me, "Start receive");
      Process_Io.Receive(Msg, Timeout);
      Log(Me, "msg : " & Process_Io.Identity(Msg)'Img & " from " & Trim(Process_Io.Sender(Msg).Name));
      if Sql.Transaction_Status /= Sql.None then
        raise Sql.Transaction_Error with "Uncommited transaction in progress !! BAD!";
      end if;
      case Process_Io.Identity(Msg) is
        when Core_Messages.Exit_Message                  =>
          exit Main_Loop;
        when Bot_Messages.Market_Notification_Message    =>
          if Cfg.Enabled then
            --notfy markets_fetcher that we are busy
            Data := (Free => 0, Name => Process_Io.This_Process.Name , Node => Process_Io.This_Process.Node);
            Bot_Messages.Send(Markets_Fetcher, Data);

            Log(Me, "Connect Db");
            Sql.Connect
              (Host     => Ini.Get_Value("database", "host", ""),
               Port     => Ini.Get_Value("database", "port", 5432),
               Db_Name  => Ini.Get_Value("database", "name", ""),
               Login    => Ini.Get_Value("database", "username", ""),
               Password => Ini.Get_Value("database", "password", ""));
            Log(Me, "db Connected");

            Run(Bot_Messages.Data(Msg));

            Log(Me, "Close Db");
            Sql.Close_Session;

          else
            Log(Me, "Poll is not enabled in poll.ini");
          end if;
        when others =>
          Log(Me, "Unhandled message identity: " & Process_Io.Identity(Msg)'Img);  --??
      end case;
    exception
      when Process_Io.Timeout =>
        Rpc.Keep_Alive(Ok);
        if not Ok then
          begin
            Rpc.Login;
          exception
            when Rpc.Login_Failed =>
              Log(Me, "login failed, but will try again");
          end;
        end if;
    end;
    Now := Calendar2.Clock;

    --restart every day
    Is_Time_To_Exit := Now.Hour = 01 and then
      ( Now.Minute = 00 or Now.Minute = 01) ; -- timeout = 2 min

    exit Main_Loop when Is_Time_To_Exit;

  end loop Main_Loop;

  Rpc.Logout;
  Logging.Close;
  Posix.Do_Exit(0); -- terminate

exception
  when Lock.Lock_Error =>
    Log(Me, "lock error, exit");
    Logging.Close;
    Posix.Do_Exit(0); -- terminate
  when E : others =>
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
    Posix.Do_Exit(0); -- terminate
end Poll;

