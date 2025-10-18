
with Ada.Exceptions;
with Ada.Command_Line;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Containers.Doubly_Linked_Lists;

with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
with Posix;
with Ini;
with Logging; use Logging;
with Markets;
--with Table_Aevents;
--with Table_Aprices;
--with Table_Abalances;
with Table_Arunners;
with Table_Abets;
with Table_Apriceshistory;
with Bot_Svn_Info;
--with Utils; use Utils;
with Bot_System_Number;


procedure Check_for_Lay_Win is
  package EV renames Ada.Environment_Variables;
  T                   : Sql.Transaction_Type;
  Select_Cand         : Sql.Statement_Type;
  Select_Ph           : Sql.Statement_Type;
  Select_Markets      : Sql.Statement_Type;
  Select_Runner       : Sql.Statement_Type;
  Select_Timestamps   : Sql.Statement_Type;
  Select_Selectionids : Sql.Statement_Type;
  Find_Plc_Market     : Sql.Statement_Type;
  Comission         : constant Fixed_Type := 0.0 / 100.0;

  Me                  : constant String := "Check_for_Lay_Win.";
  Global_Bet_Name     : Betname_Type := (others => ' ');
  Global_Laysize   : Bet_Size_Type := 30.0;

  --Global_Max_Backprice: Fixed_Type := 0.0;
  Cmd_Line            : Command_Line_Configuration;
  IA_Tics_Greenup     : aliased Integer := 0;
  IA_Tics_Stoploss    : aliased Integer := 0;

  SA_Max_Price     : aliased Gnat.Strings.String_Access;
  SA_Min_Price     : aliased Gnat.Strings.String_Access;
  --SA_Max_Backprice    : aliased Gnat.Strings.String_Access;
  Sa_Betname          : aliased Gnat.Strings.String_Access;

  package Ts_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Calendar2.Time_Type);
  Ts_List             : Ts_List_Pack.List;

  Bet_List : Table_Abets.Abets_List_Pack2.List;
  type Bets_Type is (LAY_1_30_05_WIN_2_00
                       --, LAY_2_90_20_WIN_8_00
                       );
  
  type Allowed_Type is record
    Has_Betted        : Boolean       := False;
    Is_Matched        : Boolean       := False;
  end record;

  Bets_Allowed : array (Bets_Type'range) of Allowed_Type;
  

  type Best_Runners_Array_Type is array (1..50) of Table_Apriceshistory.Data_Type ;
  --Best_Runners : Best_Runners_Array_Type := (others => Table_Apriceshistory.Empty_Data);
  

  ---------------------------------------------------

  procedure Place_Lay(M : Markets.Market_Type;
                      R : Table_Arunners.Data_Type;
                      P : Table_Apriceshistory.Data_Type) is
    Bet : Table_Abets.Data_Type;
    Now : Calendar2.Time_Type := Calendar2.Clock;
    Betname     : Betname_Type := Global_Bet_Name;
    Laysize       : Bet_Size_Type := Global_Laysize;
  begin
    Betname(1..36) :=  "LAY_1_80_10_WIN_4_10_DIRECT_MATCH_NO";
    Bet := (
        Betid          => Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid)),
        Marketid       => M.Marketid,
        Betmode        => Bot_Mode(Simulation),
        Powerdays      => 0,
        Selectionid    => R.Selectionid,
        Reference      => (others => '-'),
        Size           => Fixed_Type(Laysize),
        Price          => Fixed_Type(20.0) ,
        Side           => "LAY ",
        Betname        => Betname,
        Betwon         => False,
        Profit         => 0.0,
        Status         => (others => ' '),
        Exestatus      => (others => ' '),
        Exeerrcode     => (others => ' '),
        Inststatus     => (others => ' '),
        Insterrcode    => (others => ' '),
        Startts        => M.Startts,
        Betplaced      => P.Pricets,
        Pricematched   => Fixed_Type(0.0),
        Sizematched    => Fixed_Type(Laysize),
        Runnername     => R.Runnernamestripped,
        Fullmarketname => M.Marketname,
        Svnrevision    => Bot_Svn_Info.Revision,
        Ixxlupd        => (others => ' '), --set by insert
        Ixxluts        => Now              --set by insert
      );
      Bet_List.Append(Bet);
  end Place_Lay;
  ------------------------------------------------

  function Matched_Lay(BR : Best_Runners_Array_Type) return Boolean is
  begin
    for B of Bet_List loop
      for i in BR'range loop
        if BR(i).Selectionid = B.Selectionid and then
           BR(i).Pricets >= B.Betplaced + (0,0,0,1,0) then -- + 1 s
    
          if B.Status(1) = 'M' then
            return True; -- matched before
          elsif BR(i).Layprice <= B.Price and then
                BR(i).Layprice >= Fixed_Type(1.01) then
            B.Status(1) := 'M';
            B.Pricematched := BR(i).Layprice;
            return True;
          else
           -- B.Status(1) := 'L';
            return False;
            --return True;
          end if;
        end if;
      end loop;
    end loop;
    return False;
  end Matched_Lay;
  ---------------------------------------------------

  procedure Check_Bet_Won ( R : in     Table_Arunners.Data_Type;
                            B : in out Table_Abets.Data_Type) is
  begin
    if R.Status(1..7) = "REMOVED" then
      return;
    end if;
    B.Runnername := R.Runnernamestripped;
    if B.Side(1..4) = "BACK" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := True;
        elsif R.Status(1..5) = "LOSER" then
          B.Betwon := False;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
    elsif B.Side(1..3) = "LAY" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := False;
        elsif R.Status(1..5) = "LOSER" then
          B.Betwon := True;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
    end if;
  end Check_Bet_Won;
  ----------------------------------------------------------------
  procedure Check_Bet_Profit (B : in out Table_Abets.Data_Type) is
  begin
    if B.Pricematched < 0.5 then
      B.Profit := 0.0;
      return;
    end if;

    if B.Side(1..4) = "BACK" then
      if B.Betwon then
        B.Profit := (1.0 - Comission) * B.Sizematched * (B.Pricematched - 1.0);
      else
        B.Profit := -B.Sizematched;
      end if;
    elsif B.Side(1..3) = "LAY" then
      if B.Betwon then
        B.Profit := (1.0 - Comission) * B.Sizematched;
      else
        B.Profit := -B.Sizematched * (B.Pricematched - 1.0);
      end if;
    end if;
  end Check_Bet_Profit;
  ----------------------------------------------------------------

  procedure Calculate_Profit_And_Save_To_Db is
    Runner: Table_Arunners.Data_Type;
    Eos : Boolean := False;
  begin
    for B of Bet_List loop
      Runner := Table_Arunners.Empty_Data;
      Runner.Marketid    := B.Marketid;
      Runner.Selectionid := B.Selectionid;
      Runner.Read(Eos);
      if B.Status(1) = 'M' then
        B.Status(1..7) := "SETTLED";
        Check_Bet_Won(Runner,B);
        Check_Bet_Profit(B);
      elsif B.Status(1) = 'C' then
        B.Sizematched := 0.0;
        B.Profit := 0.0;
      else
        B.Status(1..6) := "LAPSED";
        B.Sizematched := 0.0;
        B.Profit := 0.0;
      end if;
      B.Insert;
      Log(Me & "Calculate_Profit_And_Save_To_Db" , B.To_String);
    end loop;
  end Calculate_Profit_And_Save_To_Db;
  --------------------------------------------------

  procedure Try_To_Make_Lay_Bet(
      Bettype         : Bets_Type;
      BR              : Best_Runners_Array_Type;
      Market          : Markets.Market_Type) is

    Max_Backprice_1       : Fixed_Type;
    Max_Layprice_n        : Max_Lay_Price_Type;
    Additional_Layprice_n : Max_Lay_Price_Type;
    pragma Unreferenced (Additional_Layprice_n);
    Layed_Num             : Integer;
    Tmp    : String (1..4) := (others => ' ');
    Image  : String := Bettype'Img;
    Runner : Table_Arunners.Data_Type;  
    Eos : Boolean := False;  
  begin          --1         2         3
      --  123456789012345678901234567890123456789
      --  Lay_2_30_10_WIN_4_02
      --  Lay_1_80_10_WIN_4_10,

    if Bets_Allowed(Bettype).Is_Matched then
      return;
    end if;

    Tmp(1)    := Image(5);
    Tmp(2)    := '.';
    Tmp(3..4) := Image(7..8);
    Max_Backprice_1 := Fixed_Type'Value(Tmp);
    
    Tmp := (others => ' ');
    Tmp(1..2) := Image(10..11);
    Max_Layprice_n := Max_Lay_Price_Type'Value(Tmp);

    Tmp := (others => ' ');
    Tmp(1) := Image(17);
    Layed_Num := Integer'Value(Tmp);
    
    Bets_Allowed(Bettype).Is_Matched := Matched_Lay(BR);
    
    if Bets_Allowed(Bettype).Has_Betted then
      return;
    end if;

    Tmp := (others => ' ');
    Tmp(1..2) := Image(19..20);
    Additional_Layprice_n :=  Max_Lay_Price_Type'Value(Tmp);

    if BR(1).Backprice <= Max_Backprice_1 and then
       BR(1).Backprice >= Fixed_Type(1.01) and then
       BR(Layed_Num).Layprice >  Fixed_Type(1.01) and then  -- so it exists
       BR(Layed_Num).Layprice <= Max_Layprice_n then 
      -- lay n in WIN market...
        Runner := Table_Arunners.Empty_Data;
        Runner.Marketid    := BR(Layed_Num).Marketid;
        Runner.Selectionid := BR(Layed_Num).Selectionid;
        Runner.Read(Eos);

        Place_Lay(Market, Runner, BR(Layed_Num));
        Bets_Allowed(Bettype).Has_Betted := True;
    end if;
  end Try_To_Make_Lay_Bet;

  --------------------------------------------------

  procedure Read_Ts(Stm  : in out Sql.Statement_Type;
                    List : in out Ts_List_Pack.List) is
    Eos : Boolean := False;
    Ts : Calendar2.Time_Type;
  begin
     Stm.Open_Cursor;
     loop
       Stm.Fetch(Eos);
       exit when Eos;
       Stm.Get("PRICETS", Ts);
       List.Append(Ts);
     end loop;
     Stm.Close_Cursor;
  end Read_Ts;
  --------------------------------------------------

begin
  if not EV.Exists("BOT_NAME") then
    EV.Set("BOT_NAME","check_for_green");
  end if;
 -- Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Define_Switch
     (Cmd_Line,
      Sa_Betname'access,
      Long_Switch => "--betname=",
      Help        => "betname for equity");

  Define_Switch
     (Cmd_Line,
      SA_Max_Price'access,
      Long_Switch => "--max_price=",
      Help        => "Max price");

  Define_Switch
     (Cmd_Line,
      SA_Min_Price'access,
      Long_Switch => "--min_price=",
      Help        => "Min price");

  Define_Switch
     (Cmd_Line,
      IA_Tics_Greenup'access,
      Long_Switch => "--tics_greenup=",
      Help        => "Tics greenup");

  Define_Switch
     (Cmd_Line,
      IA_Tics_Stoploss'access,
      Long_Switch => "--tics_stoploss=",
      Help        => "Tics stoploss");


  Getopt (Cmd_Line);  -- process the command line

  --Move(SA_Betname.all,Global_Bet_Name);
  --Global_Max_Price := Fixed_Type'Value(SA_Max_Price.all);
  --Global_Min_Price := Fixed_Type'Value(SA_Min_Price.all);
 
  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
        (Host     => Ini.Get_Value("stats", "host", ""),
         Port     => Ini.Get_Value("stats", "port", 5432),
         Db_Name  => Ini.Get_Value("stats", "name", ""),
         Login    => Ini.Get_Value("stats", "username", ""),
         Password => Ini.Get_Value("stats", "password", ""));
  Log(Me, "db Connected");

  Select_Runner.Prepare(
      "select * from APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "and BACKPRICE <= :BACKPRICE " &
      "order by PRICETS "
  );

  Select_Markets.Prepare(
      "select * " &
      "from " &
      "AMARKETS " &
      "where MARKETTYPE = 'WIN' " &
      "and STARTTS::date >= '2016-03-27' " & -- we do not have good data before that time
      "order by STARTTS"
  );

  Select_Selectionids.Prepare(
      "select * " &
      "from " &
      "ARUNNERS " &
      "where MARKETID = :MARKETID " &
      "and STATUS in ('WINNER','LOSER') " &
      "order by SORTPRIO desc"
  );

  Select_Cand.Prepare(
      "select * " &
      "from APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "and PRICETS = :PRICETS " &
      "order by BACKPRICE"
  );

  Select_Ph.Prepare(
      "select * " &
      "from " &
      "APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "and SELECTIONID = :SELECTIONID " &
      "and PRICETS >= :PRICETS " &
      "order by PRICETS"
  );

  Select_Timestamps.Prepare(
      "select distinct(PRICETS) " &
      "from " &
      "APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "order by PRICETS"
  );

  Find_Plc_Market.Prepare(
    "select MP.* from AMARKETS MW, AMARKETS MP " &
    "where MW.EVENTID = MP.EVENTID " &
    "and MW.STARTTS = MP.STARTTS " &
    "and MW.MARKETID = :WINMARKETID " &
    "and MP.MARKETTYPE = 'PLACE' " &
    "and MP.NUMWINNERS = 3 " &
    "and MW.MARKETTYPE = 'WIN' "
  ) ;

    declare
      Ph_List     : Table_Apriceshistory.Apriceshistory_List_Pack2.List;
      Market_List : Markets.Lists.List;
      Cnt         : Natural := 0;
      type Has_Type is (Back,Lay,Stoploss);
      pragma Unreferenced (Back, Lay, Stoploss);
      subtype Max_Runners_Type is Integer_4 range 1 .. 50;
      Placed  : array (Max_Runners_Type'range, Has_Type'range) of Boolean := (others => (others => False));
      pragma Unreferenced (Placed);
      Matched : array (Max_Runners_Type'range, Has_Type'range) of Boolean := (others => (others => False));
      pragma Unreferenced (Matched);

    begin
      T.Start;
      Log(Me & "Main" , "read start");
      Markets.Read_List(Select_Markets, Market_List);
      Log(Me & "Main" , "read done");
      T.Commit;

      Market_Loop : for Market of Market_List loop
        for i in Bets_Type'range loop
          Bets_Allowed(i).Has_Betted := False;
          Bets_Allowed(i).Is_Matched := False;
        end loop;
        
        T.Start;
        Cnt := Cnt +1;
        if Cnt rem 100 = 0 then
          Log(Me & "Main" , "treat: " & Market.To_String);
        end if;
        Bet_List.Clear;
        Ts_List.Clear;
        Placed  := (others => (others => False));
        Matched := (others => (others => False));
        Select_Timestamps.Set("MARKETID", Market.Marketid);
        Read_Ts(Select_Timestamps, Ts_List);
        --Has := (others => False);

        Timestamp_Loop : for Ts of Ts_List loop
          Ph_List.Clear;
          Select_Cand.Set("MARKETID",Market.Marketid);
          Select_Cand.Set("PRICETS",Ts);
          Table_Apriceshistory.Read_List(Select_Cand, Ph_List);

          declare
            Idx : Integer := 0;
            Best_Runners : Best_Runners_Array_Type := (others => Table_Apriceshistory.Empty_Data);
          begin
            Ph_Loop : for Ph of Ph_List loop
              Idx := Idx +1;
              exit Ph_Loop when Idx > Best_Runners'Last;
              Best_Runners(Idx) := Ph;
            end loop Ph_Loop;
            -- Best_Runners is sorted lowest backprice to highest, max 20 entries

            for i in Bets_Type'range loop
              Try_To_Make_Lay_Bet(i, Best_Runners, Market);              
              exit Timestamp_Loop when Bets_Allowed(i).Is_Matched;
            end loop; -- BT'range            
          end;
        end loop Timestamp_Loop;
        Calculate_Profit_And_Save_To_Db;
        if Cnt rem 100 = 0 then
          Log(Me & "Main" , "Calculate_Profit_And_Save_To_Db");
        end if;

        T.Commit;
      end loop Market_Loop;
    end ;


exception
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
    Posix.Do_Exit(0); -- terminate

end Check_for_Lay_Win;
