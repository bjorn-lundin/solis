
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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
with Market;
--with Table_Aevents;
--with Table_Aprices;
--with Table_Abalances;
with Runner;
with Bet;
with Price_History;
with Bot_Svn_Info;
--with Utils; use Utils;
with Bot_System_Number;
with Tics;

procedure Check_for_Greenup_Win3 is
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

  Me                  : constant String := "Check_for_Greenup_Win3.";
  Global_Bet_Name     : Betname_Type := (others => ' ');
  Global_Max_Price : Fixed_Type := 0.0;
  Global_Min_Price : Fixed_Type := 0.0;
  Global_Laysize   : Bet_Size_Type := 50.0;

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

  Bet_List : Bet.List_Pack.List;

  -----------------------------------------------------
 -- procedure Place_Market(M     : in     Market_Type;
 --                        P     : in out Market_Type;
 --                        Found : in out Boolean) is
 --   P : Market_Type ;
 -- begin
 --   Find_Plc_Market.Set("WINMARKETID", M.Marketid);
 --   Find_Plc_Market.Open_Cursor;
 --   Find_Plc_Market.Fetch(Eos);
 --   if not Eos then
 --     P := Table_Amarkets.Get(Find_Plc_Market);
 --     Found := M.Startts = P.Startts;
 --   else
 --     Found := False;
 --   end if;
 --   Find_Plc_Market.Close_Cursor;
 -- end Place_Market;


 -- function Check_Back_Bet_Matched(P : Price_History.Price_History_Type) return Boolean is
 -- begin
 --   for B of Bet_List loop
 --     if B.Selectionid = P.Selectionid then
 --       if B.Status(1) = 'M' then
 --         return True; -- matched before
 --       elsif P.Backprice >= B.Price then
 --         B.Status(1) := 'M';
 --         return True;
 --       else
 --         return False;
 --       end if;
 --     end if;
 --   end loop;
 --   return False;
 -- end Check_Back_Bet_Matched;

  ---------------------------------------------------

--  procedure Place_Lay(M : Market.Market_Type;
--                      R : Runner.Runner_Type;
--                      P : Price_History.Price_History_Type) is
--    Bet : Bet.Bet_Type;
--    Now : Calendar2.Time_Type := Calendar2.Clock;
--    Betname     : Betname_Type := Global_Bet_Name;
--    Laysize       : Bet_Size_Type := Global_Laysize;
--  begin
--    Betname(1..3) :=  "GRL";
--    
--    Bet.Place(Betname, Size, Price, Runnername, M,R);
--    
--    
--    Bet := (
--        Betid          => Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid)),
--        Marketid       => M.Marketid,
--        Betmode        => Bot_Mode(Simulation),
--        Powerdays      => 0,
--        Selectionid    => R.Selectionid,
--        Reference      => (others => '-'),
--        Size           => Fixed_Type(Laysize),
--        Price          => P.Layprice ,
--        Side           => "LAY ",
--        Betname        => Betname,
--        Betwon         => False,
--        Profit         => 0.0,
--        Status         => (others => ' '),
--        Exestatus      => (others => ' '),
--        Exeerrcode     => (others => ' '),
--        Inststatus     => (others => ' '),
--        Insterrcode    => (others => ' '),
--        Startts        => M.Startts,
--        Betplaced      => P.Pricets,
--        Pricematched   => Fixed_Type(0.0),
--        Sizematched    => Fixed_Type(Laysize),
--        Runnername     => R.Runnernamestripped,
--        Fullmarketname => M.Marketname,
--        Svnrevision    => Bot_Svn_Info.Revision,
--        Ixxlupd        => (others => ' '), --set by insert
--        Ixxluts        => Now              --set by insert
--      );
--      Bet_List.Append(Bet);
--  end Place_Lay;
  ------------------------------------------------
  procedure Place_Back(M : Market.Market_Type;
                       R : Runner.Runner_Type;
                       P : Price_History.Price_History_Type) is
    The_Bet : Bet.Bet_Type;
    Now : Calendar2.Time_Type := Calendar2.Clock;
    Betname     : Betname_Type := Global_Bet_Name;
    Laysize          : Bet_Size_Type := Global_Laysize;
    Layprice_Tics    : Integer := Tics.Get_Tic_Index(P.Layprice);
    Green_Backprice  : Back_Price_Type := Back_Price_Type(Tics.Get_Tic_Price(Layprice_Tics +IA_Tics_Greenup));
    Green_Backsize   : Bet_Size_Type := Tics.Get_Green_Size(Lay_Price_Type(P.Layprice), Laysize, Green_Backprice);

  begin

    Betname(1..3) :=  "GRB";
    The_Bet := (
      Betid          => Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid)),
      Marketid       => M.Marketid,
      Betmode        => Bot_Mode(Simulation),
      Powerdays      => 0,
      Selectionid    => R.Selectionid,
      Reference      => (others => '-'),
      Size           => Fixed_Type(Green_Backsize),
      Price          => Fixed_Type(Green_Backprice),
      Side           => "BACK",
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
      Sizematched    => Fixed_Type(Green_Backsize),
      Runnername     => R.Runnernamestripped,
      Fullmarketname => M.Marketname,
      Svnrevision    => Bot_Svn_Info.Revision,
      Ixxlupd        => (others => ' '), --set by insert
      Ixxluts        => Now              --set by insert
    );
  --  Bet_List.Append(Bet);
  end Place_Back;

  ---------------------------------------------------
  procedure Place_Stoploss(M : Market.Market_Type;
                           R : Runner.Runner_Type;
                           P : Price_History.Price_History_Type) is
    The_Bet : Bet.Bet_Type;
    Now : Calendar2.Time_Type := Calendar2.Clock;
    Betname         : Betname_Type := Global_Bet_Name;
    Layprice_Tics   : Integer := 0;
    Stop_Backprice  : Back_Price_Type := 0.0;
    Stop_Backsize   : Bet_Size_Type := 0.0;
    Old_Lay : Bet.Bet_Type ;

  begin
    for B of Bet_List loop
      if B.Selectionid = R.Selectionid then      
        if    B.Betname(1..3) = "GRL" then
           Old_Lay := B;
        elsif B.Betname(1..3) = "GRB" then
           B.Sizematched := 0.0;
           B.Status(1..9) := "CANCELLED";
        end if;
      end if;
    end loop;

    if Old_Lay.Betname(1..3) /= "GRL" then
      return; -- not found old
    end if;

    Layprice_Tics  := Tics.Get_Tic_Index(Old_Lay.Price);
    Stop_Backprice := Back_Price_Type(Tics.Get_Tic_Price(Layprice_Tics -IA_Tics_Stoploss));
    Stop_Backsize  := Tics.Get_Green_Size(Lay_Price_Type(Old_Lay.Price), Bet_Size_Type(Old_Lay.Size), Stop_Backprice);

    Betname(1..3) :=  "GRS";
    The_Bet := (
      Betid          => Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid)),
      Marketid       => M.Marketid,
      Betmode        => Bot_Mode(Simulation),
      Powerdays      => 0,
      Selectionid    => R.Selectionid,
      Reference      => (others => '-'),
      Size           => Fixed_Type(Stop_Backsize),
      Price          => Fixed_Type(Stop_Backprice),
      Side           => "BACK",
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
      Sizematched    => Fixed_Type(Stop_Backsize),
      Runnername     => R.Runnernamestripped,
      Fullmarketname => M.Marketname,
      Svnrevision    => Bot_Svn_Info.Revision,
      Ixxlupd        => (others => ' '), --set by insert
      Ixxluts        => Now              --set by insert
    );
   -- Bet_List.Append(Bet);
  end Place_Stoploss;
  --------------------------------------------------

  function Matched_Lay(Ph : Price_History.Price_History_Type) return Boolean is
  begin
    for B of Bet_List loop
      if B.Selectionid = Ph.Selectionid and then      -- correct runner
         B.Betname(1..3) = "GRL" and then            -- laybet
         Ph.Pricets >= B.Betplaced + (0,0,0,1,0) then -- + 1 s

        if B.Status(1) = 'M' then
          return True; -- matched before
        elsif Ph.Layprice <= B.Price then
          B.Status(1) := 'M';
          B.Pricematched := Ph.Layprice;
          return True;
        else
          return False;
        end if;
      end if;
    end loop;
    return False;
  end Matched_Lay;

  --------------------------------------------------
  function Matched_Back(Ph : Price_History.Price_History_Type) return Boolean is
  begin
    for B of Bet_List loop
      if B.Selectionid = Ph.Selectionid and then      -- correct runner
         B.Betname(1..3) = "GRB" and then            -- backbet
         Ph.Pricets >= B.Betplaced + (0,0,0,1,0) then -- + 1 s

        if B.Status(1) = 'M' then
          return True; -- matched before
        elsif Ph.Backprice >= B.Price then
          B.Status(1) := 'M';
          B.Pricematched := Ph.Backprice;
          return True;
        else
          return False;
        end if;
      end if;
    end loop;
    return False;
  end Matched_Back;

  --------------------------------------------------

  function Matched_Stoploss(Ph : Price_History.Price_History_Type) return Boolean is
  begin
    for B of Bet_List loop
      if B.Selectionid = Ph.Selectionid and then      -- correct runner
         B.Betname(1..3) = "GRS" and then            -- backbet
         Ph.Pricets >= B.Betplaced + (0,0,0,1,0) then -- + 1 s

        if B.Status(1) = 'M' then
          return True; -- matched before
        elsif Ph.Backprice >= B.Price then
          B.Status(1) := 'M';
          B.Pricematched := Ph.Backprice;
          return True;
        else
          return False;
        end if;
      end if;
    end loop;
    return False;
  end Matched_Stoploss;

  ---------------------------------------------------

  procedure Check_Bet_Won ( R : in     Runner.Runner_Type;
                            B : in out Bet.Bet_Type) is
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
  procedure Check_Bet_Profit (B : in out Bet.Bet_Type) is
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
    The_Runner: Runner.Runner_Type;
    Eos : Boolean := False;
  begin
    for B of Bet_List loop
     -- The_Runner := Table_Arunners.Empty_Data;
      The_Runner.Marketid    := B.Marketid;
      The_Runner.Selectionid := B.Selectionid;
      The_Runner.Read(Eos);
      if B.Status(1) = 'M' then
        B.Status(1..7) := "SETTLED";
        Check_Bet_Won(The_Runner,B);
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

  Move(SA_Betname.all,Global_Bet_Name);
  Global_Max_Price := Fixed_Type'Value(SA_Max_Price.all);
  Global_Min_Price := Fixed_Type'Value(SA_Min_Price.all);
 
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
      Ph_List     : Price_History.List_Pack.List;
      Market_List : Market.List_Pack.List;
      Cnt         : Natural := 0;
      type Has_Type is (Back,Lay,Stoploss);
      subtype Max_Runners_Type is Integer_4 range 1 .. 50;
      Placed  : array (Max_Runners_Type'range, Has_Type'range) of Boolean := (others => (others => False));
      Matched : array (Max_Runners_Type'range, Has_Type'range) of Boolean := (others => (others => False));

    begin
      T.Start;
      Log(Me & "Main" , "read start");
     -- Table_Amarkets.Read_List(Select_Markets, Market_List);
      Market.Read_List(Select_Markets, Market_List);
      Log(Me & "Main" , "read done");
      T.Commit;

      Market_Loop : for The_Market of Market_List loop
        T.Start;
        Cnt := Cnt +1;
        if Cnt rem 100 = 0 then
          Log(Me & "Main" , "treat: " & The_Market.To_String);
        end if;
        Bet_List.Clear;
        Ts_List.Clear;
        Placed  := (others => (others => False));
        Matched := (others => (others => False));
        Select_Timestamps.Set("MARKETID", The_Market.Marketid);
        Read_Ts(Select_Timestamps, Ts_List);
        --Has := (others => False);

        Timestamp_Loop : for Ts of Ts_List loop
          Ph_List.Clear;
          Select_Cand.Set("MARKETID",The_Market.Marketid);
          Select_Cand.Set("PRICETS",Ts);
          Price_History.Read_List(Select_Cand, Ph_List);

          declare
            Idx : Integer_4 := 0;
            type Best_Runners_Array_Type is array (Max_Runners_Type'range) of Price_History.Price_History_Type ;
            Best_Runners : Best_Runners_Array_Type := (others => Price_History.Empty_Data);
            The_Runner       : Runner.Runner_Type;
            Eos          : Boolean := False;
            The_Bet      : Bet.Bet_Type;
          begin
            Ph_Loop : for Ph of Ph_List loop
              Idx := Idx +1;
              exit Ph_Loop when Idx > Best_Runners'Last;
              Best_Runners(Idx) := Ph;
            end loop Ph_Loop;
            -- Best_Runners is sorted lowest backprice to highest, max 20 entries

            for i in Best_Runners'range loop
              The_Runner := Runner.Empty_Data;
              The_Runner.Marketid    := Best_Runners(i).Marketid;
              The_Runner.Selectionid := Best_Runners(i).Selectionid;
              The_Runner.Read(Eos);
              
              if not Eos then
                if not Placed(The_Runner.Sortprio,Lay) and then
                   Best_Runners(i).Backprice <= Global_Max_Price and then
                   Best_Runners(i).Backprice >= Global_Min_Price and then
                   Best_Runners(i).Layprice  <= Global_Max_Price and then
                   Best_Runners(i).Layprice  >= Global_Min_Price then

                  --Place_Lay(The_Market, The_Runner, Best_Runners(i));
                  The_Bet.Place(Side       => Lay,
                                Name       => Global_Bet_Name,
                                Size       => Global_Laysize,
                                Price      => Price_Type(Best_Runners(i).Layprice),  
                                Placed     => Best_Runners(i).Pricets,                  
                                The_Runner => The_Runner,
                                The_Market => The_Market) ;
                  Bet_List.Append(The_Bet);                   
                  Placed(The_Runner.Sortprio,Lay) := True;
                end if;

                if Placed(The_Runner.Sortprio,Lay) and then
                   Best_Runners(i).Backprice <= Fixed_Type(1000.0) and then
                   Best_Runners(i).Backprice >= Fixed_Type(1.01)   and then
                   Best_Runners(i).Layprice  <= Fixed_Type(1000.0) and then
                   Best_Runners(i).Layprice  >= Fixed_Type(1.01)   and then
                   not Matched(The_Runner.Sortprio,Lay) then
  
                  Matched(The_Runner.Sortprio,Lay) := Matched_Lay(Best_Runners(i));
                end if;

                if Placed(The_Runner.Sortprio,Lay) and then
                   Matched(The_Runner.Sortprio,Lay) and then
                   Best_Runners(i).Backprice <= Fixed_Type(1000.0) and then
                   Best_Runners(i).Backprice >= Fixed_Type(1.01)   and then
                   Best_Runners(i).Layprice  <= Fixed_Type(1000.0) and then
                   Best_Runners(i).Layprice  >= Fixed_Type(1.01)   and then
                   not Placed(The_Runner.Sortprio,Back) then

                  Place_Back(The_Market, The_Runner, Best_Runners(i));
                  Bet_List.Append(The_Bet);                   
                  Placed(The_Runner.Sortprio,Back) := True;
                end if;

                if Placed(The_Runner.Sortprio,Back) and then
                   Best_Runners(i).Backprice <= Fixed_Type(1000.0) and then
                   Best_Runners(i).Backprice >= Fixed_Type(1.01)   and then
                   Best_Runners(i).Layprice  <= Fixed_Type(1000.0) and then
                   Best_Runners(i).Layprice  >= Fixed_Type(1.01)   and then
                   not Placed(The_Runner.Sortprio,Stoploss)         and then
                   not Matched(The_Runner.Sortprio,Back) then

                  Matched(The_Runner.Sortprio,Back) := Matched_Back(Best_Runners(i));
                end if;

                if Matched(The_Runner.Sortprio,Lay) and then
                   not Matched(The_Runner.Sortprio,Back) and then
                   not Placed(The_Runner.Sortprio,Stoploss) and then
                   Best_Runners(i).Backprice <= Fixed_Type(1000.0) and then
                   Best_Runners(i).Backprice >= Fixed_Type(1.01)   and then
                   Best_Runners(i).Layprice  <= Fixed_Type(1000.0) and then
                   Best_Runners(i).Layprice  >= Fixed_Type(1.01)   and then
                   Best_Runners(i).Backprice <= Global_Min_Price then
                  Place_Stoploss(The_Market, The_Runner, Best_Runners(i)); -- cancels Back too
                  Bet_List.Append(The_Bet);                   
                  
                  Placed(The_Runner.Sortprio,Stoploss) := True;
                end if;

                if Placed(The_Runner.Sortprio,Stoploss) and then
                   Best_Runners(i).Backprice <= Fixed_Type(1000.0) and then
                   Best_Runners(i).Backprice >= Fixed_Type(1.01)   and then
                   Best_Runners(i).Layprice  <= Fixed_Type(1000.0) and then
                   Best_Runners(i).Layprice  >= Fixed_Type(1.01)   and then
                   not Matched(The_Runner.Sortprio,Stoploss) then

                  Matched(The_Runner.Sortprio,Stoploss) := Matched_Stoploss(Best_Runners(i));
                end if;
              end if; --Eos
            end loop; -- BR'range
            -- exit Timestamp_Loop; -- 1 set of bets per market
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

end Check_for_Greenup_Win3;





