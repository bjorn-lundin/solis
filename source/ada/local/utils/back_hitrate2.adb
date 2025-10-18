
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
with Price_Histories;
with Bets;
--with Gnat.Command_Line; use Gnat.Command_Line;
---with GNAT.Strings;
with Calendar2;
with Logging; use Logging;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Utils; use Utils;

with Bot_System_Number;

with Sim;
with Ada.Exceptions;
with  Ada.Command_Line;

procedure Back_Hitrate2 is

  Global_Bet_List : Bets.Lists.List;

  T            : Sql.Transaction_Type;

--  Config           : Command_Line_Configuration;

--  IA_Max_Start_Price : aliased Integer := 30;
--  IA_Lay_At_Price    : aliased Integer := 100;
--  IA_Max_Lay_Price   : aliased Integer := 200;

  Global_Back_Size : Fixed_Type := 30.0;
  Empty_Market     : constant Marketid_Type := (others => ' ');

  type Best_Runners_Type is array (1..4) of Price_Histories.Price_History_Type ;

  Global_Strategy_List : Simulation_Storage.Strategy_List_Pack.List;
  --------------------------------------------------------------------------

  type Place_Of_Next_Type is new Integer range 2 .. 4 ;
  type Place_Of_Runner_Type is new Integer range 1 .. 3 ;
  type Strategy_Type is record
    Betname           : Repository_Types.String_Object;
    Marketid          : Market_Id_Type := (others => ' ');
    Leader_At_Max     : Fixed_Type := 0.0;
    Next_At_Min       : Fixed_Type := 0.0;
    Place_Of_Next     : Place_Of_Next_Type:= Place_Of_Next_Type'First;
    Place_Of_Runner   : Place_Of_Runner_Type:= Place_Of_Runner_Type'First;
    Ts_Of_Fulfill     : Calendar2.Time_Type := Calendar2.Time_Type_First;
    Backprice_Matched : Fixed_Type := 0.0;
    Profit            : Fixed_Type := 0.0;
    Num_Matched       : Integer_4 := 0;
    Num_Wins          : Integer_4 := 0;
    Num_Lost          : Integer_4 := 0;
  end record;

  package Strategy_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Strategy_Type, "=");





  function "<" (Left,Right : Table_Apricesfinish.Data_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";

  package Backprice_Sorter is new Table_Apricesfinish.Apricesfinish_List_Pack2.Generic_Sorting("<");

  procedure Sort_Best_Runners(Best : in out Best_Runners_Type;  Sample_List : in out Table_Apricesfinish.Apricesfinish_List_Pack2.List) is
    Price : Table_Apricesfinish.Data_Type;
  begin
    -- ok find the runner with lowest backprice:
    Backprice_Sorter.Sort(Sample_List);
    Price.Backprice := 10_000.0;
    Price.Layprice := 10_000.0;
    Best := (others => Price);

    declare
      Idx : Integer := 0;
    begin
      for Tmp of Sample_List loop
        if Tmp.Status(1..6) = "ACTIVE" and then
           Tmp.Backprice > Fixed_Type(0.0) and then
           Tmp.Layprice  > Fixed_Type(0.0) and then
           Tmp.Backprice < Fixed_Type(1_000.0) and then
           Tmp.Layprice  < Fixed_Type(1_000.0)
        then
          Idx := Idx +1;
          exit when Idx > Best'Last;
          Best(Idx) := Tmp;
        end if;
      end loop;
    end ;
  end Sort_Best_Runners;
  --------------------------------------------------------------------

  procedure Treat(Strategy_List : in out Simulation_Storage.Strategy_List_Pack.List;
                  Bet_List      : in out Bet_List_Pack.List) is
    use Calendar2;
    Runner_Index, High_Index : Integer := 0;
    Bet                      : Bets.Bet_Type;
    Best_Runners             : Best_Runners_Type;
  begin




    -- reset strategies wrt previous bets on other markets
    for Strategy of Global_Strategy_List loop
      Strategy.Marketid := Empty_Market;
      Strategy.Ts_Of_Fulfill := Calendar2.Time_Type_First;
    end loop;

    for Strategy of Strategy_List loop
      if Strategy.Marketid = Empty_Market then
        --check the strategy againt the Best_Runners
        High_Index := Integer(Strategy.Place_Of_Next);
        if Best_Runners(1).Backprice <= Strategy.Leader_At_Max and then
            Best_Runners(High_Index).Backprice >= Strategy.Next_At_Min
        then
           Strategy.Marketid := Best_Runners(1).Marketid;     -- mark strategy as fulfilled, when and with what marketid
           Strategy.Ts_Of_Fulfill := Best_Runners(1).Pricets;
           --for i in Best_Runners'range loop
           --  Log(Strategy.Betname.Fix_String & " taken with Best_Runners(i)" & i'Img & " " & Best_Runners(i).To_String);
           --end loop;
        end if;
      else  -- strategy fullfilled, se what odds we get, if matched
        if Best_Runners(1).Pricets >  Bet.Betplaced + (0,0,0,1,0) and then -- 1 second later at least, time for BF delay
           Strategy.Ts_Of_Fulfill /= Calendar2.Time_Type_First then
          Runner_Index := Integer(Strategy.Place_Of_Runner);
          -- Strategy.Backprice_Matched := get_price of runner from PLACE market ...
          pragma Compile_Time_Warning(True, "get_price of runner from PLACE market");
          Strategy.Backprice_Matched := 1.01;
          Best_Runners(Runner_Index).Backprice := 1.01;

          Strategy.Ts_Of_Fulfill := Calendar2.Time_Type_First; -- so we do not bet again with this strategy on this market
          Strategy.Num_Matched := Strategy.Num_Matched +1;

          Move(Strategy.Betname.Upper_Case, Bet.Betname);
          Bet.Marketid     := Global_Win_Place_Map(Best_Runners(Runner_Index).Marketid);
          Bet.Selectionid  := Best_Runners(Runner_Index).Selectionid;
          Bet.Size         := Global_Back_Size;
          Bet.Price        := Best_Runners(Runner_Index).Backprice;
          Bet.Sizematched  := Global_Back_Size;
          Bet.Pricematched := Best_Runners(Runner_Index).Backprice;
          Bet.Betplaced    := Best_Runners(Runner_Index).Pricets;
          Bet.Startts      := Best_Runners(Runner_Index).Pricets;  -- correct date anyway
          Bet_List.Append(Bet);
        end if;
      end if;
    end loop;
  end Treat;
  --------------------------------------------------------------------------
  procedure Load_Strategies(Strategy_List : out Strategy_List_Pack.List) is
  begin
    Strategy_List.Append(Strategy_Type'(Betname         => Repository_Types.Create("SIM_PLC_1.10_7.0_1"),
                                        Marketid        => (others => ' '),
                                        Leader_At_Max   => 1.10,
                                        Next_At_Min     => 7.0,
                                        Place_Of_Next   => 2,
                                        Place_Of_Runner => 1,
                                        Backprice_Matched => 0.0,
                                        Profit            => 0.0,
                                        Profit_102        => 0.0,
                                        Profit_103        => 0.0,
                                        Profit_104        => 0.0,
                                        Num_Matched       => 0,
                                        Num_Lost          => 0,
                                        Num_Wins          => 0,
                                        Ts_Of_Fulfill   => Calendar2.Time_Type_First)
                               );
    --Strategy_List.Append(Strategy_Type'(Betname         => Repository_Types.Create("SIM_PLC_1.25_12.0_1"),
    --                                    Marketid        => (others => ' '),
    --                                    Leader_At_Max   => 1.25,
    --                                    Next_At_Min     => 12.0,
    --                                    Place_Of_Next   => 2,
    --                                    Place_Of_Runner => 1,
    --                                    Backprice_Matched => 0.0,
    --                                    Profit            => 0.0,
    --                                    Profit_102        => 0.0,
    --                                    Profit_103        => 0.0,
    --                                    Profit_104        => 0.0,
    --                                    Num_Lost          => 0,
    --                                    Num_Wins          => 0,
    --                                    Num_Matched       => 0,
    --                                    Ts_Of_Fulfill   => Calendar2.Time_Type_First)
    --                           );
    --Strategy_List.Append(Strategy_Type'(Betname         => Repository_Types.Create("SIM_PLC_1.10_7.0_2"),
    --                                    Marketid        => (others => ' '),
    --                                    Leader_At_Max   => 1.10,
    --                                    Next_At_Min     => 7.0,
    --                                    Place_Of_Next   => 2,
    --                                    Place_Of_Runner => 2,
    --                                    Backprice_Matched => 0.0,
    --                                    Profit            => 0.0,
    --                                    Profit_102        => 0.0,
    --                                    Profit_103        => 0.0,
    --                                    Profit_104        => 0.0,
    --                                    Num_Matched       => 0,
    --                                    Num_Lost          => 0,
    --                                    Num_Wins          => 0,
    --                                    Ts_Of_Fulfill   => Calendar2.Time_Type_First)
    --                           );
    --Strategy_List.Append(Strategy_Type'(Betname         => Repository_Types.Create("SIM_PLC_1.25_12.0_2"),
    --                                    Marketid        => (others => ' '),
    --                                    Leader_At_Max   => 1.25,
    --                                    Next_At_Min     => 12.0,
    --                                    Place_Of_Next   => 2,
    --                                    Place_Of_Runner => 2,
    --                                    Backprice_Matched => 0.0,
    --                                    Profit            => 0.0,
    --                                    Profit_102        => 0.0,
    --                                    Profit_103        => 0.0,
    --                                    Profit_104        => 0.0,
    --                                    Num_Lost          => 0,
    --                                    Num_Wins          => 0,
    --                                    Num_Matched       => 0,
    --                                    Ts_Of_Fulfill   => Calendar2.Time_Type_First)
    --                           );
    --Strategy_List.Append(Strategy_Type'(Betname         => Repository_Types.Create("SIM_PLC_1.10_7.0_3"),
    --                                    Marketid        => (others => ' '),
    --                                    Leader_At_Max   => 1.10,
    --                                    Next_At_Min     => 7.0,
    --                                    Place_Of_Next   => 2,
    --                                    Place_Of_Runner => 3,
    --                                    Backprice_Matched => 0.0,
    --                                    Profit            => 0.0,
    --                                    Profit_102        => 0.0,
    --                                    Profit_103        => 0.0,
    --                                    Profit_104        => 0.0,
    --                                    Num_Matched       => 0,
    --                                    Num_Lost          => 0,
    --                                    Num_Wins          => 0,
    --                                    Ts_Of_Fulfill   => Calendar2.Time_Type_First)
    --                           );
    --Strategy_List.Append(Strategy_Type'(Betname         => Repository_Types.Create("SIM_PLC_1.25_12.0_3"),
    --                                    Marketid        => (others => ' '),
    --                                    Leader_At_Max   => 1.25,
    --                                    Next_At_Min     => 12.0,
    --                                    Place_Of_Next   => 2,
    --                                    Place_Of_Runner => 3,
    --                                    Backprice_Matched => 0.0,
    --                                    Profit            => 0.0,
    --                                    Profit_102        => 0.0,
    --                                    Profit_103        => 0.0,
    --                                    Profit_104        => 0.0,
    --                                    Num_Lost          => 0,
    --                                    Num_Wins          => 0,
    --                                    Num_Matched       => 0,
    --                                    Ts_Of_Fulfill   => Calendar2.Time_Type_First)
    --                           );
    --declare            --1234567890123456789
    --  Templ : String := "SIM_PLC_1.90_60.0_1";
    --begin
    --  for Leader_Int in 1 ..2 loop
    --     for Leader_First_Fraction in 0 .. 9 loop
    --        for Next_At_10 in 2 .. 9 loop
    --          for Runner_Postion in 1 .. 3 loop
    --            Templ(9) := Leader_Int'Img(2);
    --            Templ(11) := Leader_First_Fraction'Img(2);
    --            Templ(14) := Next_At_10'Img(2);
    --            Templ(19) := Runner_Postion'Img(2);
    --            Strategy_List.Append(
    --                  Strategy_Type'(
    --                       Betname         => Repository_Types.Create(Templ),
    --                       Marketid        => (others => ' '),
    --                       Leader_At_Max   => Fixed_Type'Value(Templ(9..12)),
    --                       Next_At_Min     => Fixed_Type'Value(Templ(14..17)),
    --                       Place_Of_Next   => 4,
    --                       Place_Of_Runner => Place_Of_Runner_Type'Value(Templ(19..19)),
    --                       Backprice_Matched => 0.0,
    --                       Profit            => 0.0,
    --                       Profit_102        => 0.0,
    --                       Profit_103        => 0.0,
    --                       Profit_104        => 0.0,
    --                       Num_Matched       => 0,
    --                       Num_Lost          => 0,
    --                       Num_Wins          => 0,
    --                       Ts_Of_Fulfill   => Calendar2.Time_Type_First
    --                  )
    --            );
    --
    --          end loop;
    --        end loop;
    --     end loop;
    --  end loop;
    --end;

  end Load_Strategies;
  -------------------------------------------------------------


  Current_Date : Calendar2.Time_Type := (2016,02,02,1,1,1,1);

  ------------------------------------------------------



begin
--  Define_Switch
--    (Config      => Config,
--     Output      => IA_Max_Start_Price'access,
--     Long_Switch => "--max_start_price=",
--     Help        => "starting price (back)(");
--
--  Define_Switch
--    (Config      => Config,
--     Output      => Ia_Lay_At_Price'access,
--     Long_Switch => "--lay_at_price=",
--     Help        => "Lay the runner at this price(Back)");
--
--  Define_Switch
--    (Config      => Config,
--     Output      => IA_Max_Lay_Price'access,
--     Long_Switch => "--max_lay_price=",
--     Help        => "Runner cannot have higer price that this when layed (Lay)");
--
--  Getopt (Config);  -- process the command line

--     if Ia_Best_Position = 0 or else
--       Ia_Max_Odds = 0 then
--       Display_Help (Config);
--       return;
--     end if;

  Log ("Connect db");
  Sql.Connect
    (Host     => "localhost",
     Port     => 5432,
     Db_Name  => "bnl",
     Login    => "bnl",
     Password => "bnl");
  Log ("Connected to db");

  Log ("Loading Strategies");
  Load_Strategies(Global_Strategy_List);


  loop
    Log ("Load mapfiles " & Current_Date.String_Date_ISO);
    Sim.Fill_Data_Maps(Date => Current_Date, Animal => Horse);
    Treat(Global_Strategy_List, Global_Bet_List );
    exit when Sim.Market_With_Data_List.Length = 0;
    Current_Date := Current_Date + (1,0,0,0,0); -- one day
  end loop;








  Log("start process");
  declare
    use Simulation_Storage;
    Cur,Cnt      : Integer := 0;
    Market_Id_C  : Marketid_Map_Pack.Cursor := Global_Marketid_Map.First;
    Marketid     : Market_Id_Type := (others => ' ');
    Sample_Id_C  : Sample_Map_Pack.Cursor;
   -- Pricets      : Calendar2.Time_Type := Calendar2.Time_Type_First;
    A_Sample_Map : Sample_Map_Pack.Map;
  begin
    Cnt := Integer(Global_Marketid_Map.Length);
    if Cnt = 0 then
      Cnt := 1; -- no division by 0
    end if;

    Markets_Loop : while Marketid_Map_Pack.Has_Element(Market_Id_C) loop
      -- reset strategies wrp prevouis bets on other markets
      for Strategy of Global_Strategy_List loop
        Strategy.Marketid := Empty_Market;
        Strategy.Ts_Of_Fulfill := Calendar2.Time_Type_First;
      end loop;

      Marketid := Marketid_Map_Pack.Key(Market_Id_C);
      Cur := Cur +1;
      Log("Marketid " & Marketid & " " & Utils.F8_Image( Fixed_Type( 100 * Cur) / Fixed_Type(Cnt)) & " %");

      A_Sample_Map := Marketid_Map_Pack.Element(Market_Id_C);
      Sample_Id_C := A_Sample_Map.First;

      while Sample_Map_Pack.Has_Element(Sample_Id_C) loop
       -- Pricets := Sample_Map_Pack.Key(Sample_Id_C);
        -- Log("pricts " & Pricets.To_String);
        declare
          Sample_List  : Table_Apricesfinish.Apricesfinish_List_Pack2.List := Sample_Map_Pack.Element(Sample_Id_C);
          Best_Runners : Best_Runners_Type := (others => Table_Apricesfinish.Empty_Data);
        begin
          Sort_Best_Runners(Best_Runners, Sample_List);
          if Best_Runners(4).Backprice < 10_000.0 then
            Treat(Best_Runners, Global_Strategy_List, Global_Bet_List );
          end if;
        end;
        Sample_Map_Pack.Next(Sample_Id_C);
      end loop;

      Marketid_Map_Pack.Next(Market_Id_C);
    end loop Markets_Loop;
  end;

  Log("num matched bets" & Global_Bet_List.Length'Img);
  declare
    Sum, Sum_Winners, Sum_Losers : Fixed_Type := 0.0;
    Profit,Profit_102,Profit_103,Profit_104 : Fixed_Type := 0.0;
    Winners,Losers : Integer_4 := 0;
    Betwon : Boolean := False;
    Cur,Cnt      : Integer := 0;
  begin
    T.Start;
    Cnt := Integer(Global_Bet_List.Length);
    if Cnt = 0 then
      Cnt := 1; -- no division by 0
    end if;
    for Bet of Global_Bet_List loop
      Cur := Cur +1;
      Log("Correcting bets " & Utils.Trim(Bet.Betname) & " " & Bet.Marketid & " " & Utils.F8_Image( Fixed_Type( 100 * Cur) / Fixed_Type(Cnt)) & " %");

      Betwon := False;
      for Winner of Global_Winner_Map(Bet.Marketid) loop
        if Bet.Selectionid = Winner.Selectionid then
          Betwon := True;
          exit;
        end if;
      end loop;
      Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
      Bet.Betwon := Betwon;
      -- did it win ?
      if Betwon then
        -- won
        Profit := (Bet.Price -1.0) * Global_Back_Size * 0.935;
        Profit_102 := (1.02 -1.0) * Global_Back_Size * 0.935;
        Profit_103 := (1.03 -1.0) * Global_Back_Size * 0.935;
        Profit_104 := (1.04 -1.0) * Global_Back_Size * 0.935;
        Winners := Winners +1;
        Sum_Winners := Sum_Winners + Profit;
      else
        -- lost
        Profit := -Global_Back_Size;
        Losers := Losers +1;
        Sum_Losers := Sum_Losers + Profit;
      end if;

      Bet.Profit := Profit;
      Bet.Insert;

      for Strategy of Global_Strategy_List loop
        if Utils.Trim(Bet.Betname) = Strategy.Betname.Fix_String then
          Strategy.Profit := Strategy.Profit + Profit;
          Strategy.Profit_102 := Strategy.Profit_102 + Profit_102;
          Strategy.Profit_103 := Strategy.Profit_103 + Profit_103;
          Strategy.Profit_104 := Strategy.Profit_104 + Profit_104;

          if Betwon then
            Strategy.Num_Wins := Strategy.Num_Wins + 1;
          else
            Strategy.Num_Lost := Strategy.Num_Lost + 1;
          end if;
          exit;
        end if;
      end loop;

    end loop;
    T.Commit;
    Sum := Sum_Winners + Sum_Losers ;
    Log("Winners :" & Winners'Img & Integer_4(Sum_Winners)'Img );
    Log("Losers  :" & Losers'Img  & Integer_4(Sum_Losers)'Img);
    Log("Sum     :" & Integer_4(Sum)'Img  );
  end ;
  -- no need for db anymore
  Sql.Close_Session;


  for Strategy of Global_Strategy_List loop
    if Strategy.Profit      > Fixed_Type(0.0) or else
       Strategy.Profit_102  > Fixed_Type(0.0) or else
       Strategy.Profit_103  > Fixed_Type(0.0) or else
       Strategy.Profit_104  > Fixed_Type(0.0) then
        Log( Strategy.Betname.Fix_String & " " &
                 F8_Image(Strategy.Profit) & " " &
                 F8_Image(Strategy.Profit_102) & " " &
                 F8_Image(Strategy.Profit_103) & " " &
                 F8_Image(Strategy.Profit_104) & " " &
                 Strategy.Num_Matched'Img &
                 Strategy.Num_Wins'Img &
                 Strategy.Num_Lost'Img & " " &
                 Utils.F8_Image( Fixed_Type( 100 * Strategy.Num_Wins) / Fixed_Type(Strategy.Num_Matched)) & " %"
                 );
    end if;
  end loop;

--  Log("used --max_start_price=" & IA_Max_Start_Price'Img &
--    " --lay_at_price=" & IA_Lay_At_Price'Img &
--    " --max_lay_price=" & IA_Max_Lay_Price'Img);
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
end Back_Hitrate2;
