
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
with Table_Apricesfinish;
with Table_Abets;
with Calendar2;
with Logging; use Logging;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Utils; use Utils;

with Bot_System_Number;

with Simulation_Storage;
with Ada.Exceptions;
with Ada.Command_Line;

procedure Lay_Greenup_Filter is

  Global_Marketid_Map  : Simulation_Storage.Marketid_Map_Pack.Map;
  Global_Winner_Map    : Simulation_Storage.Winner_Map_Pack.Map;
  Global_Win_Place_Map : Simulation_Storage.Win_Place_Map_Pack.Map;

  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Table_Abets.Data_Type, Table_Abets."=");

  Global_Bet_List : Bet_List_Pack.List;


  --package Filter_Sample_Map_Pack is new Ada.Containers.Ordered_Maps
  --      (Key_Type     => Integer_4,
  --       Element_Type => Table_Apricesfinish.Apricesfinish_List_Pack2.List,
  --       "<"          => "<",
  --       "="          =>  Table_Apricesfinish.Apricesfinish_List_Pack2."=");
  --
  --Global_Filter_Sample_Map : Filter_Sample_Map_Pack.Map;


  T : Sql.Transaction_Type;

  use type Ada.Containers.Count_Type;
  
  Global_Back_Size : Fixed_Type := 30.0;
  Empty_Market     : constant Market_Id_Type := (others => ' ');

  type Best_Runners_Type is array (1..4) of Table_Apricesfinish.Data_Type ;

  Global_Strategy_List : Simulation_Storage.Strategy_List_Pack.List;

  Min_Num_Samples : constant Ada.Containers.Count_Type := 500;

  subtype Num_Runners_Type is Integer range 1..36;
  type Fifo_Type is record
    Selectionid    : Integer_4 := 0;
    One_Runner_Sample_List : Table_Apricesfinish.Apricesfinish_List_Pack2.List;
    Avg_Lay_Price  : Fixed_Type := 0.0;   
    Avg_Back_Price : Fixed_Type := 0.0;   
    In_Use         : Boolean := False;
    Index          : Num_Runners_Type := Num_Runners_Type'first;
  end record;
  
  Fifo : array (Num_Runners_Type'range) of Fifo_Type;
  
  --------------------------------------------------------------------------

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

  procedure Treat(Best_Runners  : in out Best_Runners_Type;
                  Strategy_List : in out Simulation_Storage.Strategy_List_Pack.List;
                  Bet_List      : in out Bet_List_Pack.List) is
    use Calendar2;
    Runner_Index, High_Index : Integer := 0;
    Bet : Table_Abets.Data_Type;
  begin
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
      else  -- strategy fullfilled, see what odds we get, if matched
      --  if Best_Runners(1).Pricets >  Bet.Betplaced + (0,0,0,1,0) and then -- 1 second later at least, time for BF delay
        if Best_Runners(1).Pricets >=  Bet.Betplaced + (0,0,0,0,0) and then -- 0 second later at least, time for BF delay
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


  procedure Filter_List(Sample_List , Avg_Sample_List: in out Table_Apricesfinish.Apricesfinish_List_Pack2.List)  is
  begin
  --  Log ("Filter_List : start Sample_List.Length" & Sample_List.Length'Img & " Global_Sample_List.Length" & Global_Sample_List.Length'Img);
   -- Fifo : array (Num_Runners_Type'range) of Fifo_Type;
       
       
    for s of Sample_List loop
    -- find my index in the array
      Num_Run: for i in Num_Runners_Type'range loop
    
        if S.Selectionid = Fifo(i).Selectionid then      
          -- insert elements at bottom and remove from top
          -- check if list needs trim
          loop
            exit when Fifo(i).One_Runner_Sample_List.Length <= Min_Num_Samples;
            Fifo(i).One_Runner_Sample_List.Delete_First;
          end loop;  
          
          -- append the new value
          Fifo(i).One_Runner_Sample_List.Append(s);

          if Fifo(i).One_Runner_Sample_List.Length >= Min_Num_Samples then
          
          -- recalculate the avg values
            declare
              Backprice,Layprice : Fixed_Type := 0.0;
              Sample : Table_Apricesfinish.Data_Type;
              Cnt : Natural := 0;
            begin
              for s2 of Fifo(i).One_Runner_Sample_List loop
                Backprice := Backprice + S2.Backprice;
                Layprice := Layprice + S2.Layprice;
                Sample := S2; -- save some data
                Cnt := Cnt +1 ;
              --  Log ("Filter_List Cnt : " & Cnt'Img & Sample.To_String );
              end loop;
              Sample.Backprice := Backprice / Fixed_Type(Fifo(i).One_Runner_Sample_List.Length);
              Sample.Layprice := Layprice / Fixed_Type(Fifo(i).One_Runner_Sample_List.Length);
              Avg_Sample_List.Append(Sample);
           --   Log ("Filter_List : avg " & Sample.To_String );
            end;
          end if;
           
          exit Num_Run;  
        end if;
      end loop Num_Run;
      
    end loop;

    --Log ("Filter_List : 7" );

  end Filter_List;

  -----------------------------------------------------


begin

  Log ("Connect db");
  Sql.Connect
    (Host     => "localhost",
     Port     => 5432,
     Db_Name  => "dry",
     Login    => "bnl",
     Password => "bnl");
  Log ("Connected to db");

  Log ("Loading Strategies");
  Simulation_Storage.Load_Strategies(Global_Strategy_List);

  Log ("Load mapfiles");
  Simulation_Storage.Fill_Maps(Marketid_Map  => Global_Marketid_Map,
                               Winner_Map    => Global_Winner_Map,
                               Win_Place_Map => Global_Win_Place_Map);


  Log("start process");
  declare
    use Simulation_Storage;
    Cur,Cnt      : Integer := 0;
    Market_Id_C  : Marketid_Map_Pack.Cursor := Global_Marketid_Map.First;
    Marketid     : Market_Id_Type := (others => ' ');
    Sample_Id_C  : Sample_Map_Pack.Cursor;
    --Pricets      : Calendar2.Time_Type := Calendar2.Time_Type_First;
    A_Sample_Map : Sample_Map_Pack.Map;
    First_Time : Boolean := True;
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
      
      --reset the fifo for new race      
      for i in Num_Runners_Type loop
        Fifo(i).Selectionid    := 0;
        Fifo(i).Avg_Lay_Price  := 0.0;   
        Fifo(i).Avg_Back_Price := 0.0;   
        Fifo(i).In_Use         := False;
        Fifo(i).Index          := Num_Runners_Type'first;
        Fifo(i).One_Runner_Sample_List.Clear;
      end loop; 

      First_Time := True;
      Marketid := Marketid_Map_Pack.Key(Market_Id_C);
      Cur := Cur +1;
      Log("Marketid " & Marketid & " " & Utils.F8_Image( Fixed_Type( 100 * Cur) / Fixed_Type(Cnt)) & " %");

      A_Sample_Map := Marketid_Map_Pack.Element(Market_Id_C);
      Sample_Id_C := A_Sample_Map.First;

      while Sample_Map_Pack.Has_Element(Sample_Id_C) loop
        --Pricets := Sample_Map_Pack.Key(Sample_Id_C);
        --Log("pricts " & Pricets.To_String);
        declare
          Sample_List          : Table_Apricesfinish.Apricesfinish_List_Pack2.List := Sample_Map_Pack.Element(Sample_Id_C);
          Filtered_Sample_List : Table_Apricesfinish.Apricesfinish_List_Pack2.List;
          Best_Runners         : Best_Runners_Type := (others => Table_Apricesfinish.Empty_Data);
        begin
          if First_Time then
            declare
              i : Integer := 0;
            begin  
               -- get a free slot in the array
              for s of Sample_List loop
                i := i +1;
                Fifo(i).In_Use := True;
                Fifo(i).Selectionid := S.Selectionid;           
                Fifo(i).Index := i;                
              end loop;
            end;
            First_Time := False;
          end if;
          
          Filter_List(Sample_List, Filtered_Sample_List); 
          Sort_Best_Runners(Best_Runners, Filtered_Sample_List);
          if Best_Runners(4).Backprice < 10_000.0 then
            Treat(Best_Runners, Global_Strategy_List, Global_Bet_List );
          end if;
        end;
        Sample_Map_Pack.Next(Sample_Id_C);
      end loop;

      Marketid_Map_Pack.Next(Market_Id_C);
      --return;
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
    if Strategy.Profit      > Fixed_Type(0.0) then
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
end Lay_Greenup_Filter;
