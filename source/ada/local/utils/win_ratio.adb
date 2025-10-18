with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Sim;
--with Utils; use Utils;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
with Table_Apriceshistory;
with Table_Abets;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Bot_System_Number;

procedure Win_Ratio is

  Bad_Bet_Side : exception;
  
  type Bet_Type is (
      Back_1_01_1_05_01_07_1_2_WIN, 
      Back_1_01_1_05_08_10_1_2_WIN, 
      Back_1_01_1_05_11_13_1_2_WIN, 
      Back_1_01_1_05_14_17_1_2_WIN, 
      Back_1_01_1_05_18_20_1_2_WIN, 
      Back_1_01_1_05_21_23_1_2_WIN, 
      Back_1_01_1_05_24_26_1_2_WIN, 
      Back_1_01_1_05_27_30_1_2_WIN, 
      Back_1_01_1_05_31_33_1_2_WIN, 
      Back_1_01_1_05_34_37_1_2_WIN, 
      Back_1_01_1_05_38_40_1_2_WIN, 

      Back_1_06_1_10_01_07_1_2_WIN, 
      Back_1_06_1_10_08_10_1_2_WIN, 
      Back_1_06_1_10_11_13_1_2_WIN, 
      Back_1_06_1_10_14_17_1_2_WIN, 
      Back_1_06_1_10_18_20_1_2_WIN, 
      Back_1_06_1_10_21_23_1_2_WIN, 
      Back_1_06_1_10_24_26_1_2_WIN, 
      Back_1_06_1_10_27_30_1_2_WIN, 
      Back_1_06_1_10_31_33_1_2_WIN, 
      Back_1_06_1_10_34_37_1_2_WIN, 
      Back_1_06_1_10_38_40_1_2_WIN, 

      Back_1_11_1_15_01_07_1_2_WIN, 
      Back_1_11_1_15_08_10_1_2_WIN, 
      Back_1_11_1_15_11_13_1_2_WIN, 
      Back_1_11_1_15_14_17_1_2_WIN, 
      Back_1_11_1_15_18_20_1_2_WIN, 
      Back_1_11_1_15_21_23_1_2_WIN, 
      Back_1_11_1_15_24_26_1_2_WIN, 
      Back_1_11_1_15_27_30_1_2_WIN, 
      Back_1_11_1_15_31_33_1_2_WIN, 
      Back_1_11_1_15_34_37_1_2_WIN, 
      Back_1_11_1_15_38_40_1_2_WIN, 

      Back_1_16_1_20_01_07_1_2_WIN, 
      Back_1_16_1_20_08_10_1_2_WIN, 
      Back_1_16_1_20_11_13_1_2_WIN, 
      Back_1_16_1_20_14_17_1_2_WIN, 
      Back_1_16_1_20_18_20_1_2_WIN, 
      Back_1_16_1_20_21_23_1_2_WIN, 
      Back_1_16_1_20_24_26_1_2_WIN, 
      Back_1_16_1_20_27_30_1_2_WIN, 
      Back_1_16_1_20_31_33_1_2_WIN, 
      Back_1_16_1_20_34_37_1_2_WIN, 
      Back_1_16_1_20_38_40_1_2_WIN, 

      Back_1_21_1_25_01_07_1_2_WIN, 
      Back_1_21_1_25_08_10_1_2_WIN, 
      Back_1_21_1_25_11_13_1_2_WIN, 
      Back_1_21_1_25_14_17_1_2_WIN, 
      Back_1_21_1_25_18_20_1_2_WIN, 
      Back_1_21_1_25_21_23_1_2_WIN, 
      Back_1_21_1_25_24_26_1_2_WIN, 
      Back_1_21_1_25_27_30_1_2_WIN, 
      Back_1_21_1_25_31_33_1_2_WIN, 
      Back_1_21_1_25_34_37_1_2_WIN, 
      Back_1_21_1_25_38_40_1_2_WIN, 

      Back_1_26_1_30_01_07_1_2_WIN, 
      Back_1_26_1_30_08_10_1_2_WIN, 
      Back_1_26_1_30_11_13_1_2_WIN, 
      Back_1_26_1_30_14_17_1_2_WIN, 
      Back_1_26_1_30_18_20_1_2_WIN, 
      Back_1_26_1_30_21_23_1_2_WIN, 
      Back_1_26_1_30_24_26_1_2_WIN, 
      Back_1_26_1_30_27_30_1_2_WIN, 
      Back_1_26_1_30_31_33_1_2_WIN, 
      Back_1_26_1_30_34_37_1_2_WIN, 
      Back_1_26_1_30_38_40_1_2_WIN, 

      Back_1_31_1_35_01_07_1_2_WIN, 
      Back_1_31_1_35_08_10_1_2_WIN, 
      Back_1_31_1_35_11_13_1_2_WIN, 
      Back_1_31_1_35_14_17_1_2_WIN, 
      Back_1_31_1_35_18_20_1_2_WIN, 
      Back_1_31_1_35_21_23_1_2_WIN, 
      Back_1_31_1_35_24_26_1_2_WIN, 
      Back_1_31_1_35_27_30_1_2_WIN, 
      Back_1_31_1_35_31_33_1_2_WIN, 
      Back_1_31_1_35_34_37_1_2_WIN, 
      Back_1_31_1_35_38_40_1_2_WIN, 

      Back_1_36_1_40_01_07_1_2_WIN, 
      Back_1_36_1_40_08_10_1_2_WIN, 
      Back_1_36_1_40_11_13_1_2_WIN, 
      Back_1_36_1_40_14_17_1_2_WIN, 
      Back_1_36_1_40_18_20_1_2_WIN, 
      Back_1_36_1_40_21_23_1_2_WIN, 
      Back_1_36_1_40_24_26_1_2_WIN, 
      Back_1_36_1_40_27_30_1_2_WIN, 
      Back_1_36_1_40_31_33_1_2_WIN, 
      Back_1_36_1_40_34_37_1_2_WIN, 
      Back_1_36_1_40_38_40_1_2_WIN, 

      Back_1_41_1_45_01_07_1_2_WIN, 
      Back_1_41_1_45_08_10_1_2_WIN, 
      Back_1_41_1_45_11_13_1_2_WIN, 
      Back_1_41_1_45_14_17_1_2_WIN, 
      Back_1_41_1_45_18_20_1_2_WIN, 
      Back_1_41_1_45_21_23_1_2_WIN, 
      Back_1_41_1_45_24_26_1_2_WIN, 
      Back_1_41_1_45_27_30_1_2_WIN, 
      Back_1_41_1_45_31_33_1_2_WIN, 
      Back_1_41_1_45_34_37_1_2_WIN, 
      Back_1_41_1_45_38_40_1_2_WIN, 

      Back_1_46_1_50_01_07_1_2_WIN, 
      Back_1_46_1_50_08_10_1_2_WIN, 
      Back_1_46_1_50_11_13_1_2_WIN, 
      Back_1_46_1_50_14_17_1_2_WIN, 
      Back_1_46_1_50_18_20_1_2_WIN, 
      Back_1_46_1_50_21_23_1_2_WIN, 
      Back_1_46_1_50_24_26_1_2_WIN, 
      Back_1_46_1_50_27_30_1_2_WIN, 
      Back_1_46_1_50_31_33_1_2_WIN, 
      Back_1_46_1_50_34_37_1_2_WIN, 
      Back_1_46_1_50_38_40_1_2_WIN);


  type Allowed_Type is record
    Bet_Name          : Bet_Name_Type := (others => ' ');
    Has_Betted        : Boolean       := False;
  end record;
      
  Global_Bets_Allowed : array (Bet_Type'range) of Allowed_Type;
      
      
  type Bet_List_Record is record
    Bet           : Table_Abets.Data_Type;
    Price_Finish  : Table_Apriceshistory.Data_Type;
    Price_Finish2  : Table_Apriceshistory.Data_Type;
  end record;

  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_List_Record);
  type Best_Runners_Array_Type is array (1..4) of Table_Apriceshistory.Data_Type ;


  Global_Bet_List : Bet_List_Pack.List;
  Global_Laybet_for_This_Market_List : Bet_List_Pack.List;

  Global_Lay_Size  : Fixed_Type := 30.0;
  Global_Back_Size : Fixed_Type := 30.0;
  
  
  Start : Calendar2.Time_Type := Calendar2.Clock;

  type Side_Type is (Lay,Back);

  Old_Market_Of_Sample,
  Current_Market_Of_Sample : Table_Apriceshistory.Data_Type;

  
  --------------------------------------------
  function "<" (Left,Right : Table_Apriceshistory.Data_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  package Backprice_Sorter is new  Table_Apriceshistory.Apriceshistory_List_Pack2.Generic_Sorting("<");
  
  
  --------------------------------------------------------------------------

  procedure Check_Bet_Matched(List     : in     Table_Apriceshistory.Apriceshistory_List_Pack2.List ;
                              Bet_List : in out Bet_List_Pack.List) is
  begin
    for R of List loop
      for B of Bet_List loop
        if B.Bet.Side = "BACK" then
          if B.Bet.Selectionid = R.Selectionid then
            if B.Bet.Status(1) = 'U' and then
              R.Pricets >  B.Bet.Betplaced + (0,0,0,1,0) and then -- 1 second later at least, time for BF delay
              R.Backprice >= B.Bet.Price and then -- Backbet so yes '<=' NOT '>='
              R.Backprice >  Fixed_Type(1.0) and then -- sanity
              R.Layprice >  Fixed_Type(1.0) then -- sanity
  
               B.Bet.Status(1) := 'M';
               B.Bet.Pricematched := R.Backprice;
               Log("Matched backbet " & B.Bet.To_String);
            end if;
          end if;      
        elsif  B.Bet.Side = "LAY " then 
          if B.Bet.Selectionid =  R.Selectionid then
            if B.Bet.Status(1) = 'U' and then
              R.Pricets >  B.Bet.Betplaced + (0,0,0,1,0) and then -- 1 second later at least, time for BF delay
              R.Layprice <= B.Bet.Price and then -- Laybet so yes '<=' NOT '>='
              R.Layprice >  Fixed_Type(1.0) and then -- sanity
              R.Backprice >  Fixed_Type(1.0) then -- sanity
  
               B.Bet.Status(1) := 'M';
               B.Bet.Pricematched := R.Layprice;
               Log("Matched Laybet " & B.Bet.To_String);
            end if;
          end if;
        
        end if;     
      end loop;
    end loop;
  end Check_Bet_Matched;
  --------------------------------------------

  --------------------------------------------------------------------------
  procedure Treat(List         : in     Table_Apriceshistory.Apriceshistory_List_Pack2.List ;
                  Bet_List     : in out Bet_List_Pack.List) is
                  
    Price_List        : Table_Apriceshistory.Apriceshistory_List_Pack2.List;
    Price             : Table_Apriceshistory.Data_Type;
    Best_Runners      : Best_Runners_Array_Type := (others => Table_Apriceshistory.Empty_Data);
    Bet               : Table_Abets.Data_Type;
    
  begin
     -- check for bet already laid for this runner on this market
     -- can only treat 1 back stratgy at a time ...
-- laybets
--    for R of List loop 
--       if not Global_Bets_Allowed(Lay_600_700).Has_Betted then
--         if R.Backprice >= Fixed_Type(600)and then
--            R.Layprice  >= Fixed_Type(50) and then
--            R.Backprice <= Fixed_Type(900) and then
--            R.Layprice  <= Fixed_Type(700) then
--
--           Bet.Marketid    := R.Marketid;
--           Bet.Selectionid := R.Selectionid;
--           Bet.Size        := Global_Lay_Size;
--           Bet.Side        := "LAY ";
--           Bet.Price       := R.Layprice;
--           Bet.Betplaced   := R.Pricets;
--           Bet.Status(1) := 'U';
--           Move("Lay_600_700",Bet.Betname);
--           Bet_List.Append(Bet_List_Record'(
--                 Bet          => Bet,
--                 Price_Finish => Best_Runners(1)));
--      --     Global_Laybet_for_This_Market_List.Append(Bet_List_Record'(
--      --           Bet          => Bet,
--      --           Price_Finish => Best_Runners(1)));
--           Global_Bets_Allowed(Lay_600_700).Has_Betted := True;
--           Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);         
--         end if;       
--       end if;    
--       
--    end loop;     
--     
     
    -- back bets
    Price_List := List.Copy;
    -- ok find the runner with lowest backprice:
    Backprice_Sorter.Sort(Price_List);
 
    Price.Backprice := 10_000.0;
    Best_Runners := (others => Price);
   --Worst_Runner.Layprice := 10_000.0;
 
    declare
      Idx : Integer := 0;
    begin
      for Tmp of Price_List loop
        if Tmp.Status(1..6) = "ACTIVE" then
          Idx := Idx +1;
          exit when Idx > Best_Runners'Last;
          Best_Runners(Idx) := Tmp;
        end if;
      end loop;
    end ;     
    
    
    for i in Bet_Type'range loop
      -- 123456789012345678901234567890    
      -- Back_1_46_1_50_11_13_1_2_WIN, 

      declare
        Min_1, Max_1 : Fixed_Type := 0.0;
        Min_2, Max_2 : Fixed_Type := 0.0;
        Betname      : String  := i'img;
      begin
        if not Global_Bets_Allowed(i).Has_Betted then
          Min_1 := Fixed_Type'Value(Betname( 6) & "." & Betname(8..9));
          Max_1 := Fixed_Type'Value(Betname(11) & "." & Betname(13..14));
          Min_2 := Fixed_Type'Value(Betname(16..17));
          Max_2 := Fixed_Type'Value(Betname(19..20));
          
          if Best_Runners(1).Backprice >= Min_1 and then
             Best_Runners(1).Backprice <= Max_1 and then
             Best_Runners(2).Backprice >= Min_2 and then
             Best_Runners(2).Backprice <= Max_2 and then
             Best_Runners(1).Layprice > Fixed_Type(1.01) and then
             Best_Runners(1).Layprice < Fixed_Type(1_000.0) then
             
             Bet.Marketid    := Best_Runners(1).Marketid;
             Bet.Selectionid := Best_Runners(1).Selectionid;
             Bet.Side        := "BACK";
             Bet.Size        := Global_Back_Size;
             Bet.Price       := Best_Runners(1).Backprice;
             Bet.Sizematched := Global_Back_Size;
             Bet.Pricematched:= Best_Runners(1).Backprice;
             Bet.Betplaced   := Best_Runners(1).Pricets;
             Bet.Status(1) := 'U';
             Move(Betname, Bet.Betname);
             Bet_List.Append(Bet_List_Record'(
                  Bet          => Bet,
                  Price_Finish => Best_Runners(1),
                  Price_Finish2 => Best_Runners(2))
             );
             Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String); 
             
             
             declare
               Place_Marketid : Market_id_Type := Sim.Win_Place_Map(Best_Runners(1).Marketid);
             begin
              -- also bet on place
               Bet.Marketid    := Place_Marketid;
               Bet.Selectionid := Best_Runners(1).Selectionid;
               Bet.Side        := "BACK";
               Bet.Size        := Global_Back_Size;
               Bet.Price       := Best_Runners(1).Backprice;
               Bet.Sizematched := Global_Back_Size;
               Bet.Pricematched:= Best_Runners(1).Backprice;
               Bet.Betplaced   := Best_Runners(1).Pricets;
               Bet.Status(1) := 'M';
               Betname(26..28) := "PLC";
               Move(Betname, Bet.Betname);
               Bet_List.Append(Bet_List_Record'(
                    Bet          => Bet,
                    Price_Finish => Best_Runners(1),
                    Price_Finish2 => Best_Runners(2))
               );             
             exception
               when others => 
                 Log("no place market for " & Best_Runners(1).Marketid); 
             end ;
             Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String); 
             Global_Bets_Allowed(i).Has_Betted := True;                
             
          end if;
          
        end if;    
      end;
    end loop;
    
  end Treat;
  --------------------------------------------------------------------------


  Enough_Runners : Boolean := False;
  use type Ada.Containers.Count_Type;
  
  Date_Start   : Calendar2.Time_Type := (2015,08,05,00,00,00,000);
  Date_Stop    : Calendar2.Time_Type := (2015,11,01,00,00,00,000);
  Current_Date : Calendar2.Time_Type := Date_Start - (1,0,0,0,0); -- 1 day

begin

    Log ("Connect db");
    Sql.Connect
      (Host     => "localhost",
       Port     => 5432,
       Db_Name  => "bnl",
       Login    => "bnl",
       Password => "bnl");
    Log ("Connected to db");

  loop
    Current_Date := Current_Date + (1,0,0,0,0);
    exit when Current_Date >= Date_Stop;
  
    Log("start process day " & Current_Date.String_Date_ISO);
    Sim.Fill_Data_Maps(Current_Date);
    Global_Bet_List.Clear; 
    declare
      Cnt : Integer := 0;
      Is_Win : Boolean := True;
    begin
      for Marketid of Sim.Market_Id_With_Data_List loop

        --Log("start process Marketid " & Marketid);
        declare
         Dummy : Market_Id_Type;
        begin
          Is_Win := True;
          Dummy := Sim.Win_Place_Map(Marketid);
        exception
          when Constraint_Error => Is_Win := False;
        end;
        if Is_Win then
          Cnt := Cnt + 1;
          -- list of timestamps in this market
          
          Global_Laybet_for_This_Market_List.Clear;
          Global_Bets_Allowed  := (others => ((others => ' '), False));
          declare
            Timestamp_To_Apriceshistory_Map : Sim.Timestamp_To_Apriceshistory_Maps.Map :=
                          Sim.Marketid_Timestamp_To_Apriceshistory_Map(Marketid);
          begin
            for Timestamp of Sim.Marketid_Pricets_Map(Marketid) loop
              declare
                List : Table_Apriceshistory.Apriceshistory_List_Pack2.List :=
                          Timestamp_To_Apriceshistory_Map(Timestamp.To_String);
              begin

                Enough_Runners := List.Length > 3;
                if Enough_Runners then

                  Current_Market_Of_Sample := List.First_Element;
                  if Current_Market_Of_Sample.Marketid /= Old_Market_Of_Sample.Marketid then
                    Log("Treat marketid '" & Current_Market_Of_Sample.Marketid & "' " &
                        "pricets " & Current_Market_Of_Sample.Pricets.To_String & " #runners" & List.Length'Img);
                    Old_Market_Of_Sample := Current_Market_Of_Sample;
                  end if;
                  
                  Treat(List => List, Bet_List => Global_Bet_List);
                  Check_Bet_Matched(List => List, Bet_List => Global_Bet_List);
                end if;
              end;
            end loop; --  Timestamp
          end;
        end if; -- Is_Win
      end loop;  -- marketid
    end;

    Log("num bets laid" & Global_Bet_List.Length'Img);

    declare
      Profit, Sum, Sum_Winners, Sum_Losers  : array (Side_Type'range) of Fixed_Type   := (others => 0.0);
      Winners, Losers, Unmatched, Strange   : array (Side_Type'range) of Integer_4 := (others => 0);
      T : Sql.Transaction_Type;
    begin
      T.Start;
      for Bet_Record of Global_Bet_List loop
        --Log("----------------");

        Bet_Record.Bet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
        if Bet_Record.Bet.Side(1..3) = "LAY" then
          case Bet_Record.Bet.Status(1) is
            when 'M'  => -- matched
                -- did it win ?
                if Sim.Is_Race_Winner(Bet_Record.Bet.Selectionid, Bet_Record.Bet.Marketid) then
                  Profit(Lay) := -(Bet_Record.Bet.Pricematched - 1.0) * Global_Lay_Size ;
                  Losers(Lay) := Losers(Lay) +1;
                  Sum_Losers(Lay) := Sum_Losers(Lay) + Profit(Lay);
                  Log("bad Laybet: " & Bet_Record.Price_Finish.To_String);
                  Bet_Record.Bet.Betwon := False;
                else-- lost
                  Profit(Lay) := Global_Lay_Size * 0.935;
                  Winners(Lay) := Winners(Lay)+1;
                  Sum_Winners(Lay) := Sum_Winners(Lay) + Profit(Lay);
                  Bet_Record.Bet.Betwon := True;
                end if;
                Bet_Record.Bet.Profit := Profit(Lay);
                Bet_Record.Bet.Insert;
            when 'U'  => -- unmatched
              Unmatched(Lay) := Unmatched(Lay) +1;
             -- Bet_Record.Bet.Insert;
            when others => --Strange
              Strange(Lay) := Strange(Lay) +1;
          end case;          
              
        elsif Bet_Record.Bet.Side(1..4) = "BACK" then
          case Bet_Record.Bet.Status(1) is
            when 'M'  => -- matched
              -- did it win ?
              if Sim.Is_Race_Winner(Bet_Record.Bet.Selectionid, Bet_Record.Bet.Marketid) then
                Profit(Back) := (Bet_Record.Bet.Pricematched - 1.0) * Global_Back_Size * 0.935;
                Winners(Back) := Winners(Back)+1;
                Sum_Winners(Back) := Sum_Winners(Back) + Profit(Back);
                Bet_Record.Bet.Betwon := True;
              else-- lost
                Profit(Back) := -Global_Back_Size ;
                Losers(Back) := Losers(Back) +1;
                Sum_Losers(Back) := Sum_Losers(Back) + Profit(Back);
                Bet_Record.Bet.Betwon := False;
                Log("bad Backbet: " & Bet_Record.Price_Finish.To_String);
              end if;
              Bet_Record.Bet.Profit := Profit(Back);              
              Bet_Record.Bet.Insert;
            when 'U'  => -- unmatched
              Unmatched(Back) := Unmatched(Back) +1;
            --  Bet_Record.Bet.Insert;
            when others => --Strange
              Strange(Back) := Strange(Back) +1;
          end case;
        else
          raise Bad_Bet_Side with "Bet_Record.Bet.Side ='" & Bet_Record.Bet.Side & "'";
        end if;

      end loop;
      T.Commit;

      for i in Side_Type'range loop
        Sum(i) := Sum_Winners(i) + Sum_Losers(i) ;
        Log("RESULT date   : " & Current_Date.String_Date_ISO & " " & i'Img );
        Log("RESULT Winners   : " & Winners(i)'Img & " " & Integer_4(Sum_Winners(i))'Img );
        Log("RESULT Losers    : " & Losers(i)'Img  & " " & Integer_4(Sum_Losers(i))'Img);
        Log("RESULT Unmatched : " & Unmatched(i)'Img  & " " & Unmatched(i)'Img);
        Log("RESULT Strange   : " & Strange(i)'Img  & " " & Strange(i)'Img);
        Log("RESULT Sum       : " & Integer_4(Sum(i))'Img );
      end loop;

      Log(" GT:" &  Integer(Sum(Back) + Sum(Lay))'Img);
    end ;
  end loop;
  Log("Started : " & Start.To_String);
  Log("Done : " & Calendar2.Clock.To_String);
  Sql.Close_Session;   


  exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Win_Ratio;
