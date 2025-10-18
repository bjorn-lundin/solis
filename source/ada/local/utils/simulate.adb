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

procedure Simulate is

  Bad_Bet_Side : exception;
  
  type Bet_Type is (
      Back_1_10_30_1_2_WIN, 
      Back_1_10_26_1_2_WIN, 
      Back_1_10_23_1_2_WIN, 
      Back_1_10_20_1_2_WIN, 
      Back_1_10_16_1_2_WIN, 
      Back_1_10_13_1_2_WIN, 
      Back_1_10_10_1_2_WIN, 
      Back_1_10_07_1_2_WIN, 
      Lay_160_200, 
      Lay_200_300, --,
      Lay_300_400, --,
      Lay_200_400, --,
      Lay_400_500, --,
      Lay_500_600, --,
      Lay_600_700 
    --  Lay_05_And_Below,
    --  Lay_10_And_Below,
    --  Lay_15_And_Below,
    --  Lay_20_And_Below,
    --  Lay_25_And_Below,
    --  Lay_30_And_Below,
    --  Lay_35_And_Below,
    --  Lay_40_And_Below
      );


  type Allowed_Type is record
    Bet_Name          : Bet_Name_Type := (others => ' ');
    Has_Betted        : Boolean       := False;
  end record;
      
  Global_Bets_Allowed : array (Bet_Type'range) of Allowed_Type;
      
      
  type Bet_List_Record is record
    Bet           : Table_Abets.Data_Type;
    Price_Finish  : Table_Apriceshistory.Data_Type;
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
              R.Backprice >  Fixed_Type(1.0) then -- sanity
  
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

    for R of List loop 
       -- laybets
     --  Global_Bets_Allowed(Lay_160_200).Has_Betted := False; 
     --  for b of Global_Laybet_for_This_Market_List loop
     --    if B.Bet.Selectionid = R.Selectionid and then
     --       B.Bet.Betname(1..11) = "Lay_160_200" then
     --         Global_Bets_Allowed(Lay_160_200).Has_Betted := True; 
     --    end if;
     --  end loop;
       
      
       
       if not Global_Bets_Allowed(Lay_160_200).Has_Betted then
         if R.Backprice >= Fixed_Type(160)and then
            R.Layprice  >= Fixed_Type(50) and then
            R.Backprice <= Fixed_Type(800) and then
            R.Layprice  <= Fixed_Type(200) then

           Bet.Marketid    := R.Marketid;
           Bet.Selectionid := R.Selectionid;
           Bet.Size        := Global_Lay_Size;
           Bet.Side        := "LAY ";
           Bet.Price       := R.Layprice;
           Bet.Betplaced   := R.Pricets;
           Bet.Status(1) := 'U';
           Move("Lay_160_200",Bet.Betname);
           Bet_List.Append(Bet_List_Record'(
                 Bet          => Bet,
                 Price_Finish => Best_Runners(1)));
                 
        --   Global_Laybet_for_This_Market_List.Append(Bet_List_Record'(
        --         Bet          => Bet,
        --         Price_Finish => Best_Runners(1)));
                 
                 
           Global_Bets_Allowed(Lay_160_200).Has_Betted := True;
           Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);         
         end if;       
       end if;     
       
     --  Global_Bets_Allowed(Lay_200_400).Has_Betted := False; 
     --  for b of Global_Laybet_for_This_Market_List loop
     --    if B.Bet.Selectionid = R.Selectionid and then
     --       B.Bet.Betname(1..11) = "Lay_200_400" then
     --         Global_Bets_Allowed(Lay_200_400).Has_Betted := True; 
     --    end if;
     --  end loop;
       if not Global_Bets_Allowed(Lay_200_300).Has_Betted then
         if R.Backprice >= Fixed_Type(200)and then
            R.Layprice  >= Fixed_Type(50) and then
            R.Backprice <= Fixed_Type(800) and then
            R.Layprice  <= Fixed_Type(300) then

           Bet.Marketid    := R.Marketid;
           Bet.Selectionid := R.Selectionid;
           Bet.Size        := Global_Lay_Size;
           Bet.Side        := "LAY ";
           Bet.Price       := R.Layprice;
           Bet.Betplaced   := R.Pricets;
           Bet.Status(1) := 'U';
           Move("Lay_200_300",Bet.Betname);
           Bet_List.Append(Bet_List_Record'(
                 Bet          => Bet,
                 Price_Finish => Best_Runners(1)));
      --     Global_Laybet_for_This_Market_List.Append(Bet_List_Record'(
      --           Bet          => Bet,
      --           Price_Finish => Best_Runners(1)));
           Global_Bets_Allowed(Lay_200_300).Has_Betted := True;
           Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);         
         end if;       
       end if;    
       if not Global_Bets_Allowed(Lay_300_400).Has_Betted then
         if R.Backprice >= Fixed_Type(300)and then
            R.Layprice  >= Fixed_Type(50) and then
            R.Backprice <= Fixed_Type(800) and then
            R.Layprice  <= Fixed_Type(400) then

           Bet.Marketid    := R.Marketid;
           Bet.Selectionid := R.Selectionid;
           Bet.Size        := Global_Lay_Size;
           Bet.Side        := "LAY ";
           Bet.Price       := R.Layprice;
           Bet.Betplaced   := R.Pricets;
           Bet.Status(1) := 'U';
           Move("Lay_300_400",Bet.Betname);
           Bet_List.Append(Bet_List_Record'(
                 Bet          => Bet,
                 Price_Finish => Best_Runners(1)));
      --     Global_Laybet_for_This_Market_List.Append(Bet_List_Record'(
      --           Bet          => Bet,
      --           Price_Finish => Best_Runners(1)));
           Global_Bets_Allowed(Lay_300_400).Has_Betted := True;
           Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);         
         end if;       
       end if;           
              
       if not Global_Bets_Allowed(Lay_200_400).Has_Betted then
         if R.Backprice >= Fixed_Type(200)and then
            R.Layprice  >= Fixed_Type(50) and then
            R.Backprice <= Fixed_Type(800) and then
            R.Layprice  <= Fixed_Type(400) then

           Bet.Marketid    := R.Marketid;
           Bet.Selectionid := R.Selectionid;
           Bet.Size        := Global_Lay_Size;
           Bet.Side        := "LAY ";
           Bet.Price       := R.Layprice;
           Bet.Betplaced   := R.Pricets;
           Bet.Status(1) := 'U';
           Move("Lay_200_400",Bet.Betname);
           Bet_List.Append(Bet_List_Record'(
                 Bet          => Bet,
                 Price_Finish => Best_Runners(1)));
      --     Global_Laybet_for_This_Market_List.Append(Bet_List_Record'(
      --           Bet          => Bet,
      --           Price_Finish => Best_Runners(1)));
           Global_Bets_Allowed(Lay_200_400).Has_Betted := True;
           Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);         
         end if;       
       end if;    

       if not Global_Bets_Allowed(Lay_400_500).Has_Betted then
         if R.Backprice >= Fixed_Type(400)and then
            R.Layprice  >= Fixed_Type(50) and then
            R.Backprice <= Fixed_Type(900) and then
            R.Layprice  <= Fixed_Type(500) then

           Bet.Marketid    := R.Marketid;
           Bet.Selectionid := R.Selectionid;
           Bet.Size        := Global_Lay_Size;
           Bet.Side        := "LAY ";
           Bet.Price       := R.Layprice;
           Bet.Betplaced   := R.Pricets;
           Bet.Status(1) := 'U';
           Move("Lay_400_500",Bet.Betname);
           Bet_List.Append(Bet_List_Record'(
                 Bet          => Bet,
                 Price_Finish => Best_Runners(1)));
      --     Global_Laybet_for_This_Market_List.Append(Bet_List_Record'(
      --           Bet          => Bet,
      --           Price_Finish => Best_Runners(1)));
           Global_Bets_Allowed(Lay_400_500).Has_Betted := True;
           Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);         
         end if;       
       end if;    

       if not Global_Bets_Allowed(Lay_500_600).Has_Betted then
         if R.Backprice >= Fixed_Type(500)and then
            R.Layprice  >= Fixed_Type(50) and then
            R.Backprice <= Fixed_Type(900) and then
            R.Layprice  <= Fixed_Type(600) then

           Bet.Marketid    := R.Marketid;
           Bet.Selectionid := R.Selectionid;
           Bet.Size        := Global_Lay_Size;
           Bet.Side        := "LAY ";
           Bet.Price       := R.Layprice;
           Bet.Betplaced   := R.Pricets;
           Bet.Status(1) := 'U';
           Move("Lay_500_600",Bet.Betname);
           Bet_List.Append(Bet_List_Record'(
                 Bet          => Bet,
                 Price_Finish => Best_Runners(1)));
      --     Global_Laybet_for_This_Market_List.Append(Bet_List_Record'(
      --           Bet          => Bet,
      --           Price_Finish => Best_Runners(1)));
           Global_Bets_Allowed(Lay_500_600).Has_Betted := True;
           Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);         
         end if;       
       end if;    
       if not Global_Bets_Allowed(Lay_600_700).Has_Betted then
         if R.Backprice >= Fixed_Type(600)and then
            R.Layprice  >= Fixed_Type(50) and then
            R.Backprice <= Fixed_Type(900) and then
            R.Layprice  <= Fixed_Type(700) then

           Bet.Marketid    := R.Marketid;
           Bet.Selectionid := R.Selectionid;
           Bet.Size        := Global_Lay_Size;
           Bet.Side        := "LAY ";
           Bet.Price       := R.Layprice;
           Bet.Betplaced   := R.Pricets;
           Bet.Status(1) := 'U';
           Move("Lay_600_700",Bet.Betname);
           Bet_List.Append(Bet_List_Record'(
                 Bet          => Bet,
                 Price_Finish => Best_Runners(1)));
      --     Global_Laybet_for_This_Market_List.Append(Bet_List_Record'(
      --           Bet          => Bet,
      --           Price_Finish => Best_Runners(1)));
           Global_Bets_Allowed(Lay_600_700).Has_Betted := True;
           Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);         
         end if;       
       end if;    
       
    end loop;     
     
     
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
    
    if not Global_Bets_Allowed(Back_1_10_30_1_2_WIN).Has_Betted then
      if Best_Runners(1).Backprice <= Fixed_Type(1.10) and then
         Best_Runners(1).Backprice >= Fixed_Type(1.04) and then
         Best_Runners(2).Backprice >= Fixed_Type(30.0) and then
         Best_Runners(3).Backprice <  Fixed_Type(10_000.0) then
         -- back this one too
         -- update in place in list
         Bet.Marketid    := Best_Runners(1).Marketid;
         Bet.Selectionid := Best_Runners(1).Selectionid;
         Bet.Size        := Global_Back_Size;
         Bet.Side        := "BACK";
         Bet.Price       := Best_Runners(1).Backprice;
         Bet.Betplaced   := Best_Runners(1).Pricets;
         Bet.Status(1) := 'U';
         Move("Back_1_10_30_1_2_WIN",Bet.Betname);
         Bet_List.Append(Bet_List_Record'(
              Bet          => Bet,
              Price_Finish => Best_Runners(1)));
         Global_Bets_Allowed(Back_1_10_30_1_2_WIN).Has_Betted := True;                
         Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);  
      end if;
    end if;
    
    if not Global_Bets_Allowed(Back_1_10_26_1_2_WIN).Has_Betted then
      if Best_Runners(1).Backprice <= Fixed_Type(1.10) and then
         Best_Runners(1).Backprice >= Fixed_Type(1.04) and then
         Best_Runners(2).Backprice >= Fixed_Type(26.0) and then
         Best_Runners(3).Backprice <  Fixed_Type(10_000.0) then
         -- back this one too
         -- update in place in list
         Bet.Marketid    := Best_Runners(1).Marketid;
         Bet.Selectionid := Best_Runners(1).Selectionid;
         Bet.Size        := Global_Back_Size;
         Bet.Side        := "BACK";
         Bet.Price       := Best_Runners(1).Backprice;
         Bet.Betplaced   := Best_Runners(1).Pricets;
         Bet.Status(1) := 'U';
         Move("Back_1_10_26_1_2_WIN",Bet.Betname);
         Bet_List.Append(Bet_List_Record'(
              Bet          => Bet,
              Price_Finish => Best_Runners(1)));
         Global_Bets_Allowed(Back_1_10_26_1_2_WIN).Has_Betted := True;                
         Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);  
      end if;
    end if;
 
   
    if not Global_Bets_Allowed(Back_1_10_23_1_2_WIN).Has_Betted then
      if Best_Runners(1).Backprice <= Fixed_Type(1.10) and then
         Best_Runners(1).Backprice >= Fixed_Type(1.04) and then
         Best_Runners(2).Backprice >= Fixed_Type(23.0) and then
         Best_Runners(3).Backprice <  Fixed_Type(10_000.0) then
         -- back this one too
         -- update in place in list
         Bet.Marketid    := Best_Runners(1).Marketid;
         Bet.Selectionid := Best_Runners(1).Selectionid;
         Bet.Size        := Global_Back_Size;
         Bet.Side        := "BACK";
         Bet.Price       := Best_Runners(1).Backprice;
         Bet.Betplaced   := Best_Runners(1).Pricets;
         Bet.Status(1) := 'U';
         Move("Back_1_10_23_1_2_WIN",Bet.Betname);
         Bet_List.Append(Bet_List_Record'(
              Bet          => Bet,
              Price_Finish => Best_Runners(1)));
         Global_Bets_Allowed(Back_1_10_23_1_2_WIN).Has_Betted := True;                
         Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);  
      end if;
    end if; 
    
    if not Global_Bets_Allowed(Back_1_10_20_1_2_WIN).Has_Betted then
      if Best_Runners(1).Backprice <= Fixed_Type(1.10) and then
         Best_Runners(1).Backprice >= Fixed_Type(1.04) and then
         Best_Runners(2).Backprice >= Fixed_Type(20.0) and then
         Best_Runners(3).Backprice <  Fixed_Type(10_000.0) then
         -- back this one too
         -- update in place in list
         Bet.Marketid    := Best_Runners(1).Marketid;
         Bet.Selectionid := Best_Runners(1).Selectionid;
         Bet.Size        := Global_Back_Size;
         Bet.Side        := "BACK";
         Bet.Price       := Best_Runners(1).Backprice;
         Bet.Betplaced   := Best_Runners(1).Pricets;
         Bet.Status(1) := 'U';
         Move("Back_1_10_20_1_2_WIN",Bet.Betname);
         Bet_List.Append(Bet_List_Record'(
              Bet          => Bet,
              Price_Finish => Best_Runners(1)));
         Global_Bets_Allowed(Back_1_10_20_1_2_WIN).Has_Betted := True;                
         Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);  
      end if;
    end if; 
    if not Global_Bets_Allowed(Back_1_10_16_1_2_WIN).Has_Betted then
      if Best_Runners(1).Backprice <= Fixed_Type(1.10) and then
         Best_Runners(1).Backprice >= Fixed_Type(1.04) and then
         Best_Runners(2).Backprice >= Fixed_Type(16.0) and then
         Best_Runners(3).Backprice <  Fixed_Type(10_000.0) then
         -- back this one too
         -- update in place in list
         Bet.Marketid    := Best_Runners(1).Marketid;
         Bet.Selectionid := Best_Runners(1).Selectionid;
         Bet.Size        := Global_Back_Size;
         Bet.Side        := "BACK";
         Bet.Price       := Best_Runners(1).Backprice;
         Bet.Betplaced   := Best_Runners(1).Pricets;
         Bet.Status(1) := 'U';
         Move("Back_1_10_16_1_2_WIN",Bet.Betname);
         Bet_List.Append(Bet_List_Record'(
              Bet          => Bet,
              Price_Finish => Best_Runners(1)));
         Global_Bets_Allowed(Back_1_10_16_1_2_WIN).Has_Betted := True;                
         Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);  
      end if;
    end if; 
    if not Global_Bets_Allowed(Back_1_10_13_1_2_WIN).Has_Betted then
      if Best_Runners(1).Backprice <= Fixed_Type(1.10) and then
         Best_Runners(1).Backprice >= Fixed_Type(1.04) and then
         Best_Runners(2).Backprice >= Fixed_Type(13.0) and then
         Best_Runners(3).Backprice <  Fixed_Type(10_000.0) then
         -- back this one too
         -- update in place in list
         Bet.Marketid    := Best_Runners(1).Marketid;
         Bet.Selectionid := Best_Runners(1).Selectionid;
         Bet.Size        := Global_Back_Size;
         Bet.Side        := "BACK";
         Bet.Price       := Best_Runners(1).Backprice;
         Bet.Betplaced   := Best_Runners(1).Pricets;
         Bet.Status(1) := 'U';
         Move("Back_1_10_13_1_2_WIN",Bet.Betname);
         Bet_List.Append(Bet_List_Record'(
              Bet          => Bet,
              Price_Finish => Best_Runners(1)));
         Global_Bets_Allowed(Back_1_10_13_1_2_WIN).Has_Betted := True;                
         Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);  
      end if;
    end if; 
    if not Global_Bets_Allowed(Back_1_10_10_1_2_WIN).Has_Betted then
      if Best_Runners(1).Backprice <= Fixed_Type(1.10) and then
         Best_Runners(1).Backprice >= Fixed_Type(1.04) and then
         Best_Runners(2).Backprice >= Fixed_Type(10.0) and then
         Best_Runners(3).Backprice <  Fixed_Type(10_000.0) then
         -- back this one too
         -- update in place in list
         Bet.Marketid    := Best_Runners(1).Marketid;
         Bet.Selectionid := Best_Runners(1).Selectionid;
         Bet.Size        := Global_Back_Size;
         Bet.Side        := "BACK";
         Bet.Price       := Best_Runners(1).Backprice;
         Bet.Betplaced   := Best_Runners(1).Pricets;
         Bet.Status(1) := 'U';
         Move("Back_1_10_10_1_2_WIN",Bet.Betname);
         Bet_List.Append(Bet_List_Record'(
              Bet          => Bet,
              Price_Finish => Best_Runners(1)));
         Global_Bets_Allowed(Back_1_10_10_1_2_WIN).Has_Betted := True;                
         Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);  
      end if;
    end if; 
    if not Global_Bets_Allowed(Back_1_10_07_1_2_WIN).Has_Betted then
      if Best_Runners(1).Backprice <= Fixed_Type(1.10) and then
         Best_Runners(1).Backprice >= Fixed_Type(1.04) and then
         Best_Runners(2).Backprice >= Fixed_Type(07.0) and then
         Best_Runners(3).Backprice <  Fixed_Type(10_000.0) then
         -- back this one too
         -- update in place in list
         Bet.Marketid    := Best_Runners(1).Marketid;
         Bet.Selectionid := Best_Runners(1).Selectionid;
         Bet.Size        := Global_Back_Size;
         Bet.Side        := "BACK";
         Bet.Price       := Best_Runners(1).Backprice;
         Bet.Betplaced   := Best_Runners(1).Pricets;
         Bet.Status(1) := 'U';
         Move("Back_1_10_07_1_2_WIN",Bet.Betname);
         Bet_List.Append(Bet_List_Record'(
              Bet          => Bet,
              Price_Finish => Best_Runners(1)));
         Global_Bets_Allowed(Back_1_10_07_1_2_WIN).Has_Betted := True;                
         Log("bet list len : " & Bet_List.Length'Img & " " & Bet.To_String);  
      end if;
    end if; 

  end Treat;
  --------------------------------------------------------------------------


  Enough_Runners : Boolean := False;
  use type Ada.Containers.Count_Type;
  
  Date_Start   : Calendar2.Time_Type := (2015,04,01,00,00,00,000);
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

                Enough_Runners := List.Length > 4;
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
        Log("RESULT Month   : " & Current_Date.String_Date_ISO & " " & i'Img );
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
end Simulate;
