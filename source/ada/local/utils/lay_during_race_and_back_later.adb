with Ada.Containers.Doubly_Linked_Lists;
--with Ada.Strings;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Sim;
--with Utils; use Utils;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Text_Io;
with Table_Apriceshistory;
with Table_Abets;
--with Gnat.Command_Line; use Gnat.Command_Line;
---with GNAT.Strings;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Bot_System_Number;

procedure Lay_During_Race_And_Back_Later is

  Bad_Bet_Side : exception;
  -- Market_Id_With_Data_Pack
  -- Holds list of all market ids that has data


  type Bet_List_Record is record
    Backbet           : Table_Abets.Data_Type;
    Laybet            : Table_Abets.Data_Type;
    Price_Finish_Back : Table_Apriceshistory.Data_Type;
    Price_Finish_Lay  : Table_Apriceshistory.Data_Type;
  end record;

  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_List_Record);

  Global_Bet_List : Bet_List_Pack.List;
--  Config           : Command_Line_Configuration;

--  IA_Back_At_Back_Price : aliased Integer := 50;
--  IA_Lay_At_Back_Price  : aliased Integer := 100;
--  IA_Max_Lay_Price      : aliased Integer := 200;

  Lay_Size  : Fixed_Type := 30.0;
  Back_Size : Fixed_Type := 1500.0;

  --type Bet_Status_Type is (No_Bet_Laid, Bet_Laid);
  --Bet_Status : Bet_Status_Type := No_Bet_Laid;

  Global_Min_Backprice     : constant Integer_4 := 160;
  Global_Max_Backprice     : constant Integer_4 := 400;
  Global_Min_Layprice      : constant Integer_4 := 100;
  Global_Max_Layprice      : constant Integer_4 := 200;
  Global_Back_At_Backprice : constant Integer_4 := 2;

  Start                 : Calendar2.Time_Type := Calendar2.Clock;

  type Side_Type is (Lay,Back);

  Old_Market_Of_Sample,
  Current_Market_Of_Sample : Table_Apriceshistory.Data_Type;

  --------------------------------------------------------------------------
  procedure Check_Lay_Bet_Matched(List     : in     Table_Apriceshistory.Apriceshistory_List_Pack2.List ;
                                  Bet_List : in out Bet_List_Pack.List) is

  begin
    for R of List loop
      for B of Bet_List loop
        if B.Laybet.Selectionid =  R.Selectionid then
          if B.Laybet.Status(1) = 'U' and then
            R.Pricets >  B.Laybet.Betplaced + (0,0,0,1,0) and then -- 1 second later at least, time for BF delay
            R.Layprice <= B.Laybet.Price and then -- Laybet so yes '<=' NOT '>='
            R.Layprice >  Fixed_Type(1.0) and then -- sanity
            R.Backprice >  Fixed_Type(1.0) then -- sanity

             B.Laybet.Status(1) := 'M';
             B.Laybet.Pricematched := R.Layprice;
             Log("Matched Laybet " & B.Laybet.To_String);
          end if;
        end if;
      end loop;
    end loop;
  end Check_Lay_Bet_Matched;
  --------------------------------------------

  procedure Check_Back_Bet_Matched(List     : in     Table_Apriceshistory.Apriceshistory_List_Pack2.List ;
                                   Bet_List : in out Bet_List_Pack.List) is
  begin
    for R of List loop
      for B of Bet_List loop
        if B.Backbet.Selectionid =  R.Selectionid then
          if B.Backbet.Status(1) = 'U' and then
            R.Pricets >  B.Backbet.Betplaced + (0,0,0,1,0) and then -- 1 second later at least, time for BF delay
            R.Backprice >= B.Backbet.Price and then -- Backbet so yes '<=' NOT '>='
            R.Backprice >  Fixed_Type(1.0) and then -- sanity
            R.Backprice >  Fixed_Type(1.0) then -- sanity

             B.Backbet.Status(1) := 'M';
             B.Backbet.Pricematched := R.Backprice;
             Log("Matched backbet " & B.Backbet.To_String);

          end if;
        end if;
      end loop;
    end loop;
  end Check_Back_Bet_Matched;
  --------------------------------------------



--  function "<" (Left,Right : Table_Apriceshistory.Data_Type) return Boolean is
--  begin
--    return Left.Backprice < Right.Backprice;
--  end "<";
--  --------------------------------------------
--  package Backprice_Sorter is new Table_Apriceshistory.Apriceshistory_List_Pack2.Generic_Sorting("<");
--
--  type Best_Runners_Array_Type is array (1..4) of Table_Apriceshistory.Data_Type;


  procedure Treat_Lay(List         : in     Table_Apriceshistory.Apriceshistory_List_Pack2.List ;
                      Bet_List     : in out Bet_List_Pack.List;
                      Max_Backprice: in     Integer_4;
                      Min_Backprice: in     Integer_4;
                      Max_Layprice : in     Integer_4;
                      Min_Layprice : in     Integer_4) is
    Bet : Table_Abets.Data_Type;
    Bet_Already_Laid : Boolean := False;
  begin
     -- check for bet already laid for this runner on this market
     for R of List loop
       Bet_Already_Laid := False;
       for B of Bet_List loop
         if R.Selectionid = B.Laybet.Selectionid and then
            R.Marketid    = B.Laybet.Marketid then

            Bet_Already_Laid := True;
         end if;
       end loop;

       if not Bet_Already_Laid then
         -- place a laybet if odds are ok
         if R.Backprice >= Fixed_Type(Min_Backprice)and then
            R.Layprice  >= Fixed_Type(Min_Layprice) and then
            R.Backprice <= Fixed_Type(Max_Backprice) and then
            R.Layprice  <= Fixed_Type(Max_Layprice) then

           Bet.Marketid    := R.Marketid;
           Bet.Selectionid := R.Selectionid;
           Bet.Size        := Lay_Size;
           Bet.Side        := "LAY ";
           Bet.Price       := R.Layprice;
           Bet.Betplaced   := R.Pricets;
           Bet.Status(1) := 'U';
           Bet_List.Append(Bet_List_Record'(
                       Backbet => Table_Abets.Empty_Data,
                       Laybet  => Bet,
                       Price_Finish_Back => Table_Apriceshistory.Empty_Data,
                       Price_Finish_Lay => R)
                     );
         end if;
       end if;
     end loop;
  end Treat_Lay;
  --------------------------------------------------------------------------
  procedure Treat_Back(List         : in     Table_Apriceshistory.Apriceshistory_List_Pack2.List ;
                       Bet_List     : in out Bet_List_Pack.List) is
  begin
     -- check for bet already laid for this runner on this market
     -- if so, check that it is not falliung in backodds to much -> going to win
     for R of List loop
       for B of Bet_List loop
         if R.Selectionid = B.Laybet.Selectionid and then
            R.Marketid    = B.Laybet.Marketid and then
            B.Laybet.Status(1) = 'M' and then   -- matched laybet
            B.Backbet.Status(1) = ' ' then   -- not progressing backbet

           if R.Backprice <= Fixed_Type(Global_Back_At_Backprice) and then
              R.Backprice >= Fixed_Type(1.0) then --sanit
              -- this looks bad, back this one too
              -- update in place in list
              B.Backbet.Marketid    := R.Marketid;
              B.Backbet.Selectionid := R.Selectionid;
              B.Backbet.Size        := Back_Size;
              B.Backbet.Side        := "BACK";
              B.Backbet.Price       := R.Backprice;
              B.Backbet.Betplaced   := R.Pricets;
              B.Backbet.Status(1) := 'U';
           end if;
         end if;
       end loop;
     end loop;
  end Treat_Back;
  --------------------------------------------------------------------------


  Enough_Runners : Boolean := False;
  use type Ada.Containers.Count_Type;

begin

  for Month in Calendar2.Short_Month_Type'range loop
--  for Month in Aug .. Sep loop
    Log ("Connect db");
    Sql.Connect
      (Host     => "localhost",
       Port     => 5432,
       Db_Name  => "dry",
       Login    => "bnl",
       Password => "bnl");
    Log ("Connected to db");
    Sim.Fill_Data_Maps(Month);
    Sql.Close_Session;    -- no need for db anymore
    Log("start process Month " & Month'Img);

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

                Current_Market_Of_Sample := List.First_Element;
                if Current_Market_Of_Sample.Marketid /= Old_Market_Of_Sample.Marketid then
                  Log("Treat marketid '" & Current_Market_Of_Sample.Marketid & "' " &
                      "pricets " & Current_Market_Of_Sample.Pricets.To_String);
                  Old_Market_Of_Sample := Current_Market_Of_Sample;
                end if;

                if Enough_Runners then
                  Treat_Lay(List          => List,
                            Bet_List      => Global_Bet_List,
                            Max_Backprice => Global_Max_Backprice,
                            Min_Backprice => Global_Min_Backprice,
                            Max_Layprice  => Global_Max_Layprice,
                            Min_Layprice  => Global_Min_Layprice);
                  Check_Lay_Bet_Matched(List => List, Bet_List => Global_Bet_List);

                  Treat_Back(List => List, Bet_List => Global_Bet_List);

                  Check_Back_Bet_Matched(List => List, Bet_List => Global_Bet_List);
                end if;
              end;
            end loop; --  Timestamp
          end;
        end if; -- Is_Win
      end loop;  -- marketid
    end;

    Log("num bets laid" & Global_Bet_List.Length'Img);



    Sql.Connect
      (Host     => "localhost",
       Port     => 5432,
       Db_Name  => "dry",
       Login    => "bnl",
       Password => "bnl");
    Log ("Connected to db");

    declare
      Profit, Sum, Sum_Winners, Sum_Losers  : array (Side_Type'range) of Fixed_Type   := (others => 0.0);
      Winners, Losers, Unmatched, Strange   : array (Side_Type'range) of Integer_4 := (others => 0);
      T : Sql.Transaction_Type;
    begin
      T.Start;
      for Bet_Record of Global_Bet_List loop
        --Log("----------------");

        Bet_Record.Laybet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
        case Bet_Record.Laybet.Status(1) is
          when 'M'  => -- matched
            Bet_Record.Laybet.Betwon := True;
            if Bet_Record.Laybet.Side(1..3) = "LAY" then
              -- did it win ?
              if Sim.Is_Race_Winner(Bet_Record.Laybet.Selectionid, Bet_Record.Laybet.Marketid) then
                Profit(Lay) := -(Bet_Record.Laybet.Price - 1.0) * Lay_Size ;
                Losers(Lay) := Losers(Lay) +1;
                Sum_Losers(Lay) := Sum_Losers(Lay) + Profit(Lay);
                Log("bad Laybet: " & Bet_Record.Price_Finish_Lay.To_String);
                Bet_Record.Laybet.Betwon := False;
              else-- lost
                Profit(Lay) := Lay_Size * 0.935;
                Winners(Lay) := Winners(Lay)+1;
                Sum_Winners(Lay) := Sum_Winners(Lay) + Profit(Lay);
              end if;
              Bet_Record.Laybet.Profit := Profit(Lay);
            else
              raise Bad_Bet_Side with "Bet_Record.Laybet.Side ='" & Bet_Record.Laybet.Side & "'";
            end if;
            Bet_Record.Laybet.Insert;
          when 'U'  => -- unmatched
            Bet_Record.Laybet.Betwon := True;
            Unmatched(Lay) := Unmatched(Lay) +1;
            Bet_Record.Laybet.Insert;
          when others => --Strange
            Bet_Record.Laybet.Betwon := True;
            Strange(Lay) := Strange(Lay) +1;
            Bet_Record.Laybet.Insert;
        end case;


        case Bet_Record.Backbet.Status(1) is

          when 'M'  => -- matched
            Bet_Record.Backbet.Betwon := False;
            Bet_Record.Backbet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
            if Bet_Record.Backbet.Side(1..4) = "BACK" then
              -- did it win ?
              if Sim.Is_Race_Winner(Bet_Record.Backbet.Selectionid, Bet_Record.Backbet.Marketid) then
                Profit(Back) := (Bet_Record.Backbet.Price - 1.0) * Back_Size * 0.935;
                Winners(Back) := Winners(Back)+1;
                Sum_Winners(Back) := Sum_Winners(Back) + Profit(Back);
                Bet_Record.Backbet.Betwon := True;
              else-- lost
                Profit(Back) := -Back_Size ;
                Losers(Back) := Losers(Back) +1;
                Sum_Losers(Back) := Sum_Losers(Back) + Profit(Back);
                Log("bad Backbet: " & Bet_Record.Price_Finish_Back.To_String);
              end if;
              Bet_Record.Backbet.Profit := Profit(Back);              
            else
              raise Bad_Bet_Side with "Bet_Record.Backbet.Side ='" & Bet_Record.Backbet.Side & "'";
            end if;
            Bet_Record.Backbet.Insert;

          when 'U'  => -- unmatched
            Unmatched(Back) := Unmatched(Back) +1;
            Bet_Record.Backbet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
            Bet_Record.Backbet.Betwon := True;
            Bet_Record.Backbet.Insert;
          when ' '  => -- no back bet layed
            null;
          when others => --Strange
            Strange(Back) := Strange(Back) +1;
            Bet_Record.Backbet.Betwon := True;
            Bet_Record.Backbet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
            Bet_Record.Backbet.Insert;
        end case;
      end loop;
      T.Commit;

      for i in Side_Type'range loop
        Sum(i) := Sum_Winners(i) + Sum_Losers(i) ;
        Log("RESULT Month   : " & Month'Img & " " & i'Img );
        Log("RESULT Winners   : " & Winners(i)'Img & " " & Integer_4(Sum_Winners(i))'Img );
        Log("RESULT Losers    : " & Losers(i)'Img  & " " & Integer_4(Sum_Losers(i))'Img);
        Log("RESULT Unmatched : " & Unmatched(i)'Img  & " " & Unmatched(i)'Img);
        Log("RESULT Strange   : " & Strange(i)'Img  & " " & Strange(i)'Img);
        Log("RESULT Sum       : " & Integer_4(Sum(i))'Img );
      end loop;
      Log(" Min_Backprice:" & Global_Min_Backprice'Img &
          " Max_Backprice:" & Global_Max_Backprice'Img &
          " Min_Layprice:"  & Global_Min_Layprice'Img  &
          " Max_Layprice:"  & Global_Max_Layprice'Img);

      Log(" GT:" &  Integer(Sum(Back) + Sum(Lay))'Img);
    end ;
    Sql.Close_Session;    -- no need for db anymore

    Global_Bet_List.Clear;
  end loop;
  Log("Started : " & Start.To_String);
  Log("Done : " & Calendar2.Clock.To_String);


  exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Lay_During_Race_And_Back_Later;
