with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
--with Text_Io;
--with Gnat.Command_Line; use Gnat.Command_Line;
---with GNAT.Strings;

with Sim;
with Utils; use Utils;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
with Price_Histories;
with Bets;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Bot_System_Number;

procedure Back_During_Race_And_Lay_Later is

  package EV renames Ada.Environment_Variables;
  Bad_Bet_Side : exception;
  -- Market_Id_With_Data_Pack
  -- Holds list of all market ids that has data


  type Bet_Type is record
    Backbet           : Bets.Bet_Type;
    Laybet            : Bets.Bet_Type;
    --Price_Finish_Back : Price_Histories.Price_History_Type;
    --Price_Finish_Lay  : Price_Histories.Price_History_Type;
  end record;

  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_Type);

  Global_Bet_List : Bet_List_Pack.List;
--  Config           : Command_Line_Configuration;

--  IA_Back_At_Back_Price : aliased Integer := 50;
--  IA_Lay_At_Back_Price  : aliased Integer := 100;
--  IA_Max_Lay_Price      : aliased Integer := 200;

--  Lay_Size  : Fixed_Type := 30.0;
--  Back_Size : Fixed_Type := 1500.0;

  --type Bet_Status_Type is (No_Bet_Laid, Bet_Laid);
  --Bet_Status : Bet_Status_Type := No_Bet_Laid;

  Global_Min_Backprice1     : constant Fixed_Type := 1.31;
  Global_Max_Backprice1     : constant Fixed_Type := 1.36;
  Global_Min_Backprice2     : constant Fixed_Type := 2.5;
  Global_Max_Backprice2     : constant Fixed_Type := 10.0;
  Global_Lay_At_Backprice   : constant Fixed_Type := 1.25;
  Global_Lay_Size           : constant Fixed_Type := 110.0;
  Global_Back_Size          : constant Fixed_Type := 100.0;

  Start : Calendar2.Time_Type := Calendar2.Clock;

  type Side_Type is (Lay,Back);

--  Old_Market_Of_Sample,
--  Current_Market_Of_Sample : Price_Histories.Price_History_Type;

  function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new  Price_Histories.Lists.Generic_Sorting("<");

  type Best_Runners_Array_Type is array (1..4) of Price_Histories.Price_History_Type ;
  Best_Runners      : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);


  --------------------------------------------------------------------------
  procedure Check_Lay_Bet_Matched(Bet      : in out Bet_Type ;
                                  Bet_List : in out Bet_List_Pack.List;
                                  List     : in     Price_Histories.Lists.List ) is
  begin
    for R of List loop
      if Bet.Laybet.Selectionid = R.Selectionid and then
         Bet.Laybet.Status(1) = 'U' and then
         R.Pricets > Bet.Laybet.Betplaced + (0,0,0,1,0) and then -- 1 second later at least, time for BF delay
         R.Layprice <= Bet.Laybet.Price and then -- Laybet so yes '<=' NOT '>='
         R.Layprice >  Fixed_Type(1.0) then -- sanity

        Bet.Laybet.Status(1) := 'M';
        Bet.Laybet.Pricematched := R.Layprice;
        Log("Matched Laybet " & Bet.Laybet.To_String);
        -- update the bet in the bet list too
        for b of Bet_List loop
          if b.Laybet.Marketid    = Bet.Laybet.Marketid and then
             b.Laybet.Selectionid = Bet.Laybet.Selectionid then
            b.Laybet.Status(1) := 'M';
            Log("updated laybet in betlist");
            exit;
          end if;
        end loop;
        exit;
      end if;
    end loop;
  end Check_Lay_Bet_Matched;
  --------------------------------------------
  procedure Check_Back_Bet_Matched(Bet        : in out Bet_Type ;
                                   Bet_List   : in out Bet_List_Pack.List;
                                   List       : in     Price_Histories.Lists.List) is
  begin
    for R of List loop
      if Bet.Backbet.Selectionid = R.Selectionid and then
         Bet.Backbet.Status(1) = 'U' and then
         R.Pricets > Bet.Backbet.Betplaced + (0,0,0,1,0) and then -- 1 second later at least, time for BF delay
         R.Backprice >= Bet.Backbet.Price and then -- Backbet so yes '<=' NOT '>='
         R.Backprice >  Fixed_Type(1.0) then -- sanity

        Bet.Backbet.Status(1) := 'M';
        Bet.Backbet.Pricematched := R.Backprice;
        Log("Matched backbet " & Bet.Backbet.To_String);
        -- update the bet in the bet list too
        for b of Bet_List loop
          if b.Backbet.Marketid    = Bet.Backbet.Marketid and then
             b.Backbet.Selectionid = Bet.Backbet.Selectionid then
            b.Backbet.Status(1) := 'M';
            Log("updated backbet in betlist");
            exit;
          end if;
        end loop;
        exit;
      end if;
    end loop;
  end Check_Back_Bet_Matched;
  --------------------------------------------
  procedure Treat(BR             : in     Best_Runners_Array_Type;
                  Bet            : in out Bet_Type;
                  Bet_Placed     : in out Boolean;
                  Bet_List       : in out Bet_List_Pack.List;
                  Max_Backprice1 : in     Fixed_Type;
                  Min_Backprice1 : in     Fixed_Type;
                  Max_Backprice2 : in     Fixed_Type;
                  Min_Backprice2 : in     Fixed_Type;
                  Lay_Atprice    : in     Fixed_Type ) is
  begin
    if Bet_Placed then
      return;
    end if;

    if BR(1).Backprice >= Min_Backprice1 and then
       BR(1).Backprice <= Max_Backprice1 and then
       BR(2).Backprice <= Max_Backprice2 and then
       BR(2).Backprice >= Min_Backprice2  then

      --back bet
      Bet.Backbet.Marketid    := BR(1).Marketid;
      Bet.Backbet.Selectionid := BR(1).Selectionid;
      Bet.Backbet.Size        := Global_Back_Size;
      Bet.Backbet.Side        := "BACK";
      Bet.Backbet.Price       := BR(1).Backprice;
      Bet.Backbet.Betplaced   := BR(1).Pricets;
      Bet.Backbet.Status(1) := 'U';
      Move("BACK_" &
            F8_Image(Min_Backprice1) & "_" &
            F8_Image(Max_Backprice1) & "_" &
            F8_Image(Min_Backprice2) & "_" &
            F8_Image(Max_Backprice2) & "_" &
            F8_Image(Lay_Atprice),
            Bet.Backbet.Betname);

      --lay bet
      Bet.Laybet.Marketid    := BR(1).Marketid;
      Bet.Laybet.Selectionid := BR(1).Selectionid;
      Bet.Laybet.Size        := Global_Lay_Size;
      Bet.Laybet.Side        := "LAY ";
      Bet.Laybet.Price       := Lay_Atprice;
      Bet.Laybet.Betplaced   := BR(1).Pricets;
      Bet.Laybet.Status(1) := 'U';
      Move("LAY_" &
            F8_Image(Min_Backprice1) & "_" &
            F8_Image(Max_Backprice1) & "_" &
            F8_Image(Min_Backprice2) & "_" &
            F8_Image(Max_Backprice2) & "_" &
            F8_Image(Lay_Atprice) ,
            Bet.Laybet.Betname);


      Bet_List.Append(Bet);
      Bet_Placed := True;
    end if;
  end Treat;
  --------------------------------------------------------------------------


  Enough_Runners : Boolean := False;
  use type Ada.Containers.Count_Type;
  Price : Price_Histories.Price_History_Type;

  Day : Time_Type := (2016,03,19,00,00,00,000);
  End_Date : Time_Type := Clock;
  One_Day : Interval_Type := (1,0,0,0,0);
begin

  Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

  Log ("Connect db");
  Sql.Connect
    (Host     => "localhost",
     Port     => 5432,
     Db_Name  => "dry",
     Login    => "bnl",
     Password => "bnl");
  Log ("Connected to db");

  Day_Loop : loop

    Day := Day + One_Day;
    exit Day_Loop when Day.Year  =  End_Date.Year and then
                       Day.Month = End_Date.Month and then
                       Day.Day   > End_Date.Day;
    Sim.Fill_Data_Maps(Day, Bot_Types.Horse);
    Log("start process date " & Day.To_String);

    declare
      Cnt : Integer := 0;
      Is_Win : Boolean := True;
      Bet : Bet_Type;
    begin
      Log("num markets " & Day.To_String & " " & Sim.Market_With_Data_List.Length'Img);

      Loop_Market : for Market of Sim.Market_With_Data_List loop
        Is_Win := Market.Markettype(1..3) = "WIN";

        if Is_Win then
          Log("Treat market " & Market.To_String );
          Cnt := Cnt + 1;
          -- list of timestamps in this market
          declare
            Timestamp_To_Apriceshistory_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                          Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);
            Bet_Placed : Boolean := False;
            First : Boolean := True;
          begin
            Loop_Timestamp : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              --Log("Treat marketid '" & Market.Marketid & "' pricets " & Timestamp.To_String);
              declare
                List : Price_Histories.Lists.List :=
                          Timestamp_To_Apriceshistory_Map(Timestamp.To_String);
              begin
                if First then
                  Enough_Runners := List.Length > 6;
                  First := False;
                  if not Enough_Runners then
                    exit Loop_Timestamp;  -- too few runners
                  end if;
                end if;

                --Current_Market_Of_Sample := List.First_Element;
                --if Current_Market_Of_Sample.Marketid /= Old_Market_Of_Sample.Marketid then
                --  Log("Treat marketid '" & Current_Market_Of_Sample.Marketid & "' " &
                --      "pricets " & Current_Market_Of_Sample.Pricets.To_String);
                --  Old_Market_Of_Sample := Current_Market_Of_Sample;
                --end if;

                Backprice_Sorter.Sort(List);
                Price.Backprice := 10_000.0;
                Best_Runners := (others => Price);

                declare
                  Idx : Integer := 0;
                begin
                  for Tmp of List loop
                    Idx := Idx +1;
                    exit when Idx > Best_Runners'Last;
                    Best_Runners(Idx) := Tmp;
                  end loop;
                end ;

                Treat(BR             => Best_Runners,
                      Bet            => Bet,
                      Bet_Placed     => Bet_Placed,
                      Bet_List       => Global_Bet_List,
                      Max_Backprice1 => Global_Max_Backprice1,
                      Min_Backprice1 => Global_Min_Backprice1,
                      Max_Backprice2 => Global_Max_Backprice2,
                      Min_Backprice2 => Global_Min_Backprice2,
                      Lay_Atprice    => Global_Lay_At_Backprice);

                if Bet.Backbet.Status(1) /= 'M' then
                  Check_Back_Bet_Matched(Bet => Bet, Bet_List => Global_Bet_List, List => List);
                end if;
                -- check laybet only if backbet was matched
                if Bet.Backbet.Status(1) = 'M'  and then Bet.Laybet.Status(1) /= 'M' then
                  Check_Lay_Bet_Matched (Bet => Bet, Bet_List => Global_Bet_List, List => List);
                end if;
              end;
              exit Loop_Market when Bet.Backbet.Status(1) = 'M' and then Bet.Laybet.Status(1) = 'M';
            end loop Loop_Timestamp; --  Timestamp
          end;
        end if; -- Is_Win
      end loop Loop_Market;  -- marketid

    end;
    Log("num bets laid" & Global_Bet_List.Length'Img);

    declare
      Profit, Sum, Sum_Winners, Sum_Losers  : array (Side_Type'range) of Fixed_Type   := (others => 0.0);
      Winners, Losers, Unmatched, Strange   : array (Side_Type'range) of Integer_4 := (others => 0);
      T : Sql.Transaction_Type;
    begin
      T.Start;
      for Bet of Global_Bet_List loop
        --Log("----------------");

        Bet.Laybet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
        case Bet.Laybet.Status(1) is
          when 'M' => -- matched
            Bet.Laybet.Betwon := True;
            if Bet.Laybet.Side(1..3) = "LAY" then
              -- did it win ?
              if Sim.Is_Race_Winner(Bet.Laybet.Selectionid, Bet.Laybet.Marketid) then
                Profit(Lay) := -(Bet.Laybet.Price - 1.0) * Bet.Laybet.Size ;
                Losers(Lay) := Losers(Lay) +1;
                Sum_Losers(Lay) := Sum_Losers(Lay) + Profit(Lay);
                Log("bad Laybet: " & Bet.Laybet.To_String);
                Bet.Laybet.Betwon := False;
              else-- lost
                Profit(Lay) := Bet.Laybet.Size; --Lay_Size * 0.935;
                Winners(Lay) := Winners(Lay)+1;
                Sum_Winners(Lay) := Sum_Winners(Lay) + Profit(Lay);
              end if;
              Bet.Laybet.Profit := Profit(Lay);
            else
              raise Bad_Bet_Side with "Bet.Laybet.Side ='" & Bet.Laybet.Side & "'";
            end if;
            Bet.Laybet.Insert;
          when 'U'  => -- unmatched
            Bet.Laybet.Betwon := True;
            Unmatched(Lay) := Unmatched(Lay) +1;
            Bet.Laybet.Insert;
          when others => --Strange
            Bet.Laybet.Betwon := True;
            Strange(Lay) := Strange(Lay) +1;
            Bet.Laybet.Insert;
        end case;


        case Bet.Backbet.Status(1) is
          when 'M'  => -- matched
            Bet.Backbet.Betwon := False;
            Bet.Backbet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
            if Bet.Backbet.Side(1..4) = "BACK" then
              -- did it win ?
              if Sim.Is_Race_Winner(Bet.Backbet.Selectionid, Bet.Backbet.Marketid) then
                Profit(Back) := (Bet.Backbet.Price - 1.0) * Bet.Backbet.Size ;
                Winners(Back) := Winners(Back)+1;
                Sum_Winners(Back) := Sum_Winners(Back) + Profit(Back);
                Bet.Backbet.Betwon := True;
              else-- lost
                Profit(Back) := -Bet.Backbet.Size ;
                Losers(Back) := Losers(Back) +1;
                Sum_Losers(Back) := Sum_Losers(Back) + Profit(Back);
                Log("bad Backbet: " & Bet.Backbet.To_String);
              end if;
              Bet.Backbet.Profit := Profit(Back);
            else
              raise Bad_Bet_Side with "Bet.Backbet.Side ='" & Bet.Backbet.Side & "'";
            end if;
            Bet.Backbet.Insert;

          when 'U'  => -- unmatched
            Unmatched(Back) := Unmatched(Back) +1;
            Bet.Backbet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
            Bet.Backbet.Betwon := True;
            Bet.Backbet.Insert;
          when ' '  => -- no back bet layed
            null;
          when others => --Strange
            Strange(Back) := Strange(Back) +1;
            Bet.Backbet.Betwon := True;
            Bet.Backbet.Betid := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));
            Bet.Backbet.Insert;
        end case;
      end loop;
      T.Commit;

      for i in Side_Type'range loop
        Sum(i) := Sum_Winners(i) + Sum_Losers(i) ;
        Log("RESULT day       : " & Day.To_String & " " & i'Img );
        Log("RESULT Winners   : " & Winners(i)'Img & " " & Integer_4(Sum_Winners(i))'Img );
        Log("RESULT Losers    : " & Losers(i)'Img  & " " & Integer_4(Sum_Losers(i))'Img);
        Log("RESULT Unmatched : " & Unmatched(i)'Img  & " " & Unmatched(i)'Img);
        Log("RESULT Strange   : " & Strange(i)'Img  & " " & Strange(i)'Img);
        Log("RESULT Sum       : " & Integer_4(Sum(i))'Img );
      end loop;
      Log(" Min_Backprice1:" & Global_Min_Backprice1'Img &
          " Max_Backprice1:" & Global_Max_Backprice1'Img &
          " Min_Backprice2:" & Global_Min_Backprice2'Img &
          " Max_Backprice2:" & Global_Max_Backprice2'Img);

      Log(" GT:" &  Integer(Sum(Back) + Sum(Lay))'Img);
    end ;

    Global_Bet_List.Clear;
  end loop Day_Loop;
  Sql.Close_Session;
  Log("Started : " & Start.To_String);
  Log("Done : " & Calendar2.Clock.To_String);
  Logging.Close;


  exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Back_During_Race_And_Lay_Later;
