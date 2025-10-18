
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Text_Io;
with Table_Apricesfinish;
with Table_Arunners;
with Table_Amarkets;
with Gnat.Command_Line; use Gnat.Command_Line;
---with GNAT.Strings;
with Calendar2; -- use Calendar2;
with Logging; use Logging;
--with General_Routines; use General_Routines;

with Simple_List_Class;
pragma Elaborate_All(Simple_List_Class);

procedure Lay_At_Finish is

   Pricesfinish : Table_Apricesfinish.Data_Type;
   Runner : Table_Arunners.Data_Type;

--    Bad_Input : exception;

   type H_Type is record
     Marketid : Market_Id_Type := (others => ' ');
     Startts  : Calendar2.Time_Type := Calendar2.Time_Type_First;
   end record;

   package H_Pack is new Simple_List_Class(H_Type);
   H_List_2 : H_Pack.List_Type := H_Pack.Create;
   H_List : H_Pack.List_Type := H_Pack.Create;
   H_Data : H_Type;

    function Sort_Condition( Left, Right : H_Type) return Boolean is
    -- Sort new records in list with ascending ts
      use type Calendar2.Time_Type;
    begin
      return Left.Startts <= Right.Startts;
    end Sort_Condition;

    procedure Insert_Item_In_List is new H_Pack.Put( Sort_Condition);


   T            : Sql.Transaction_Type;
   Select_All,
   Select_Runners : Sql.Statement_Type;

   Eos,
   Eos2             : Boolean := False;

   Market           : Markets.Market_Type;

   Config           : Command_Line_Configuration;

   Ia_Best_Position : aliased Integer := 3;
   Ia_Min_Odds      : aliased Integer := 7;
   Ia_Max_Odds      : aliased Integer := 50;

   Lay_Size  : Fixed_Type := 30.0;

   Income, Stake: Fixed_Type := 0.0;

   type Bet_Status_Type is (Bet_Laid, Bet_Won, Bet_Lost, No_Bet_Laid);
   Bet_Status : Bet_Status_Type := No_Bet_Laid;

   type Stats_Type is record
     Hits : Integer_4 := 0;
     Profit : Fixed_Type := 0.0;
   end record ;
   Profit, Global_Profit : Fixed_Type := 0.0;

   Has_A_Winner_Below_1_15 : Boolean := False;
   Has_A_Second_At_Seven_Or_More : Boolean := False;

   Stats : array (Bet_Status_Type'range) of Stats_Type;
   Cnt : Integer := 0;
begin

   Define_Switch
     (Config      => Config,
      Output      => Ia_Best_Position'access,
      Long_Switch => "--best_position=",
      Help        => "best position a horse can have");

   Define_Switch
     (Config      => Config,
      Output      => Ia_Max_Odds'access,
      Long_Switch => "--max_odds=",
      Help        => "Max odds to accept, inclusive, to place the bet");

   Define_Switch
     (Config      => Config,
      Output      => Ia_Min_Odds'access,
      Long_Switch => "--min_odds=",
      Help        => "Min odds to accept, inclusive, to place the bet");

   Getopt (Config);  -- process the command line



--     if Ia_Best_Position = 0 or else
--       Ia_Max_Odds = 0 then
--       Display_Help (Config);
--       return;
--     end if;

   Log ("Connect db");
   Sql.Connect
     (Host     => "localhost",
      Port     => 5432,
      Db_Name  => "nono",
      Login    => "bnl",
      Password => "bnl");
   Log ("Connected to db");

      T.Start;
      Select_All.Prepare ("select distinct(MARKETID) from APRICESFINISH");
      Select_All.Open_Cursor;
      loop
        Select_All.Fetch(Eos);
        exit when Eos;
        Select_All.Get("MARKETID", H_Data.Marketid);
        H_Pack.Insert_At_Tail(H_List_2,H_Data);
      end loop;
      Select_All.Close_Cursor;

      --sort H_List on starts
      -- first read market data
      -- insert sorted on startts in H_list

      H_Pack.Get_First(H_List_2,H_Data,Eos);
      loop
        exit when Eos;
        Market.Marketid := H_Data.Marketid;
        Table_Amarkets.Read(Market, Eos2);
        if not Eos2 then
          H_Data.Startts := Market.Startts;
        else
          H_Data.Startts := Calendar2.Time_Type_First;
        end if;
        Insert_Item_In_List(H_List, H_Data);
        H_Pack.Get_Next(H_List_2,H_Data,Eos);
      end loop;

      Select_Runners.Prepare( "select * " &
            "from APRICESFINISH " &
            "where MARKETID = :MARKETID " &
            "order by BACKPRICE"  ) ;

      Loop_All : while not H_Pack.Is_Empty(H_List) loop
          --reset
          Bet_Status                    := No_Bet_Laid;
          Profit                        := 0.0;
          Has_A_Winner_Below_1_15       := False;
          Has_A_Second_At_Seven_Or_More := False;
          Cnt                           := 0;
          H_Pack.Remove_From_Head(H_List, H_Data);
          Select_Runners.Set("MARKETID", H_Data.Marketid);
          Select_Runners.Open_Cursor;
          Pricesfinish := Table_Apricesfinish.Empty_Data;

          Loop_Runner : loop
            Select_Runners.Fetch(Eos2);
            exit Loop_Runner when Eos2;
            Pricesfinish := Table_Apricesfinish.Get(Select_Runners);
            Cnt := Cnt +1;
            if not Has_A_Winner_Below_1_15 and then Cnt = 1 then
              Has_A_Winner_Below_1_15 := Pricesfinish.Backprice <= Fixed_Type(1.15);
            elsif not Has_A_Second_At_Seven_Or_More and then Cnt = 2 then
              Has_A_Second_At_Seven_Or_More := Pricesfinish.Backprice >= Fixed_Type(7.0);
            end if;

            if Has_A_Winner_Below_1_15 and then
               Has_A_Second_At_Seven_Or_More and then
               Cnt >= Ia_Best_Position and then
               Pricesfinish.Layprice >= Fixed_Type(Ia_Min_Odds) and then
               Pricesfinish.Layprice <= Fixed_Type(Ia_Max_Odds) then
               Bet_Status := Bet_Laid;
               exit Loop_Runner; -- 1 bet per race
            end if;
          end loop Loop_Runner;
          Select_Runners.Close_Cursor;

          if Bet_Status = Bet_Laid then
            Runner.Marketid := Pricesfinish.Marketid;
            Runner.Selectionid := Pricesfinish.Selectionid;
            Table_Arunners.Read(Runner, Eos);
            if not Eos then
              if Runner.Status(1..6) = "WINNER" then
                Bet_Status := Bet_Lost;
              elsif Runner.Status(1..5) = "LOSER" then
                Bet_Status := Bet_Won;
              else
                Bet_Status := No_Bet_Laid;
              end if;
            else
              Bet_Status := No_Bet_Laid;
            end if;
          end if;

          case Bet_Status is
             when Bet_Laid | No_Bet_Laid =>    -- no bet at all
               Income := 0.0;
               Stake  := 0.0;
               Profit := 0.0;
             when Bet_Won =>  -- A winning lay bet
               Income := 0.935 * Lay_Size;
               Stake  := 0.0;
               Profit := Income;
             when Bet_Lost =>  -- A losing lay bet
               Income := 0.0;
               Stake  := Lay_Size * (Pricesfinish.Layprice - 1.0);
               Profit := - Stake;
          end case;
          Stats(Bet_Status).Hits := Stats(Bet_Status).Hits + 1;
          Stats(Bet_Status).Profit := Stats(Bet_Status).Profit + Profit;
--          Log(Bet_Status'Img & Table_Apricesfinish.To_String(Pricesfinish));

          Global_Profit := Global_Profit + Profit;
          Log("|" & Calendar2.String_Date_Time_ISO(H_Data.Startts, " ", "") & "|" & Integer_4(Global_Profit)'Img);

      end loop Loop_All;
      T.Commit ;

   Sql.Close_Session;

   Log("Total profit = " & Integer_4(Global_Profit)'Img);
   for i in Bet_Status_Type'range loop
     Log(i'Img & Stats(i).Hits'Img & Integer_4(Stats(i).Profit)'Img);
   end loop;
   Log("used --best_position=" & Ia_Best_Position'Img &
     " --min_odds=" & Ia_Min_Odds'Img &
     " --max_odds=" & Ia_Max_Odds'Img);
exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Lay_At_Finish;
