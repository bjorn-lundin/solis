
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Text_Io;
with Table_Araceprices;
with Table_Arunners;
with Table_Amarkets;
with Gnat.Command_Line; use Gnat.Command_Line;
---with GNAT.Strings;
--with Calendar2; -- use Calendar2;
with Logging; use Logging;
--with General_Routines; use General_Routines;

--with Simple_List_Class;
--pragma Elaborate_All(Simple_List_Class);

procedure Lay_During_Race is

   Prices : Table_Araceprices.Data_Type;
   Runner : Table_Arunners.Data_Type;

--    Bad_Input : exception;

   type H_Type is record
     Marketid    : Marketid_Type := (others => ' ');
     Selectionid : Integer_4 := 0;
   end record;

   Data : H_Type;

   T            : Sql.Transaction_Type;
   Select_All_Markets,
   Select_Race_Runners_In_One_Market,
   Select_Prices_For_Runner_In_One_Market : Sql.Statement_Type;

   Eos,
   Eos2,
   Eos3      : Boolean := False;

   Market : Markets.Market_Type;

   Config           : Command_Line_Configuration;

   IA_Max_Start_Price : aliased Integer := 30;
   IA_Lay_At_Price    : aliased Integer := 50;
   IA_Max_Lay_Price   : aliased Integer := 100;

   Lay_Size  : Fixed_Type := 30.0;

   Income, Stake: Fixed_Type := 0.0;

   type Bet_Status_Type is (Bet_Laid, Bet_Won, Bet_Lost, No_Bet_Laid);
   Bet_Status : Bet_Status_Type := No_Bet_Laid;

   type Stats_Type is record
     Hits : Integer_4 := 0;
     Profit : Fixed_Type := 0.0;
   end record ;
   Profit, Global_Profit : Fixed_Type := 0.0;

   OK_Starting_Price : Boolean := False;
   First_Loop        : Boolean := True;

   Stats : array (Bet_Status_Type'range) of Stats_Type;
   Cnt : Integer := 0;
begin
  Define_Switch
    (Config      => Config,
     Output      => IA_Max_Start_Price'access,
     Long_Switch => "--max_start_price=",
     Help        => "starting price (back)(");

  Define_Switch
    (Config      => Config,
     Output      => Ia_Lay_At_Price'access,
     Long_Switch => "--lay_at_price=",
     Help        => "Lay the runner at this price(Back)");

  Define_Switch
    (Config      => Config,
     Output      => IA_Max_Lay_Price'access,
     Long_Switch => "--max_lay_price=",
     Help        => "Runner cannot have higer price that this when layed (Lay)");

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
  Select_All_Markets.Prepare ("select distinct(MARKETID) " &
                      "from ARACEPRICES " &
                      "order by MARKETID");
                      
  Select_Race_Runners_In_One_Market.Prepare( "select * " &
        "from ARUNNERS " &
        "where MARKETID = :MARKETID " &
        "and STATUS <> 'REMOVED' "  &
        "order by SORTPRIO" ) ;

  Select_Prices_For_Runner_In_One_Market.Prepare( "select * " &
        "from ARACEPRICES " &
        "where MARKETID = :MARKETID " &
        "and SELECTIONID = :SELECTIONID " &
        "order by PRICETS"  ) ;
                      
  Select_All_Markets.Open_Cursor;
  
  Select_All_Markets_Loop : loop
    Select_All_Markets.Fetch(Eos);
    exit Select_All_Markets_Loop when Eos;
    Select_All_Markets.Get("MARKETID", Data.Marketid); -- Get a new market
    Log("|Start market|" & Data.Marketid);      

    Cnt := 0; --reset
    Market.Marketid := Data.Marketid;
    
    Table_Amarkets.Read(Market, Eos);
    if Eos then  
      Log("| " & Market.Marketid & " |NOT FOUND !");
    end if;
    
    if not Eos and then          -- Must find market
      Market.Numrunners >= 8 then
      Select_Race_Runners_In_One_Market.Set("MARKETID", Data.Marketid);      
      Select_Race_Runners_In_One_Market.Open_Cursor; 
      
      Race_Runners_Loop : loop -- get a new runner in this market
        Select_Race_Runners_In_One_Market.Fetch(Eos3); 
        exit Race_Runners_Loop when Eos3;
        Select_Race_Runners_In_One_Market.Get("SELECTIONID", Data.Selectionid);
        Log("|Start runner|" & Data.Marketid & "|" &  Data.Selectionid'Img);      
        
        
        if  Cnt >= 10 then  -- max 3 lay bet per race
          Log("|" & Data.Marketid & "|" &  Data.Selectionid'Img & "|cnt > 3|" & Cnt'Img);
          exit Race_Runners_Loop;
        end if;
        -------------------------------------------------      
        Select_Prices_For_Runner_In_One_Market.Set("MARKETID", Data.Marketid);
        Select_Prices_For_Runner_In_One_Market.Set("SELECTIONID", Data.Selectionid);
        Select_Prices_For_Runner_In_One_Market.Open_Cursor;
        Prices := Table_Araceprices.Empty_Data;
        
        OK_Starting_Price := False;
        First_Loop := True;
        Bet_Status := No_Bet_Laid;
      
        Loop_Runner_Prices : loop -- get prices for this runner in the race
          Select_Prices_For_Runner_In_One_Market.Fetch(Eos2);
          exit Loop_Runner_Prices when Eos2;
          Prices := Table_Araceprices.Get(Select_Prices_For_Runner_In_One_Market);
          
          if First_Loop then
            First_Loop := False;
            OK_Starting_Price := Prices.Backprice <= Fixed_Type(IA_Max_Start_Price);
            if not OK_Starting_Price then
              Log("|BAD STARTPRICE|" & Table_Araceprices.To_String(Prices));      
              exit Loop_Runner_Prices ;  -- done with this runner in this market !!
            else  
              Log("|OK  STARTPRICE|" & Table_Araceprices.To_String(Prices));      
            end if;
          end if; 
      
          if Prices.Backprice >= Fixed_Type(IA_Lay_At_Price) and then
             Prices.Layprice <= Fixed_Type(IA_Max_Lay_Price) and then
             Prices.Layprice > Fixed_Type(0.0) then             
            Bet_Status := Bet_Laid;
            Cnt := Cnt + 1;
            Log("BET_LAID:       " & Table_Araceprices.To_String(Prices));
            exit Loop_Runner_Prices; -- 1 bet per runner/market only
          end if;
        end loop Loop_Runner_Prices;
        Select_Prices_For_Runner_In_One_Market.Close_Cursor;
      
        if Bet_Status = Bet_Laid then
          Runner.Marketid := Prices.Marketid;
          Runner.Selectionid := Prices.Selectionid;
          Table_Arunners.Read(Runner, Eos);
          if not Eos then
            if Runner.Status(1..6) = "WINNER" then
              Bet_Status := Bet_Lost;
            elsif Runner.Status(1..5) = "LOSER" then
              Bet_Status := Bet_Won;
            else
              Bet_Status := No_Bet_Laid;
              Log("CHANGE_1-BET_NOT_LAID: " & Table_Araceprices.To_String(Prices));
            end if;
          else
            Bet_Status := No_Bet_Laid;
            Log("CHANGE_2-BET_NOT_LAID: " & Table_Araceprices.To_String(Prices));
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
             Log(Bet_Status'Img & "         " & Table_Araceprices.To_String(Prices));
           when Bet_Lost =>  -- A losing lay bet
             Income := 0.0;
             Stake  := Lay_Size * (Prices.Layprice - 1.0);
             Profit := - Stake;
             Log(Bet_Status'Img & "        " & Table_Araceprices.To_String(Prices));
        end case;
        Stats(Bet_Status).Hits := Stats(Bet_Status).Hits + 1;
        Stats(Bet_Status).Profit := Stats(Bet_Status).Profit + Profit;
      
        Global_Profit := Global_Profit + Profit;
        Log("|" & Data.Marketid & "|" &  Data.Selectionid'Img & "|"  & Integer_4(Global_Profit)'Img & " " & Bet_Status'Img);
        Log("|Stop  runner|" & Data.Marketid & "|" &  Data.Selectionid'Img);      

      end loop Race_Runners_Loop;
      Select_Race_Runners_In_One_Market.Close_Cursor; 
    end if;  
    Log("|Stop  market|" & Data.Marketid);      
  end loop Select_All_Markets_Loop;
  Select_All_Markets.Close_Cursor;
  T.Commit ;

  Sql.Close_Session;

  Log("Total profit = " & Integer_4(Global_Profit)'Img);
  for i in Bet_Status_Type'range loop
    Log(i'Img & Stats(i).Hits'Img & Integer_4(Stats(i).Profit)'Img);
  end loop;
  Log("used --max_start_price=" & IA_Max_Start_Price'Img &
    " --lay_at_price=" & IA_Lay_At_Price'Img &
    " --max_lay_price=" & IA_Max_Lay_Price'Img);
exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Lay_During_Race;
