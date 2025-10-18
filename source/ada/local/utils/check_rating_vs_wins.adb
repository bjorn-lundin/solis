--with Ada.Strings;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers;
--with Text_Io;
--with Gnat.Command_Line; use Gnat.Command_Line;
---with GNAT.Strings;

with Sim;
--with Utils; use Utils;
with Types ; use Types;
--with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
with Prices;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Bot_Types;
--with Bot_System_Number;
with Markets;


procedure Check_Rating_Vs_Wins is



 -- Global_Bet_List : Bets.Lists.List;
--  Config           : Command_Line_Configuration;

--  IA_Back_At_Back_Price : aliased Integer := 50;
--  IA_Lay_At_Back_Price  : aliased Integer := 100;
--  IA_Max_Lay_Price      : aliased Integer := 200;

--  Lay_Size  : Fixed_Type := 30.0;
--  Back_Size : Fixed_Type := 1500.0;

  --type Bet_Status_Type is (No_Bet_Laid, Bet_Laid);
  --Bet_Status : Bet_Status_Type := No_Bet_Laid;

--    Global_Min_Backprice1     : constant Fixed_Type := 1.31;
--    Global_Max_Backprice1     : constant Fixed_Type := 1.36;
--    Global_Min_Backprice2     : constant Fixed_Type := 2.5;
--    Global_Max_Backprice2     : constant Fixed_Type := 10.0;
--    Global_Lay_At_Backprice   : constant Fixed_Type := 1.25;
--    Global_Lay_Size           : constant Fixed_Type := 110.0;
--    Global_Back_Size          : constant Fixed_Type := 100.0;

  subtype Rank_Type is Integer range 1 ..3;
  type Race_Type_Type is (Win,Plc);

  Race_Type : Race_Type_Type := Win;

  type Num_Races_Type is array (Race_Type_Type'Range) of Natural ;
  type Num_Races_Won_Type is array (Race_Type_Type'Range, Rank_Type) of Natural;


  type Result_Type is record
    Num_Races     : Num_Races_Type     := (others => 0);
    Num_Races_Won : Num_Races_Won_Type := ((others => 0),(others => 0));
  end record;

  Result : Result_Type;



  Start : Calendar2.Time_Type := Calendar2.Clock;

  function "<" (Left,Right : Prices.Price_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Prices.Lists.Generic_Sorting("<");

  --type Best_Runners_Array_Type is array (1..4) of Price_Histories.Price_History_Type ;
  --Best_Runners      : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);

  --------------------------------------------

 -- Enough_Runners : Boolean := False;
  use type Ada.Containers.Count_Type;

  Day : Time_Type := (2016,03,19,00,00,00,000);
  End_Date : Time_Type := (2019,12,31,00,00,00,000);
  One_Day : Interval_Type := (1,0,0,0,0);
begin

 -- Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

  Log ("Connect db");
  Sql.Connect
    (Host     => "localhost",
     Port     => 5432,
     Db_Name  => "bnl",
     Login    => "bnl",
     Password => "bnl",
     SSL_Mode => "prefer");
  Log ("Connected to db");

  Day_Loop : loop

    exit Day_Loop when Day >  End_Date;
    Sim.Fill_Data_Maps(Day, Bot_Types.Horse, False, False, False);

    Log("start process date " & Day.To_String);

    declare
      Ok : Boolean := True;
      Market : Markets.Market_Type;
      Found : Boolean := False;
    begin
      Log("  -num markets " & Day.To_String & " " & Sim.Market_With_Data_List.Length'Img);

      Loop_Market : for M of Sim.Market_With_Data_List loop

        if M.Markettype(1) = 'W' then
          Race_Type := Win;
        elsif M.Markettype(1) = 'P' then
          Race_Type := Plc;
        else
          raise Constraint_Error with "bad racetype " & M.To_String;
        end if;

        Found := False;
        case Race_Type is
          when Win =>
            Market := M;
            Ok := Market.Marketname_Ok and then Sim.Prices_Map.Length >= 8;
          when Plc =>
            M.Corresponding_Win_Market(Market,Found ); -- check plc market is an ok one
            Ok := Market.Marketname_Ok and then Found and then Sim.Prices_Map.Length >= 8;
            Market := M; -- to get plc-market back
        end case;


        if Ok then

          Result.Num_Races(Race_Type) := Result.Num_Races(Race_Type) +1;

          declare
            Price_Data : array (Rank_Type'Range) of Prices.Price_Type;
            use type Prices.Price_Type;
            Did_Set    : array (Rank_Type'Range) of Boolean;
            The_List : Prices.Lists.List;
          begin
            --find runner ranked 1,2,3
            The_List := Sim.Prices_Map(Market.Marketid).Copy;
            Backprice_Sorter.Sort(The_List);

            for P of The_List loop
              Log("P " & P.To_String);

              Did_Set := (others => False);

              if P.Backprice > 1.0
                and then P.Status(1..6) = "ACTIVE"
                and then Price_Data(1).Backprice = 0.0
                and then Price_Data(2).Backprice = 0.0
                and then Price_Data(3).Backprice = 0.0
              then
                Price_Data(1) := P;
                Did_Set(1) := True;
              end if;

              if P.Backprice > 1.0
                and then P.Status(1..6) = "ACTIVE"
                and then not Did_Set(1)
                and then Price_Data(1).Backprice /= 0.0
                and then Price_Data(2).Backprice = 0.0
                and then Price_Data(3).Backprice = 0.0
              then
                Price_Data(2) := P;
                Did_Set(2) := True;
              end if;

              if P.Backprice > 1.0
                and then P.Status(1..6) = "ACTIVE"
                and then not Did_Set(1)
                and then not Did_Set(2)
                and then Price_Data(1).Backprice /= 0.0
                and then Price_Data(2).Backprice /= 0.0
                and then Price_Data(3).Backprice = 0.0
              then
                Price_Data(3) := P;
                Did_Set(3) := True;
              end if;

              if Price_Data(1) = Price_Data(2) and then Price_Data(1).Backprice > 0.0 then
                Log("Price_Data(1) " & Price_Data(1).To_String);
                raise Constraint_Error with "P(1) = P(2)";
              end if;
              if Price_Data(2) = Price_Data(3) and then Price_Data(2).Backprice > 0.0 then
                Log("Price_Data(2) " & Price_Data(2).To_String);
                raise Constraint_Error with "P(2) = P(3)";
              end if;
              if Price_Data(1) = Price_Data(3) and then Price_Data(1).Backprice > 0.0 then
                Log("Price_Data(1) " & Price_Data(1).To_String);
                raise Constraint_Error with "P(1) = P(3)";
              end if;

              exit when Did_Set(3);

            end loop;

            -- Check winners
            for W of Sim.Winners_Map(Market.Marketid) loop
              for Rank in Rank_Type'Range loop
                if W.Selectionid = Price_Data(Rank).Selectionid then
                  Result.Num_Races_Won(Race_Type,Rank) := Result.Num_Races_Won(Race_Type,Rank) +1;
                end if;
              end loop;
            end loop;

          end;
        end if; -- Marketname_Ok
      end loop Loop_Market;  -- marketid
    end;

    Day := Day + One_Day;
  end loop Day_Loop;

  begin
    for Race_Type in Race_Type_Type'Range loop
      Log("Num_Races " & Race_Type'Img & " " & Result.Num_Races(Race_Type)'Img);
      for Rank in Rank_Type'Range loop
        Log("Num_Races " & Rank'Img & " " & Result.Num_Races_Won(Race_Type,Rank)'Img);
      end loop;
      Log("-----------------------");
    end loop;
  end ;


  Sql.Close_Session;
  Log("Started : " & Start.To_String);
  Log("Done : " & Calendar2.Clock.To_String);
  Logging.Close;


  exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Check_Rating_Vs_Wins ;
