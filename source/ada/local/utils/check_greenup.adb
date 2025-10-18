
with Types ; use Types;
with Stacktrace;
with Sql;
with Table_Arunners;
with Table_Abets;
--with Calendar2; use Calendar2;
with Logging; use Logging;
--with General_Routines; use General_Routines;

procedure Check_Greenup is
   Bet_List : Table_Abets.Abets_List_Pack2.List  ;
   Runner : Table_Arunners.Data_Type;
   T : Sql.Transaction_Type;
   Total_Profit   : Fixed_Type := 0.0;

   type Stats_Type is record
     Hits   : Integer_4 := 0;
     Profit : Fixed_Type   := 0.0;
   end record ;

   type Betting_Type Is (Back_Win, Back_Lose, Lay_Win, Lay_Lose, Back_Removed, Lay_Removed);
   Stats : array (Betting_Type'range) of Stats_Type;
   Eos : Boolean := False;

begin
    Log ("Connect db");
    Sql.Connect
     (Host     => "localhost",
      Port     => 5432,
      Db_Name  => "dry",
      Login    => "bnl",
      Password => "bnl");

      
    T.Start;

    Table_Abets.Read_All(Bet_List);
    Log("num bets" & Bet_List.Length'Img);
     
    for Bet of Bet_List loop
      Runner.Marketid := Bet.Marketid;
      Runner.Selectionid := Bet.Selectionid;
      Runner.Read(Eos);
      if Eos then
        Log("runner is missing " & Runner.To_String);
        exit;
      end if;
      
      if    Bet.Side(1..3) = "LAY" then
        if Runner.Status(1..5) = "LOSER" then
          Bet.Profit := Bet.Sizematched * 0.935;
          Bet.Betwon := True;
          Stats(Lay_Win).Hits   := Stats(Lay_Win).Hits +1;
          Stats(Lay_Win).Profit := Stats(Lay_Win).Profit + Bet.Profit;
          
        elsif Runner.Status(1..7) = "REMOVED" then
          Stats(Lay_Removed).Hits   := Stats(Lay_Removed).Hits +1;
          Stats(Lay_Removed).Profit := 0.0;
          Bet.Betwon := False;
          Bet.Profit := 0.0;
          
        elsif Runner.Status(1..6) = "WINNER" then
          Bet.Profit := - Bet.Sizematched * (Bet.Pricematched -1.0);
          Stats(Lay_Lose).Hits   := Stats(Lay_Lose).Hits +1;
          Stats(Lay_Lose).Profit := Stats(Lay_Lose).Profit + Bet.Profit;
          Bet.Betwon := False;
          
        else
          Log("bad status" & Runner.To_String);
          exit;
        end if;
      elsif Bet.Side(1..4) = "BACK" then
        if Runner.Status(1..5) = "LOSER" then
          Bet.Profit := - Bet.Sizematched ;
          Stats(Back_Lose).Hits   := Stats(Back_Lose).Hits +1;
          Stats(Back_Lose).Profit := Stats(Back_Lose).Profit + Bet.Profit ;
          Bet.Betwon := False;

        elsif Runner.Status(1..7) = "REMOVED" then
          Stats(Back_Removed).Hits   := Stats(Back_Removed).Hits +1;
          Stats(Back_Removed).Profit := 0.0;
          Bet.Betwon := False;
          Bet.Profit := 0.0;
          
        elsif Runner.Status(1..6) = "WINNER" then
          Bet.Profit := Bet.Sizematched * (Bet.Pricematched -1.0);
          Stats(Back_Win).Hits   := Stats(Back_Win).Hits +1;
          Stats(Back_Win).Profit := Stats(Back_Win).Profit + Bet.Profit;
          Bet.Betwon := True;
          
        else
          Log("bad status" & Runner.To_String);
          exit;
        end if;
      
      else
        Log("bad bet type " & Bet.To_String);
        exit;
      end if;
      Bet.Update_Withcheck;

    end loop;    
       
    T.Commit ;
    Sql.Close_Session;
    
    for i in Betting_Type'range loop
      Log(i'Img & " hits " & Stats(i).Hits'Img & " profit " & Integer_4(Stats(i).Profit)'Img);
      Total_Profit := Total_Profit + Stats(i).Profit;     
    end loop;
   
   Log("Total profit = " & Integer_4(Total_Profit)'Img);
exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Check_Greenup;
