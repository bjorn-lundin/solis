
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Types ; use Types;
with Stacktrace;
with Sql;
--with Table_Arunners;
with Table_Abets;
with Table_Amarkets;
with Table_Apriceshistory;

with Calendar2; use Calendar2;
with Logging; use Logging;
with Utils; use Utils;
--with General_Routines; use General_Routines;
with Bot_Types; use Bot_Types;
with Tics;
with Sim;

procedure Check_Greenup_2 is
   Market_List : Table_Amarkets.Amarkets_List_Pack2.List  ;
   History_List : Table_Apriceshistory.Apriceshistory_List_Pack2.List  ;

  -- Market : Markets.Market_Type;
   History_n,
   History_1: Table_Apriceshistory.Data_Type;
--   Runner : Table_Arunners.Data_Type;

   Select_Markets,
   Select_Selectionid   : Sql.Statement_Type;

   T : Sql.Transaction_Type;
   Selectionid  : Integer_4 := 0;

   Lay_Bet_Name   : constant Bet_Name_Type := "3_HOUNDS_WIN_GREEN_UP_LAY                                                                           ";
   Back_Bet_Name  : constant Bet_Name_Type := "3_HOUNDS_WIN_GREEN_UP_BACK                                                                          ";
   Stop_Loss_Name : constant Bet_Name_Type := "3_HOUNDS_WIN_GREEN_UP_BACK_STOP_LOSS                                                                ";

   Eos : Boolean := False;
   First : Boolean := True;

   Tic_Lay,
   Tic_Back : Integer := 0;

   subtype Delta_Tics_Greenup_Type is Integer range 1 .. 5;

   type Selection_Info_Type is record
     Greenup    : Integer_4 := 0;
     Stop_Loss  : Integer_4 := 0;
     Tot_Num    : Integer_4 := 0;
   end record;

   type Odds_Range_Type is (Invalid,
                            A_01_05,
                            A_06_10,
                            A_11_15,
                            A_16_20,
                            A_21_25,
                            A_26_30,
                            A_31_35,
                            A_36_40,
                            A_41_45,
                            A_46_50);

   Info : array(Delta_Tics_Greenup_Type,Odds_Range_Type) of Selection_Info_Type;
   Odds_Range : Odds_Range_Type ;

   Last_Market_Date : Calendar2.Time_Type := Calendar2.Clock;
   First_Market_Date : Calendar2.Time_Type := (2016,03,15,00,00,00,000);
   Delta_Tics_Stop_Loss : Positive := 15;

  type Bet_Type is record
    Laybet            : Table_Abets.Data_Type := Table_Abets.Empty_Data;
    Greenup_Backbet   : Table_Abets.Data_Type := Table_Abets.Empty_Data;
    Stop_Loss_Backbet : Table_Abets.Data_Type := Table_Abets.Empty_Data;
  end record;
  use type Table_Abets.Data_Type;
  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_Type);

  Bet_List : array (Delta_Tics_Greenup_Type'range) of Bet_List_Pack.List;

  Back_Stake      : constant Bet_Size_Type := 30.0;
  Lay_Stake       : constant Bet_Size_Type := 40.0;
  Stop_Loss_Stake : constant Bet_Size_Type := 40.0;

  Dst_Change : Calendar2.Time_Type := (2016,03,27,03,0,0,0);

  Bet : Bet_Type;

begin
    Log ("Connect db");
    Sql.Connect
     (Host     => "localhost",
      Port     => 5432,
      Db_Name  => "ghd",
      Login    => "bnl",
      Password => "bnl");

    T.Start;

    Select_Markets.Prepare( "select * from AMARKETS where STARTTS > :STARTTS order by STARTTS" );
    Select_Markets.Set("STARTTS", First_Market_Date);

    Select_Selectionid.Prepare("select SELECTIONID from ARUNNERS where MARKETID=:MARKETID");

    Table_Amarkets.Read_List(Select_Markets,Market_List);
    Log("num markets" & Market_List.Length'Img);

    Log("fill map winners ");
    Sim.Fill_Winners_Map(Market_List, Sim.Winners_Map );
    Log("done map winners ");

    M_List_Loop : for M of Market_List loop
      --fix dst/utc
      if M.Startts < Dst_Change then
        M.Startts := M.Startts + (0,1,0,0,0); -- 1 hour
      elsif M.Startts > Dst_Change then
        M.Startts := M.Startts + (0,2,0,0,0); -- 2 hours
      end if;

      Delta_Loop : for Delta_Tics in Delta_Tics_Greenup_Type loop
         Select_Selectionid.Set("MARKETID", M.Marketid);
         Select_Selectionid.Open_Cursor;
         Get_Sel_Id_Loop : loop
            Select_Selectionid.Fetch(Eos);
            exit Get_Sel_Id_Loop when Eos;
            Select_Selectionid.Get("SELECTIONID",Selectionid);
            Log("markets " & M.Marketid & Selectionid'Img & " " & M.Startts.To_String & " Delta_Tics" & Delta_Tics'Img);
            History_List.Clear;
            Sim.Read_Marketid_Selectionid(M.Marketid, Selectionid, History_List);
            First := True;
            H_List_Loop : for H of History_List loop
              History_n := H;
              if H.Backprice > Fixed_Type(1.0) and  --sane data
                 H.Layprice > Fixed_Type(1.0) and
                 H.Layprice < Fixed_Type(1001.0) and
                 H.Layprice < Fixed_Type(1001.0) then
                if First then
                  Odds_Range := Invalid;
                  Bet.Laybet            := Table_Abets.Empty_Data;
                  Bet.Greenup_Backbet   := Table_Abets.Empty_Data;
                  Bet.Stop_Loss_Backbet := Table_Abets.Empty_Data;

                  exit H_List_Loop when History_List.Length < 30;
                  History_1 := H ;
                  First := False;
                  case Integer(History_1.Backprice) is
                    when 01..05 => Odds_Range := A_01_05;
                    when 06..10 => Odds_Range := A_06_10;
                    when 11..15 => Odds_Range := A_11_15;
                    when 16..20 => Odds_Range := A_16_20;
                    when 21..25 => Odds_Range := A_21_25;
                    when 26..30 => Odds_Range := A_26_30;
                    when 31..35 => Odds_Range := A_31_35;
                    when 36..40 => Odds_Range := A_36_40;
                    when 41..45 => Odds_Range := A_41_45;
                    when 46..50 => Odds_Range := A_46_50;
                    when others => Odds_Range := Invalid;
                                   exit H_List_Loop; -- get next selid
                  end case;
                  Tic_Lay  := Tics.Get_Tic_Index(History_1.Layprice);
                  case Odds_Range is
                    when Invalid => null;
                    when others =>
                       Sim.Place_Bet(Bet_Name         => Lay_Bet_Name,
                                     Market_Id        => M.Marketid,
                                     Side             => Lay,
                                     Runner_Name      => (others => '-'),
                                     Selection_Id     => Selectionid,
                                     Size             => Lay_Stake,
                                     Price            => Bet_Price_Type(History_1.Layprice),
                                     Bet_Persistence  => Persist,
                                     Bet_Placed       => History_1.Pricets,
                                     Bet              => Bet.Laybet ) ;
                       Move("M",Bet.Laybet.Status);
                       Move(Odds_Range'Img & '/' & Trim(Delta_Tics'Img,Both) , Bet.Laybet.Reference);
                  end case;
                end if;--first

                exit H_List_Loop when History_n.Backprice < Fixed_Type(1.01) or else History_n.Backprice > Fixed_Type(1000.0);
                Tic_Back := Tics.Get_Tic_Index(History_n.Backprice);
                exit when Tic_Back >= Tic_Lay + Delta_Tics or Tic_Back +15 < Tic_Lay ;
              end if;  --sane date
            end loop H_List_Loop;

            Info(Delta_Tics,Odds_Range).Tot_Num := Info(Delta_Tics,Odds_Range).Tot_Num +1;
            if Tic_Back >= Tic_Lay + Delta_Tics then
              Info(Delta_Tics,Odds_Range).Greenup := Info(Delta_Tics,Odds_Range).Greenup +1;
              Sim.Place_Bet(Bet_Name         => Back_Bet_Name,
                            Market_Id        => M.Marketid,
                            Side             => Back,
                            Runner_Name      => (others => '-'),
                            Selection_Id     => Selectionid,
                            Size             => Back_Stake,
                            Price            => Bet_Price_Type(Tics.Get_Tic_Price(Tic_Lay + Delta_Tics)),
                            Bet_Persistence  => Persist,
                            Bet_Placed       => History_n.Pricets,
                            Bet              => Bet.Greenup_Backbet ) ;
              Move("M",Bet.Greenup_Backbet.Status);
              Move(Odds_Range'Img & '/' & Trim(Delta_Tics'Img,Both) , Bet.Greenup_Backbet.Reference);
              exit Get_Sel_Id_Loop;
            elsif  Tic_Back +15 < Tic_Lay then -- get out
              Info(Delta_Tics,Odds_Range).Stop_Loss := Info(Delta_Tics,Odds_Range).Stop_Loss +1;
              Sim.Place_Bet(Bet_Name         => Stop_Loss_Name,
                            Market_Id        => M.Marketid,
                            Side             => Back,
                            Runner_Name      => (others => '-'),
                            Selection_Id     => Selectionid,
                            Size             => Stop_Loss_Stake,
                            Price            => Bet_Price_Type(Tics.Get_Tic_Price(Tic_Lay - Delta_Tics_Stop_Loss)),
                            Bet_Persistence  => Persist,
                            Bet_Placed       => History_n.Pricets,
                            Bet              => Bet.Stop_Loss_Backbet ) ;
              Move("M",Bet.Stop_Loss_Backbet.Status);
              Move(Odds_Range'Img & '/' & Trim(Delta_Tics'Img,Both) , Bet.Stop_Loss_Backbet.Reference);
              exit Get_Sel_Id_Loop;
            end if;
         end loop Get_Sel_Id_Loop;
         Select_Selectionid.Close_Cursor;
         Bet_List(Delta_Tics).Append(Bet);

         if M.Startts.Day /= Last_Market_Date.Day then
           for d in Delta_Tics_Greenup_Type'range loop
             for o in Odds_Range_Type'range loop
               Log(d'Img & " " & o'Img & " greenup/stoploss/tot/pct grennup/pct stoploss " &
                         Info(d,o).Greenup'Img & "/" &
                         Info(d,o).Stop_Loss'Img & "/" &
                         Info(d,o).Tot_Num'Img & "/" &
                         F8_Image(100.0* Fixed_Type(Info(d,o).Greenup)/Fixed_Type(Info(d,o).Greenup + Info(d,o).Stop_Loss + Info(d,o).Tot_Num )) & "/" &
                         F8_Image(100.0* Fixed_Type(Info(d,o).Stop_Loss)/Fixed_Type(Info(d,o).Greenup + Info(d,o).Stop_Loss + Info(d,o).Tot_Num )));
             end loop;
           end loop;
           Last_Market_Date.Day := M.Startts.Day ;
         end if;
      end loop Delta_Loop;
    end loop M_List_Loop;
    T.Commit ;
    Sql.Close_Session;
    for d in Delta_Tics_Greenup_Type'range loop
      for o in Odds_Range_Type'range loop
        Log(d'Img & " " & o'Img & " greenup/stoploss/tot/pct grennup/pct stoploss " &
                  Info(d,o).Greenup'Img & "/" &
                  Info(d,o).Stop_Loss'Img & "/" &
                  Info(d,o).Tot_Num'Img & "/" &
                  F8_Image(100.0* Fixed_Type(Info(d,o).Greenup)/Fixed_Type(Info(d,o).Greenup + Info(d,o).Stop_Loss + Info(d,o).Tot_Num )) & "/" &
                  F8_Image(100.0* Fixed_Type(Info(d,o).Stop_Loss)/Fixed_Type(Info(d,o).Greenup + Info(d,o).Stop_Loss + Info(d,o).Tot_Num )));
      end loop;
    end loop;


    declare
      Profit : array (Delta_Tics_Greenup_Type'range) of Fixed_Type := (others => 0.0);
    begin
      for i in Delta_Tics_Greenup_Type'range loop
        for b of Bet_List(i) loop
           if Sim.Is_Race_Winner(Marketid => B.Laybet.Marketid, Selectionid => B.Laybet.Selectionid) then
             B.Laybet.Profit := - B.Laybet.Price * (B.Laybet.Size - 1.0);  -- lost laybet
           else
             B.Laybet.Profit := B.Laybet.Size; -- won laybet
           end if;

           if B.Greenup_Backbet.Status(1) = 'M' then
             if Sim.Is_Race_Winner(Marketid => B.Greenup_Backbet.Marketid, Selectionid => B.Greenup_Backbet.Selectionid) then
               B.Greenup_Backbet.Profit := B.Greenup_Backbet.Price * (B.Greenup_Backbet.Size - 1.0);  -- won backbet
             else
               B.Greenup_Backbet.Profit := -B.Greenup_Backbet.Size; -- lost backbet
             end if;
           elsif B.Stop_Loss_Backbet.Status(1) = 'M' then
             if Sim.Is_Race_Winner(Marketid => B.Stop_Loss_Backbet.Marketid, Selectionid => B.Stop_Loss_Backbet.Selectionid) then
               B.Stop_Loss_Backbet.Profit := B.Stop_Loss_Backbet.Price * (B.Stop_Loss_Backbet.Size - 1.0);  -- won backbet
             else
               B.Stop_Loss_Backbet.Profit := -B.Stop_Loss_Backbet.Size; -- lost backbet
             end if;
           end if;
           Profit(i) := B.Laybet.Profit + B.Greenup_Backbet.Profit + B.Stop_Loss_Backbet.Profit;
        end loop;
        Log(i'Img & "profit: " & F8_Image(Profit(i)) & " len:" & Bet_List(i).Length'Img);
      end loop;
    end ;

exception
   when E: others =>
      Stacktrace.Tracebackinfo(E);
end Check_Greenup_2;
