with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
with Rpc;
with Lock ;
with Ini;
with Logging; use Logging;
with Markets;
with Table_Apriceshistory; -- Needed for not Oo Routines
with Prices;
with Price_Histories;
with Bot_Svn_Info;
with Bets;
with Runners;
with Tics;

with Sim;

procedure Greenup_Lay_First is

--  Bad_Mode : exception;
  package EV renames Ada.Environment_Variables;
  use type Rpc.Result_Type;
  use type Ada.Containers.Count_Type;

  Me              : constant String := "Greenup_Lay_First.";

  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Cmd_Line        : Command_Line_Configuration;
  -------------------------------------------------------------
  type Bet_Record is record
    Laybet  : Bets.Bet_Type := Bets.Empty_Data;
    Backbet : Bets.Bet_Type := Bets.Empty_Data;
  end record;
  use type Bets.Bet_Type;
--  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_Type);
--  Bet_List : Bet_List_Pack.List;
  subtype Delta_Tics_Type is Integer range 1 .. 50;
--  Bet_List : array (Delta_Tics_Type'range) of Bet_List_Pack.List;

  Back_Stake  : constant Bet_Size_Type := 30.0;
  Lay_Stake   : constant Bet_Size_Type := 40.0;

  subtype Max_Layprice_Type is Integer_4 range 120 .. 140;
  subtype Min_Layprice_Type is Integer_4 range  60 .. 100;

  -----------------------------------------------------------------
  procedure Check_Bet ( R : in Runners.Runner_Type;
                        B : in out Bets.Bet_Type) is
  begin
    if R.Status(1..7) = "REMOVED" then
      return;
    end if;
    if B.Side(1..4) = "BACK" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := True;
          B.Profit := Fixed_Type(Back_Stake) * (B.Price - 1.0);
        elsif R.Status(1..5) = "LOSER" then
          B.Betwon := False;
          B.Profit := Fixed_Type(-Back_Stake);
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
    elsif B.Side(1..3) = "LAY" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := False;
          B.Profit := Fixed_Type(-Lay_Stake) * (B.Price - 1.0);
        elsif R.Status(1..5) = "LOSER" then
          B.Profit := Fixed_Type(Lay_Stake);
          B.Betwon := True;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
    end if;
    B.Insert;
  end Check_Bet;

  -----------------------------------------------------------------

  procedure Run(Price_Data : in Prices.Price_Type;
                Delta_Tics : in Integer;
                Min_Layprice : in Min_Layprice_Type;
                Max_Layprice : in Max_Layprice_Type ) is

    Market    : Markets.Market_Type;
    Eos               : Boolean := False;
    Price_During_Race_List : Price_Histories.Lists.List;
--    type Greenup_Result_Type is (None, Ok, Fail_Runner_Won, Fail_Runner_Lost );
--    Greenup_Result : Greenup_Result_Type := Greenup_Result_Type'first;
    Runner : Runners.Runner_Type;
 --   Bad_Data : exception;
    Tic_Lay : Integer := 0;
    Bet : Bet_Record;
    Lay_Bet_Name  : constant Betname_Type := "1_HOUNDS_WIN_GREEN_UP_LAY                                                                           ";
    Back_Bet_Name : constant Betname_Type := "1_HOUNDS_WIN_GREEN_UP_BACK                                                                          ";
  begin
   -- Log(Me & "Run", "Treat market: " &  Price_Data.Marketid);
    Market.Marketid := Price_Data.Marketid;
   -- Market.Read(Eos);
   --
   -- if not Eos then
   --   if Market.Markettype(1..3) /= "WIN"  then
   --     Log(Me & "Run", "not a WIN market: " &  Price_Data.Marketid);
   --     return;
   --   else
   --     Event.Eventid := Market.Eventid;
   --     Event.Read(Eos);
   --     if not Eos then
   --       if Event.Eventtypeid /= Integer_4(4339) then
   --         Log(Me & "Run", "not a GH market: " &  Price_Data.Marketid);
   --         return;
   --       end if;
   --     else
   --       Log(Me & "Run", "no event found");
   --       return;
   --     end if;
   --   end if;
   --
   --   --if Market.Numrunners < Integer_4(6) then
   --   --  Log(Me & "Run", "less than 6 runners");
   --   --  return;
   --   --end if;
   --
   -- else
   --   Log(Me & "Run", "no market found");
   --   return;
   -- end if;

    if not Table_Apriceshistory.Is_Existing_I1(Marketid => Market.Marketid) then
      Log(Me & "Run", "no Apriceshistory found");
      return;
    end if;

    --Log(Me & "Run", "market found  " & Market.To_String);
    Sim.Read_Marketid_Selectionid(Marketid    =>  Market.Marketid,
                                  Selectionid => Price_Data.Selectionid,
                                  Animal      => Bot_Types.Horse,
                                  List        => Price_During_Race_List) ;
    --Greenup_Result := None;

    Runner.Marketid := Price_Data.Marketid;
    Runner.Selectionid := Price_Data.Selectionid;
    Runner.Read(Eos);

    if Price_During_Race_List.Length > 400 then
      if Eos then
       Log(Me & "Run", "no runner found found  " & Runner.To_String);
      end if;

      Sim.Place_Bet(Bet_Name         => Lay_Bet_Name,
                    Market_Id        => Market.Marketid,
                    Side             => Lay,
                    Runner_Name      => Runner.Runnernamestripped,
                    Selection_Id     => Price_Data.Selectionid,
                    Size             => Lay_Stake,
                    Price            => Bet_Price_Type(Price_Data.Layprice),
                    Bet_Persistence  => Persist,
                    Bet_Placed       => Price_Data.Pricets,
                    Bet              => Bet.Laybet ) ;
      Bet.Laybet.Status(1) := 'M';
      Move(Max_Layprice'Img & '/' & Trim(Min_Layprice'Img,Both), Bet.Laybet.Reference);
      Bet.Laybet.Betmode := Integer_4(Delta_Tics);
      Check_Bet(Runner, Bet.Laybet);

      for Race_Data of Price_During_Race_List loop
        if Price_Data.Backprice > Fixed_Type(0.0) and then Price_Data.Layprice > Fixed_Type(0.0) then   -- must be valid
          if Race_Data.Pricets >= Price_Data.Pricets then   -- must be later in time
            if Price_Data.Selectionid = Race_Data.Selectionid then -- same dog
              Tic_Lay := Tics.Get_Tic_Index(Price_Data.Layprice);
              if Race_Data.Backprice >= Tics.Get_Tic_Price(Tic_Lay + Delta_Tics) then -- and backprice is 1 tic higher that laid price
               -- Greenup_Result := Ok;

                Sim.Place_Bet(Bet_Name         => Back_Bet_Name,
                              Market_Id        => Market.Marketid,
                              Side             => Back,
                              Runner_Name      => Runner.Runnernamestripped,
                              Selection_Id     => Price_Data.Selectionid,
                              Size             => Back_Stake,
                              Price            => Bet_Price_Type(Tics.Get_Tic_Price(Tic_Lay + Delta_Tics)),
                              Bet_Persistence  => Persist,
                              Bet_Placed       => Race_Data.Pricets,
                              Bet              => Bet.Backbet ) ;
                Bet.Backbet.Status(1) := 'M';
                Move(Max_Layprice'Img & '/' & Trim(Min_Layprice'Img,Both), Bet.Backbet.Reference);
                Bet.Backbet.Betmode := Integer_4(Delta_Tics);
                Check_Bet(Runner, Bet.Backbet);
                exit;
              end if;
            end if;
          end if;
        end if;
      end loop ;
--      Bet_List(Delta_Tics).Append(Bet);
      --Bet_List.Append(Bet);
--      declare
--        T : Sql.Transaction_Type;
--      begin
--        T.Start;
--        Log(Me & "insert laybet " & Bet.Laybet.To_String);
--        Bet.Laybet.Insert;
--        if Bet.Backbet.Betid > 0 then
--          Log(Me & "insert backbet " & Bet.Backbet.To_String);
--          Bet.Backbet.Insert;
--        end if;
--        T.Commit;
--      end;

    else
      Log(Me & "not enough data for runner" & Price_During_Race_List.Length'Img, Price_Data.To_String);
    end if;
  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
------------------------------ main start -------------------------------------
begin

   Define_Switch
     (Cmd_Line,
      Sa_Par_Inifile'access,
      Long_Switch => "--inifile=",
      Help        => "use alternative inifile");

  Getopt (Cmd_Line);  -- process the command line

  Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
        (Host     => Ini.Get_Value("database_ghd", "host", ""),
         Port     => Ini.Get_Value("database_ghd", "port", 5432),
         Db_Name  => Ini.Get_Value("database_ghd", "name", ""),
         Login    => Ini.Get_Value("database_ghd", "username", ""),
         Password =>Ini.Get_Value("database_ghd", "password", ""));
  Log(Me, "db Connected");


  declare
    Stm : Sql.Statement_Type;
    T   : Sql.Transaction_Type;
    Price_List  : Prices.Lists.List;
  begin
    T.Start;
    Stm.Prepare(
     "select P.* " &
     "from APRICES P, AMARKETS M, AEVENTS E " &
     "where E.EVENTID=M.EVENTID " &
     "and M.MARKETTYPE = 'WIN' " &
     "and E.COUNTRYCODE in ('GB','IE') " &
     "and P.MARKETID = M.MARKETID " &
     "and E.EVENTTYPEID = 4339 " &
   --  "and P.LAYPRICE <= :MAX_LAYPRICE " &
   --  "and P.LAYPRICE >= :MIN_LAYPRICE " &
     "order by M.STARTTS, P.MARKETID, P.SELECTIONID ");
    Prices.Read_List(Stm, Price_List);
    T.Commit;

    for Max_Layprice in Max_Layprice_Type'range loop
      for Min_Layprice in Min_Layprice_Type'range loop
        declare
          Layprice_High : Fixed_Type := Fixed_Type(Max_Layprice)/10.0;
          Layprice_Low  : Fixed_Type := Fixed_Type(Min_Layprice)/10.0;
        begin
          T.Start;
          for Price of Price_List loop
            if Price.Layprice <= Layprice_High and then
               Price.Layprice >= Layprice_Low then
              Log(Me & "Run", "Treat market:" & Min_Layprice'Img & " " & Max_Layprice'Img & " " & Price.To_String );
              for Dt in Delta_Tics_Type'range loop
                Run(Price, Dt, Min_Layprice, Max_Layprice);
              end loop;
            end if;
          end loop;
          T.Commit;
        end;
      end loop;
    end loop;
  end;
  --declare
  --  Profit : array (Bet_Side_Type'range) of Fixed_Type := (others => 0.0);
  --  Count  : array (Bet_Side_Type'range) of Natural := (others => 0);
  --begin
  --  for Bet of Bet_List loop
  --    if Bet.Laybet.Status(1) ='M' then
  --      Profit(Lay) := Profit(Lay) + Bet.Laybet.Profit;
  --      Count(Lay) :=  Count(Lay)  + 1;
  --    end if;
  --    if Bet.Backbet.Status(1) ='M' then
  --      Profit(Back) := Profit(Back) + Bet.Backbet.Profit;
  --      Count(Back) := Count(Back) + 1;
  --    end if;
  --  end loop;
  --
  --  Log("Result",  Max_Layprice'Img & " Profit(Lay)"  & Integer(Profit(Lay))'Img);
  --  Log("Result",  Max_Layprice'Img & " Profit(Lay)"  & Integer(Profit(Lay))'Img);
  --  Log("Result",  Max_Layprice'Img & " Profit(Lay)"  & Integer(Profit(Lay))'Img);
  --  Log("Result",  Max_Layprice'Img & " Profit(Back)" & Integer(Profit(Back))'Img);
  --  Log("Result",  Max_Layprice'Img & " Count(Lay)"  & Count(Lay)'Img);
  --  Log("Result",  Max_Layprice'Img & " Count(Back)" & Count(Back)'Img);
  --  Log("Result",  Max_Layprice'Img & " Profit total" & Integer(Profit(Back) + Profit(Lay))'Img);
  --end ;


  Log(Me, "Close Db");
  Sql.Close_Session;
  Logging.Close;
  --Posix.Do_Exit(0); -- terminate

exception
  when Lock.Lock_Error =>
    Log(Me, "lock error, exit");
    Logging.Close;
   -- Posix.Do_Exit(0); -- terminate
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

    Log(Me, "Closed log and die");
    Logging.Close;
   -- Posix.Do_Exit(0); -- terminate
end Greenup_Lay_First;
