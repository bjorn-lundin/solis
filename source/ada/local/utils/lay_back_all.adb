with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
with Rpc;
--with Lock ;
with Ini;
with Logging; use Logging;
with Bot_Svn_Info;
with Table_Abets;
with Sim;

with Prices;
with Price_Histories;
with Markets;
with Runners;
with Bets;

procedure Lay_Back_All is

  package Ev renames Ada.Environment_Variables;
  use type Rpc.Result_Type;
  use type Ada.Containers.Count_Type;

  Me              : constant String := "Lay_Back_All.";

  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Cmd_Line        : Command_Line_Configuration;
  -------------------------------------------------------------
  type Bet_Type is record
    Laybet    : Bets.Bet_Type;
    Backbet   : Bets.Bet_Type;
  end record;
  use type Table_Abets.Data_Type;
  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_Type);


  -----------------------------------------------------------------
  procedure Check_Bet ( R : in Runners.Runner_Type;
                        B : in out Bets.Bet_Type) is
  begin
    if B.Status(1) = 'M' then
      if B.Side(1..4) = "BACK" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := True;
          B.Profit := B.Size * (B.Price - 1.0);
        elsif R.Status(1..5) = "LOSER" then
          B.Betwon := False;
          B.Profit := -B.Size;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
      elsif B.Side(1..3) = "LAY" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := False;
          B.Profit := -B.Size * (B.Price - 1.0);
        elsif R.Status(1..5) = "LOSER" then
          B.Profit := B.Size;
          B.Betwon := True;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
      end if;
      B.Insert;
    end if;
  end Check_Bet;

  -----------------------------------------------------------------

  procedure Run(Price_Data : in Prices.Price_Type;
                Lay_Size   : in Bet_Size_Type) is

    Market                 : Markets.Market_Type;
    Eos                    : Boolean := False;
    Price_During_Race_List : Price_Histories.Lists.List;
    Runner                 : Runners.Runner_Type;--table_Arunners.Data_Type;
    Bet                    : Bet_Type;
    Lay_Bet_Name           : String_Object;
    Back_Bet_Name          : String_Object;

    Back_Size              : Bet_Size_Type := 0.0;
    Ln                     : Betname_Type := (others => ' ');
    Bn                     : Betname_Type := (others => ' ');
    Reference              : String(1..30) := (others  => ' ');

  begin
    Log(Me & "Run", "start");

    Lay_Bet_Name.Set("LAY_BACK_ALL");
    Back_Bet_Name.Set("LAY_BACK_ALL");

    -- Log(Me & "Run", "Treat market: " &  Price_Data.Marketid);
    Market.Marketid := Price_Data.Marketid;
    Market.Read(Eos);
    if Eos then
      Log(Me & "Run", "no market found");
      return;
    end if;

    -- Log(Me & "Run", "Market: " & Market.To_String);
    Sim.Read_Marketid_Selectionid(Marketid    => Market.Marketid,
                                  Selectionid => Price_Data.Selectionid,
                                  Animal      => Horse,
                                  List        => Price_During_Race_List) ;

    Runner.Marketid := Price_Data.Marketid;
    Runner.Selectionid := Price_Data.Selectionid;
    Runner.Read(Eos);

    if Price_During_Race_List.Length > 80 then
      if Eos then
        Log(Me & "Run", "no runner found found  " & Runner.To_String);
        return;
      end if;

      if Runner.Status(1..7) = "REMOVED" then
        Log(Me & "Run", "runner removed " & Runner.To_String);
        return;
      end if;

      Move(Lay_Bet_Name.Fix_String,Ln);
      Sim.Place_Bet(Bet_Name         => Ln,
                    Market_Id        => Market.Marketid,
                    Side             => Lay,
                    Runner_Name      => Runner.Runnernamestripped,
                    Selection_Id     => Price_Data.Selectionid,
                    Size             => Lay_Size,
                    Price            => Bet_Price_Type(Price_Data.Layprice),
                    Bet_Persistence  => Persist,
                    Bet_Placed       => Price_Data.Pricets,
                    Bet              => Bet.Laybet ) ;
      Move("M",Bet.Laybet.Status);
      Check_Bet(Runner, Bet.Laybet);
      Back_Size := Lay_Size ;

      Move(Back_Bet_Name.Fix_String,Bn);

      Sim.Place_Bet(Bet_Name         => Bn,
                    Market_Id        => Market.Marketid,
                    Side             => Back,
                    Runner_Name      => Runner.Runnernamestripped,
                    Selection_Id     => Price_Data.Selectionid,
                    Size             => Back_Size,
                    Price            => Bet_Price_Type(Price_Data.Backprice),
                    Bet_Persistence  => Persist,
                    Bet_Placed       => Price_Data.Pricets,
                    Bet              => Bet.Backbet ) ;
      Move("M",Bet.Backbet.Status);
      Move(Reference,Bet.Backbet.Reference);

      -- see if we meet stop_loss or greenup
      --there is no delay here since bet is placed in beginning of race
--        for Race_Data of Price_During_Race_List loop
--          if Race_Data.Backprice > Fixed_Type(0.0) and then Race_Data.Layprice > Fixed_Type(0.0) and then   -- must be valid
--             Race_Data.Backprice < Fixed_Type(1000.0) and then Race_Data.Layprice < Fixed_Type(1000.0) then   -- must be valid
--            if Race_Data.Pricets >= Price_Data.Pricets then
--                if Race_Data.Backprice >= Bet.Backbet.Price then -- a match
--                  Move("M",Bet.Backbet.Status);
--                  Move(F8_Image(Bet.Backbet.Price), Bet.Backbet.Inststatus);
--                  Bet.Backbet.Price := Race_Data.Backprice;
--                  exit;
--                  --                elsif Race_Data.Backprice <= Bet.Stop_Loss_Backbet.Price then -- a match
--                  --                  Move("M",Bet.Stop_Loss_Backbet.Status);
--                  --                  exit;
--                end if;
--            end if;
--          end if;
--        end loop;
      Check_Bet(Runner, Bet.Backbet);
    else
      Log(Me & "not enough data for runner" & Price_During_Race_List.Length'Img, Price_Data.To_String);
    end if;
  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
  ------------------------------ main start -------------------------------------
  Current_Date : Calendar2.Time_Type := Calendar2.Clock;

  Sa_Min_Layprice : aliased Gnat.Strings.String_Access;
  Sa_Max_Layprice : aliased Gnat.Strings.String_Access;

  Lay_Size        : constant Bet_Size_Type := 100.0;
  Layprice_High   : Fixed_Type := 0.0;
  Layprice_Low    : Fixed_Type := 0.0;



begin


  Define_Switch
    (Cmd_Line,
     Sa_Min_Layprice'Access,
     Long_Switch => "--min_lay=",
     Help        => "Min layprice");


  Define_Switch
    (Cmd_Line,
     Sa_Max_Layprice'Access,
     Long_Switch => "--max_lay=",
     Help        => "Min layprice");

  Define_Switch
    (Cmd_Line,
     Sa_Par_Inifile'Access,
     Long_Switch => "--inifile=",
     Help        => "use alternative inifile");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","lay_back_all");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/lay_back_all.log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database_home", "host", ""),
     Port     => Ini.Get_Value("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value("database_home", "name", ""),
     Login    => Ini.Get_Value("database_home", "username", ""),
     Password =>Ini.Get_Value("database_home", "password", ""));
  Log(Me, "db Connected");

  Layprice_High := 100.00;
  Layprice_Low  :=   1.01;

  declare
    Stm         : Sql.Statement_Type;
    T           : Sql.Transaction_Type;
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
                  "and E.EVENTTYPEID = 7 " &
                  "and P.LAYPRICE <= :MAX_LAYPRICE " &
                --  "and P.LAYPRICE >= :MIN_LAYPRICE " &
                  "order by M.STARTTS, P.MARKETID, P.SELECTIONID ");
    Stm.Set("MAX_LAYPRICE",100.0);
    Prices.Read_List(Stm, Price_List);
    T.Commit;

    Log(Layprice_Low'Img & " " & Layprice_High'Img & " " & Price_List.Length'Img);

    begin
      for Price of Price_List loop -- all runners in race
        if Price.Pricets.Day /= Current_Date.Day then
          Log(Me, "start Treat date: " & Current_Date.String_Date_Iso );
          Current_Date := Price.Pricets;
        end if;

        if Layprice_Low <= Price.Layprice and then Price.Layprice <= Layprice_High then
          T.Start;
          --for Dtg in Delta_Tics_Type'Range loop
            Log(Me, "start Treat price: " & Price.To_String );
            Run(Price_Data => Price,
                Lay_Size   => Lay_Size);
          T.Commit;
        end if;
      end loop;
    end;
  end;

  Log(Me, "Close Db");
  Sql.Close_Session;
  Logging.Close;

exception
--    when Lock.Lock_Error =>
--      Log(Me, "lock error, exit");
--      Logging.Close;

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
end Lay_Back_All;
