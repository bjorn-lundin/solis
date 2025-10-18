with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers;
--with Ada.Containers.Doubly_Linked_Lists;
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
with Utils; use Utils;
with Table_Abets;
--with Tics;
with Sim;

with Prices;
--with Price_Histories;
with Markets;
with Runners;
with Bets;

procedure Bet_At_Start is

  package Ev renames Ada.Environment_Variables;
  use type Rpc.Result_Type;
  use type Ada.Containers.Count_Type;

  Me              : constant String := "Bet_At_Start.";

  Sa_Par_Inifile  : aliased Gnat.Strings.String_Access;
  Cmd_Line        : Command_Line_Configuration;
  -------------------------------------------------------------
  type Market_Type is (Place,Win);

  type Bet_Type is array(Bet_Side_Type'Range,Market_Type'Range) of Bets.Bet_Type;

 -- type Bet_Type is record
 --   Bet  : Bet_Array;
 -- end record;
  use type Table_Abets.Data_Type;
  --package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_Type);
  subtype Delta_Tics_Type is Integer range 0 .. 350;

  --B_Place : aliased Boolean := False;
  --B_Nolay : aliased Boolean := False;

  -----------------------------------------------------------------
  procedure Check_Bet ( R : in Runners.Runner_Type;
                        B : in out Bets.Bet_Type) is
  begin
    if B.Status(1) = 'M' then
      if B.Side(1..4) = "BACK" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := True;
          B.Profit := (B.Size * (B.Price - 1.0)) * (1.0 - Bets.Commission);
        elsif R.Status(1..5) = "LOSER" then
          B.Betwon := False;
          B.Profit := -B.Size;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
          B.Profit := 0.0;
        end if;
      elsif B.Side(1..3) = "LAY" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := False;
          B.Profit := -B.Size * (B.Price - 1.0);
        elsif R.Status(1..5) = "LOSER" then
          B.Profit := B.Size * (1.0 - Bets.Commission);
          B.Betwon := True;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
          B.Profit := 0.0;
        end if;
      end if;
      B.Insert;
    end if;
  end Check_Bet;

  -----------------------------------------------------------------

  procedure Run(Price_Data : in Prices.Price_Type;
                Delta_Tics : in Delta_Tics_Type;
                Size       : in Bet_Size_Type) is


    Market                 : array (Market_Type'Range) of Markets.Market_Type;
    Eos                    : array (Market_Type'Range) of Boolean := ( others => False);
--    Price_During_Race_List : array (Market_Type'Range) of Price_Histories.Lists.List;
 --   History_Exists         : array (Market_Type'Range) of Boolean := (others => False);
    Runner                 : array (Market_Type'Range) of Runners.Runner_Type;--table_Arunners.Data_Type;
--    Tic_Lay                : Integer := 0;
    Bet                    : Bet_Type;
    Betname                : String_Object;

    --Ln                     : Betname_Type := (others => ' ');
    Bn                     : Betname_Type := (others => ' ');
 --   B_Price                : Fixed_Type := 0.0;
    Found_Place_Market     : Boolean := False;

    Betname_Prefix         : constant String := "BET_AT_START";

  begin
   -- Log(Me & "Run", "start");

    -- Log(Me & "Run", "Treat market: " &  Price_Data.Marketid);
    Market(Win).Marketid := Price_Data.Marketid;
    Market(Win).Read(Eos(Win));
    if Eos(Win) then
      Log(Me & "Run", "no win market found");
      return;
    end if;

    Market(Win).Corresponding_Place_Market(Place_Market => Market(Place),
                                           Found        => Found_Place_Market);


    Runner(Win).Marketid    := Market(Win).Marketid;
    Runner(Win).Selectionid := Price_Data.Selectionid;
    Runner(Win).Read(Eos(Win));

    if Runner(Win).Status(1..7) = "REMOVED" then
      Log(Me & "Run", "runner removed " & Runner(Win).To_String);
      return;
    end if;

    for M in Market_Type'Range loop
      for S in Bet_Side_Type'Range loop
        declare
          Price       : Bet_Price_Type := 0.0;
          Place_Price : Prices.Price_Type;
        begin
          case M is
            when Win   =>
              case S is
                when Lay  => Price := Bet_Price_Type(Price_Data.Layprice);
                when Back => Price := Bet_Price_Type(Price_Data.Backprice);
              end case;

            when Place =>
              if not Found_Place_Market then
                goto Next; -- at end of loop Markettype
              else
                Runner(Place).Marketid    := Market(Place).Marketid;
                Runner(Place).Selectionid := Price_Data.Selectionid;
                Runner(Place).Read(Eos(Place));
              end if;

              if Eos(Place) then
                goto Next; -- at end of loop Markettype
              end if;

              Place_Price.Marketid    := Runner(Place).Marketid;
              Place_Price.Selectionid := Runner(Place).Selectionid;
              Place_Price.Read(Eos(Place));
              if not Eos(Place) then
                case S is
                  when Lay  => Price := Bet_Price_Type(Place_Price.Layprice);
                  when Back => Price := Bet_Price_Type(Place_Price.Backprice);
                end case;
              else
                goto Next; -- at end of loop Markettype
              end if;
          end case;

          --always set WIN-backprice in name
          if Delta_Tics < Delta_Tics_Type(10) then
            if Price_Data.Backprice < 10.0 then
              Betname.Set(Betname_Prefix & "_00" & Trim(Delta_Tics'Img,Both) & "_0" & F8_Image(Price_Data.Backprice));
            else
              Betname.Set(Betname_Prefix & "_00" & Trim(Delta_Tics'Img,Both) & "_" & F8_Image(Price_Data.Backprice));
            end if;

          elsif Delta_Tics < Delta_Tics_Type(100) then
            if Price_Data.Backprice < 10.0 then
              Betname.Set(Betname_Prefix & "_0" & Trim(Delta_Tics'Img,Both) & "_0" & F8_Image(Price_Data.Backprice));
            else
              Betname.Set(Betname_Prefix & "_0" & Trim(Delta_Tics'Img,Both) & "_" & F8_Image(Price_Data.Backprice));
            end if;

          else
            if Price_Data.Backprice < 10.0 then
              Betname.Set(Betname_Prefix & "_" & Trim(Delta_Tics'Img,Both) & "_0" & F8_Image(Price_Data.Backprice));
            else
              Betname.Set(Betname_Prefix & "_" & Trim(Delta_Tics'Img,Both) & "_" & F8_Image(Price_Data.Backprice));
            end if;
          end if;

          Move(M'Img & '_' & S'Img & '_' & Betname.Fix_String,Bn);
          Sim.Place_Bet(Bet_Name         => Bn,
                        Market_Id        => Market(M).Marketid,
                        Side             => S,
                        Runner_Name      => Runner(M).Runnernamestripped,
                        Selection_Id     => Price_Data.Selectionid,
                        Size             => Size,
                        Price            => Price,
                        Bet_Persistence  => Persist,
                        Bet_Placed       => Price_Data.Pricets,
                        Bet              => Bet(S,M) ) ;
          Move("M",Bet(S,M).Status);
          Check_Bet(Runner(M), Bet(S,M));
        end ;
      end loop;
      <<Next>>
    end loop;

  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
  ------------------------------ main start -------------------------------------
  Current_Date : Calendar2.Time_Type := Calendar2.Clock;

 -- Sa_Min_Backprice : aliased Gnat.Strings.String_Access;
--  Sa_Max_Backprice : aliased Gnat.Strings.String_Access;
  Sa_Logfilename  : aliased Gnat.Strings.String_Access;
  Ia_Min_Tic      : aliased Integer := 1;
  Ia_Max_Tic      : aliased Integer := 1;


  Size             : constant Bet_Size_Type := 100.0;
  Backprice_High   : Fixed_Type := 100.0;
  Backprice_Low    : Fixed_Type := 1.01;



begin

--    Define_Switch
--      (Cmd_Line,
--       Sa_Min_Backprice'Access,
--       Long_Switch => "--min_lay=",
--       Help        => "Min layprice");
--
--    Define_Switch
--      (Cmd_Line,
--       Sa_Max_Backprice'Access,
--       Long_Switch => "--max_lay=",
--       Help        => "Min layprice");
--
--    Define_Switch
--      (Cmd_Line,
--       Ia_Min_Tic'Access,
--       Long_Switch => "--min_tic=",
--       Help        => "min tic");
--
--    Define_Switch
--      (Cmd_Line,
--       Ia_Max_Tic'Access,
--       Long_Switch => "--max_tic=",
--       Help        => "max tic");

  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Define_Switch
    (Cmd_Line,
     Sa_Par_Inifile'Access,
     Long_Switch => "--inifile=",
     Help        => "use alternative inifile");

--    Define_Switch
--      (Cmd_Line,
--       B_Place'Access,
--       Long_Switch => "--place",
--       Help        => "back place market instead");
--
--    Define_Switch
--      (Cmd_Line,
--       B_Nolay'Access,
--       Long_Switch => "--nolay",
--       Help        => "do not actually make a laybet");

  Getopt (Cmd_Line);  -- process the command line

--    if Sa_Min_Backprice.all = "" then
--      raise Constraint_Error with "no min-back-price set";
--    elsif Sa_Max_Backprice.all = "" then
--      raise Constraint_Error with "no max-back-price set";
--    elsif Sa_Logfilename.all = "" then
--      raise Constraint_Error with "no log file name set";
--    elsif Ia_Min_Tic = 0 then
--      raise Constraint_Error with "no min-tic set";
--    elsif Ia_Max_Tic = 0  then
--      raise Constraint_Error with "no max-tic set";
--    end if;
  if Sa_Logfilename.all = "" then
    raise Constraint_Error with "no log file name set";
  end if;

--  Log(Ia_Min_Tic'Img & Ia_Max_Tic'Img & " '" & Sa_Min_Backprice.all & "' '" & Sa_Max_Backprice.all & "'");



  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","bet_at_start");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("local", "host", ""),
     Port     => Ini.Get_Value("local", "port", 5432),
     Db_Name  => Ini.Get_Value("local", "name", ""),
     Login    => Ini.Get_Value("local", "username", ""),
     Password =>Ini.Get_Value("local", "password", ""));
  Log(Me, "db Connected");

 --Backprice_High := Fixed_Type'Value(Sa_Max_Backprice.all);
  --BAckprice_Low  := Fixed_Type'Value(Sa_Min_Backprice.all);

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
           --       "and E.EVENTTYPEID = 7 " &
                  "and P.LAYPRICE <= :MAX_LAYPRICE " &
                --  "and P.LAYPRICE >= :MIN_LAYPRICE " &
                  "order by M.STARTTS, P.MARKETID, P.SELECTIONID ");
    Stm.Set("MAX_LAYPRICE",100.0);
    Prices.Read_List(Stm, Price_List);
    T.Commit;

--    Log(Backprice_Low'Img & " " & Backprice_High'Img & " " & Price_List.Length'Img);

    begin
      for Price of Price_List loop -- all runners in race
        if Price.Pricets.Day /= Current_Date.Day then
          Log(Me, "start Treat date: " & Current_Date.String_Date_Iso );
          Current_Date := Price.Pricets;
        end if;

        if Backprice_Low <= Price.Backprice and then Price.Backprice <= Backprice_High and then
           -- max diff 10% between back and layodds for the winner market
           Price.Layprice >= 1.0 and then Fixed_Type(Price.Backprice / Price.Layprice) <= Fixed_Type(1.1) then
          T.Start;
          --for Dtg in Delta_Tics_Type'Range loop
          for Dtg in Ia_Min_Tic .. Ia_Max_Tic loop
       --     Log(Me, "start Treat price: " & Dtg'Img  & " " & Price.To_String );
            Run(Price_Data => Price,
                Delta_Tics => Dtg,
                Size   => Size);
       --     Log(Me, "stop  Treat price: " & Dtg'Img  & " " & Price.To_String );
          end loop;
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
end Bet_At_Start;
