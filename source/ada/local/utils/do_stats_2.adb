--with Ada.Strings; use Ada.Strings;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;

with Sim;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Text_Io;
with Price_Histories; use Price_Histories;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Markets;
with Bot_Svn_Info;
with Ini;
--with Ada.Text_IO;
--with Ada.Containers.Hashed_Maps;
--with Ada.Strings.Hash;
with Prob_Headers;
with Prob_Rows;
with Bot_System_Number;

procedure Do_Stats_2 is
  -- use type Ada.Containers.Count_Type;

  package Ev renames Ada.Environment_Variables;

  --    subtype Key is String(1..7);
  --
  --    package Odds_Maps is new Ada.Containers.Hashed_Maps
  --      (Key,
  --       Natural,
  --       Ada.Strings.Hash,
  --       "=",
  --       "=");
  type Market_Type is (Place, Winner);
  -- type Probh_Type is array (Market_Type'Range) of Prob_Headers.Prob_Headers_Type;
  subtype Probh_Type is Prob_Headers.Prob_Headers_Type;
  subtype Runner_Num_Type is Integer range 1 ..4 ;
  type Probr_Type is array (Market_Type'Range, Runner_Num_Type'Range ) of Prob_Rows.Prob_Rows_Type;

  --------------------------------------------------------------------------

  function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  type Best_Runners_Array_Type is array (1..12) of Price_Histories.Price_History_Type;


  procedure Treat(Market           : in out Markets.Market_Type;
                  Bra              : in Best_Runners_Array_Type ;
                  Max_Leader_Price : in Price_Type;
                  Probh            : in out Probh_Type;
                  Probr            : in out Probr_Type;
                  Nexttime         : in out Calendar2.Time_Type;
                  Done             : out Boolean) is


    Place_Marketid : Marketid_Type := (others => ' ');
    Placemarket_Exists : Boolean := False;
  begin
    Done := False;


    if Nexttime = Calendar2.Time_Type_First then

      if Fixed_Type(1.0) <  Bra(1).Backprice and then  -- sanity
        Bra(1).Backprice <= Max_Leader_Price then

        begin
          Place_Marketid :=  Sim.Win_Place_Map(Bra(1).Marketid);
          Placemarket_Exists := True;
        exception
          when others => Place_Marketid := "notexisting" ;
        end;

        declare
          Wonrace      : array(Runner_Num_Type'Range) of Boolean := (others => False);
          Placedrace   : array(Runner_Num_Type'Range) of Boolean := (others => False);
          Bet_Id       : Integer_8 := 0;
        begin
          for I in Runner_Num_Type'Range loop
            Wonrace(I) := Sim.Is_Race_Winner(Selectionid => Bra(I).Selectionid, Marketid => Bra(I).Marketid);
          end loop;
          if Placemarket_Exists then
            for I in Runner_Num_Type'Range loop
              Placedrace(I) := Sim.Is_Race_Winner(Selectionid => Bra(I).Selectionid, Marketid => Place_Marketid);
            end loop;
          end if;

          Bet_Id := Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid));

          Probh := (Betid          => Bet_Id,
                    Marketid       => Bra(1).Marketid,
                    Marketidplc    => Place_Marketid,
                    Distance       => Market.Distance,
                    Distancename   => Market.Distance_Name,
                    Svnrevision    => 0,
                    Ixxlupd        => (others         => ' '),
                    Ixxluts        => Calendar2.Clock);

          for I in Runner_Num_Type'Range loop
            Probr(Winner,I) := (Betid                 => Bet_Id,
                                Rank                  => Integer_4(I),
                                Markettype            => "WIN                      ",
                                Selectionid           => Bra(I).Selectionid,
                                R1limit               => Fixed_Type(Max_Leader_Price),
                                Backprice             => Bra(I).Backprice,
                                Layprice              => Bra(I).Layprice,
                                Backprice1            => -1.0,
                                Layprice1             => -1.0,
                                Betplaced             => Bra(I).Pricets,
                                Wonrace               => Wonrace(I),
                                Placedrace            => Placedrace(I),
                                Svnrevision           => 0,
                                Ixxlupd               => (others                => ' '),
                                Ixxluts               => Calendar2.Clock);
          end loop;
          for I in Runner_Num_Type'Range loop
            Probr(Place,I) := (Betid                 => Bet_Id,
                               Rank                  => Integer_4(I),
                               Markettype            => "PLC                      ",
                               Selectionid           => Bra(I).Selectionid,
                               R1limit               => Fixed_Type(Max_Leader_Price),
                               Backprice             => Bra(I).Backprice,
                               Layprice              => Bra(I).Layprice,
                               Backprice1            => -1.0,
                               Layprice1             => -1.0,
                               Betplaced             => Bra(I).Pricets,
                               Wonrace               => Wonrace(I),
                               Placedrace            => Placedrace(I),
                               Svnrevision           => 0,
                               Ixxlupd               => (others                => ' '),
                               Ixxluts               => Calendar2.Clock);
          end loop;
          Nexttime := Bra(1).Pricets + (0,0,0,1,0);
        end;
      end if;


    elsif Bra(1).Pricets + (0,0,0,1,0) >= Nexttime then
      -- get prices a second later
      for I in Runner_Num_Type'Range loop
        Probr(Winner,I).Backprice1 := Bra(I).Backprice;
        Probr(Winner,I).Layprice1 := Bra(I).Layprice;
      end loop;
      -- how fix plc prices ?

      Probh.Insert;
      for I in Runner_Num_Type'Range loop
        for M in Market_Type'Range loop
          Probr(M,I).Insert;
        end loop;
      end loop;
      Done := True;

    end if;

  end Treat;

  ------------------------------------------

  procedure Sort_Array(List : in out Price_Histories.Lists.List ;
                       Bra  : in out Best_Runners_Array_Type ) is

    Price             : Price_Histories.Price_History_Type;
  begin
    -- ok find the runner with lowest backprice:
    Backprice_Sorter.Sort(List);

    Price.Backprice := 10_000.0;
    Bra := (others => Price);

    declare
      Idx : Integer := 0;
    begin
      for Tmp of List loop
        if Tmp.Status(1..6) = "ACTIVE" and then
          Tmp.Backprice > Fixed_Type(1.0) and then
          Tmp.Layprice < Fixed_Type(1_000.0)  then
          Idx := Idx +1;
          exit when Idx > Bra'Last;
          Bra(Idx) := Tmp;
        end if;
      end loop;
    end ;

  end Sort_Array;
  -- pragma Unreferenced (Sort_Array);
  ---------------------------------------------------------------

  Start_Date          : constant Calendar2.Time_Type := (2016,03,16,0,0,0,0);
  One_Day             : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date        :          Calendar2.Time_Type := Start_Date;
  Stop_Date           : constant Calendar2.Time_Type := (2018,08,01,0,0,0,0);
  T                   :          Sql.Transaction_Type;
  Cmd_Line            :          Command_Line_Configuration;
  Sa_Logfilename      : aliased  Gnat.Strings.String_Access;
  Sa_Max_Leader_Price : aliased  Gnat.Strings.String_Access;
  Max_Leader_Price    :          Price_Type := 0.0;

begin
  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Define_Switch
    (Cmd_Line,
     Sa_Max_Leader_Price'Access,
     Long_Switch => "--max_leader_price=",
     Help        => "leader's back price must be lower that this");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","lay_losers_1");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);
  Log("main", "params start");
  Log("main", "Sa_Max_Leader_Price" & Sa_Max_Leader_Price.all);
  Log("main", "params stop");

  Max_Leader_Price := Price_Type'Value(Sa_Max_Leader_Price.all);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");

  Log("main", "Connect Db " &
        Ini.Get_Value("database_home", "host", "")  & " " &
        Ini.Get_Value("database_home", "port", 5432)'Img & " " &
        Ini.Get_Value("database_home", "name", "") & " " &
        Ini.Get_Value("database_home", "username", "") & " " &
        Ini.Get_Value("database_home", "password", "")
     );

  Sql.Connect
    (Host     => Ini.Get_Value("database_home", "host", ""),
     Port     => Ini.Get_Value("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value("database_home", "name", ""),
     Login    => Ini.Get_Value("database_home", "username", ""),
     Password => Ini.Get_Value("database_home", "password", ""));
  Log("main", "db Connected");

  Date_Loop : loop
    T.Start;
    Log("start fill maps");
    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse);
    Log("start process maps");
    T.Commit;

    declare
      Cnt       : Integer := 0;
      Probh     : Probh_Type;
      Probr     : Probr_Type;
    begin
      T.Start;
      Market_Loop : for Market of Sim.Market_With_Data_List loop
        if Market.Markettype(1..3) = "WIN" and then
          Market.Marketname_Ok then

          Cnt := Cnt + 1;
          -- list of timestamps in this market
          declare
            Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);
            Done                            : Boolean := False;
            Nexttime : Calendar2.Time_Type := Calendar2.Time_Type_First;
          begin
            Done := False;
            Loop_Ts : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              declare
                List     : Price_Histories.Lists.List := Timestamp_To_Prices_History_Map(Timestamp.To_String);
                Bra      : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
              begin
                Sort_Array(List => List, Bra => Bra);
                Treat(Market           => Market,
                      Bra              => Bra,
                      Max_Leader_Price => Max_Leader_Price,
                      Probh            => Probh,
                      Probr            => Probr,
                      Nexttime         => Nexttime,
                      Done             => Done);

                exit Market_Loop when Done;
              end;
            end loop Loop_Ts; --  Timestamp
          end;
        end if; -- Market_type(1..3) = WIN
      end loop Market_Loop;
      T.Commit;
    end;

    --Sim.Delete_Shared_Mem(Current_Date, Bot_Types.Horse);

    Current_Date := Current_Date + One_Day;
    exit when Current_Date = Stop_Date;

  end loop Date_Loop;

  Sql.Close_Session;

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
    Sql.Close_Session;
end Do_Stats_2;
