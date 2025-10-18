
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Gnat; use Gnat;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Types; use Types;
with Calendar2; use Calendar2;

with Stacktrace;
--with Ada.Containers.Doubly_Linked_Lists;
with Price_Histories;
with Ini;
with Logging; use Logging;
with Sql;
with Sim;
with Bot_Types; --use Bot_Types;

with Table_Arewards;


procedure Rewards3 is
  package Ev renames Ada.Environment_Variables;

  --    function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  --    begin
  --      return Left.Backprice < Right.Backprice;
  --    end "<";
  --    package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");
  Global_Size : constant Bot_Types.Bet_Size_Type := 100.0;
  Commission  : constant Fixed_Type := 2.0/100.0;

  type Best_Runners_Array_Type is array (1..16) of Price_Histories.Price_History_Type;

  procedure To_Array(List : in out Price_Histories.Lists.List ;
                     Bra  : in out Best_Runners_Array_Type ) is

    Price             : Price_Histories.Price_History_Type := Price_Histories.Empty_Data;
  begin
    Price.Backprice := 0.0;
    Bra := (others => Price);

    declare
      Idx : Integer := 0;
    begin
      for Tmp of List loop
        if Tmp.Status(1..6) = "ACTIVE" then
          Idx := Idx +1;
          exit when Idx > Bra'Last;
          Bra(Idx) := Tmp;
        end if;
      end loop;
    end ;

  end To_Array;

  -----------------------------------------------------
  function To_Time(S2: String) return Calendar2.Time_Type is
    Tmp : Time_Type;
    S   : String (1 .. S2'Last - S2'First + 1) := S2;
  begin
    -- '22-01-2013 11:09:06'

    Tmp.Year  := Year_Type'Value (S (7 .. 10));
    Tmp.Month := Month_Type'Value (S (4 .. 5));
    Tmp.Day   := Day_Type'Value (S (1 .. 2));

    Tmp.Hour   := Hour_Type'Value (S (12 .. 13));
    Tmp.Minute := Minute_Type'Value (S (15 .. 16));
    if S'Length = 19 then
      Tmp.Second := Second_Type'Value (S (18 .. 19));
    else
      Tmp.Second := 0;
    end if;
    return Tmp;
  end To_Time;
  pragma Unreferenced(To_Time);
  -----------------------------------------------------

  function Create_Marketname(S : String) return String is
    Tmp : String := Trim(S,Both);
  begin
    for I in Tmp'Range loop
      case Tmp(I) is
      when ' ' => Tmp(I) := '_';
      when others => null;
      end case;
    end loop;
    return Tmp;
  end Create_Marketname;
  pragma Unreferenced(Create_Marketname);
  -------------------------------------------------------

  procedure Check_Odds (B : in Best_Runners_Array_Type; Seen_1_0x, More_Than_Limit : in out Boolean) is
    Some_Under_Limit : Boolean := False;
  begin
    for Runner of B loop
      if not Seen_1_0x and then Runner.Backprice <= Fixed_Type(1.05) then
        Seen_1_0x := True;
      end if;
    end loop;

    if Seen_1_0x then
      for Runner of B loop
        -- is any runners within 1.01 - 25 ? then All_more_than limit is false
        if Fixed_Type (1.0) < Runner.Backprice and then Runner.Backprice < Fixed_Type (25.0) then
          Some_Under_Limit := True;
          exit;
        end if;
      end loop;
      More_Than_Limit := not Some_Under_Limit;
    end if;
  end Check_Odds;

  -------------------------------------------------------
  function Check_Profit_Back (Runner : Price_Histories.Price_History_Type; Ttphm :  Sim.Timestamp_To_Prices_History_Maps.Map) return Bot_Types.Profit_Type is
    Is_Winner   : Boolean := False;
    use Bot_Types;
    Profit      : Profit_Type := 0.0;
  begin

    for W of Sim.Winners_Map (Runner.Marketid) loop
      if W.Selectionid = Runner.Selectionid then
        Is_Winner := True;
        exit;
      end if;
    end loop;

    --simplification, but ok. betting on the wrong horse should be penalized, even if not matched
    if not Is_Winner then
      return - Profit_Type(Global_Size);
    end if;

    Loop_Ts_Check_Profit : for Timestamp of Sim.Marketid_Pricets_Map (Runner.Marketid) loop
      declare
        List  : Price_Histories.Lists.List := Ttphm (Timestamp.To_String);
        Tmp   : Profit_Type := 0.0;
      begin
        if Timestamp > Runner.Pricets 
          and then Timestamp - Runner.Pricets >= (0, 0, 0, 1, 0) -- check time within +1..2 sec
          and then Timestamp - Runner.Pricets <= (0, 0, 0, 2, 0) 
        then 
          for J of List loop
            if J.Selectionid = Runner.Selectionid then 
              if J.Backprice >= Runner.Backprice then --match
                  Tmp := Profit_Type(Global_Size) * Profit_Type (Runner.Backprice - 1.0) ; -- or J.Backprice ? we are pessimistic
                  Profit := Tmp * Profit_Type (1.0 - Commission);
                exit Loop_Ts_Check_Profit;
              end if;
            end if;
          end loop;
        end if;
      end;
    end loop Loop_Ts_Check_Profit;
    return Profit;
  end Check_Profit_Back;

  -------------------------------------------------------
  function Check_Profit_Lay (Runner : Price_Histories.Price_History_Type; Ttphm :  Sim.Timestamp_To_Prices_History_Maps.Map) return Bot_Types.Profit_Type is
    Is_Winner   : Boolean := False;
    use Bot_Types;
    Profit      : Profit_Type := 0.0;
  begin

    for W of Sim.Winners_Map (Runner.Marketid) loop
      if W.Selectionid = Runner.Selectionid then
        Is_Winner := True;
        exit;
      end if;
    end loop;

    if Is_Winner and Runner.Layprice > Fixed_Type'(1.0) then
      return - Profit_Type(Global_Size) * Profit_Type (Runner.Layprice - 1.0);
    end if;

    Loop_Ts_Check_Profit : for Timestamp of Sim.Marketid_Pricets_Map (Runner.Marketid) loop
      declare
        List                : Price_Histories.Lists.List := Ttphm (Timestamp.To_String);
      begin

        if Timestamp > Runner.Pricets 
          and then Timestamp - Runner.Pricets >= (0, 0, 0, 1, 0) -- check time within +1..2 sec
          and then Timestamp - Runner.Pricets <= (0, 0, 0, 2, 0) 
        then
          for J of List loop
            if J.Selectionid = Runner.Selectionid  then
              if J.Layprice <= Runner.Layprice 
                and then J.Layprice > Fixed_Type'(0.0) 
              then --match
                if Is_Winner then 
                  Profit := - Profit_Type(Global_Size) * Profit_Type (Runner.Layprice - 1.0);
                else
                  Profit := Profit_Type(Global_Size) * Profit_Type (1.0 - Commission);
                end if;
                exit Loop_Ts_Check_Profit;
              end if;
            end if;
          end loop;
        end if;
      end;
    end loop Loop_Ts_Check_Profit;
    return Profit;
  end Check_Profit_Lay;
  ------------------------------------------------------

  Sa_Start_Date                   : aliased  Gnat.Strings.String_Access;
  Sa_Stop_Date                    : aliased  Gnat.Strings.String_Access;
  Sa_Logfilename                  : aliased  Gnat.Strings.String_Access;
  Start_Date                      :          Calendar2.Time_Type := (2018,8,1,0,0,0,0);
  One_Day                         : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date                    :          Calendar2.Time_Type := Start_Date;
  Stop_Date                       :          Calendar2.Time_Type := (2023,10,1,23,59,59,999);
  Cmd_Line                        :          Command_Line_Configuration;
  T                               :          Sql.Transaction_Type;
begin

  Define_Switch
    (Cmd_Line,
     Sa_Start_Date'Access,
     Long_Switch => "--startdate=",
     Help        => "start date eg 2019-02-25");

  Define_Switch
    (Cmd_Line,
     Sa_Stop_Date'Access,
     Long_Switch => "--stopdate=",
     Help        => "stop date eg 2019-12-21");

  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","rewards");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");
  --  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Log("main", "params start");
  Log("main", "start_date '" & Sa_Start_Date.all & "'");
  Log("main", "stop_date  '" & Sa_Stop_Date.all & "'");
  Log("main", "logfile    '" & Sa_Logfilename.all & "'");
  Log("main", "params stop");

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

  if Sa_Start_Date.all /= "" then
    Start_Date := Calendar2.To_Time_Type(Sa_Start_Date.all,"");
  end if;

  if Sa_Stop_Date.all /= "" then
    Stop_Date := Calendar2.To_Time_Type(Sa_Stop_Date.all,"");
  end if;

  Current_Date := Start_Date;
  Date_Loop : loop
    T.Start;
    Log("start fill maps " & Calendar2.String_Date(Current_Date));
    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse, Rewards => False, Racetimes => False);
    Log("start process maps");

    declare
      Cnt       : Integer := 0;
    begin
      Market_Loop : for Market of Sim.Market_With_Data_List loop
        Log("start market " & Market.To_String);

        if Market.Markettype(1..3) = "WIN"
          and then 8 <= Market.Numactiverunners
          and then Market.Numactiverunners <= 16 and then
         Market.Marketname_Ok
        then
          Cnt := Cnt + 1;

          -- list of timestamps in this market
          declare
            Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                Sim.Marketid_Timestamp_To_Prices_History_Map (Market.Marketid);

            Timestamp_To_Prices_History_Map2 : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                 Sim.Marketid_Timestamp_To_Prices_History_Map (Market.Marketid);
            Have_Seen_1_0x                   : Boolean := False;
            All_More_Than_Limit              : Boolean := True;
            Last_Poll                        : Calendar2.Time_Type := Calendar2.Time_Type_First;
          begin
            Loop_Ts : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
              declare
                List                : Price_Histories.Lists.List := Timestamp_To_Prices_History_Map (Timestamp.To_String);
                Bra                 : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
                Delta_Time          : Calendar2.Interval_Type := (0, 0, 0, 0, 0);
                Profit_Back,
                Profit_Lay          : Bot_Types.Profit_Type := 0.0;
                --use Bot_Types;
                Reward              : Table_Arewards.Data_Type;
              begin
                --                Text_Io.Put_Line("start loop " & Calendar2.Clock.String_Date_Time_Iso);
                To_Array(List => List, Bra => Bra);

                Delta_Time := Bra (1).Pricets - Last_Poll;
                Check_Odds (Bra, Have_Seen_1_0x, All_More_Than_Limit);
                exit Loop_Ts when Have_Seen_1_0x and then All_More_Than_Limit;

                if Delta_Time < (0, 0, 0, 2, 0) then -- don't bother when race not started
                  for I in Bra'Range loop
                    if Bra(I).Selectionid > 0 then
                      Profit_Back := Check_Profit_Back(Bra (I),Timestamp_To_Prices_History_Map2);
                      Reward := (Market.Marketid,
                                 Bra(I).Selectionid,
                                 Timestamp,
                                 "BACK",
                                 Fixed_Type(Profit_Back),
                                 (others => ' '),
                                 Timestamp
                                );
                      Reward.Insert;

                      Profit_Lay := Check_Profit_Lay(Bra (I),Timestamp_To_Prices_History_Map2);
                      Reward.Profit := Fixed_Type(Profit_Lay);
                      Reward.Side := "LAY ";
                      Reward.Insert;

                    end if;
                  end loop;
                end if;
                Last_Poll := Bra (1).Pricets;
              end;
            end loop Loop_Ts; --  Timestamp
          end;
        end if; -- Market_type(1..3) = WIN
      end loop Market_Loop;
    end;
    T.Commit;

    Current_Date := Current_Date + One_Day;
    exit Date_Loop when Current_Date = Stop_Date;

  end loop Date_Loop;
  Sql.Close_Session;
exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);

end Rewards3;
