with Ada.Strings; use Ada.Strings;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;

with Gnat;
with Gnat.Awk;



with Sim;
with Utils; use Utils;
with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
--with Text_Io;
with Price_Histories;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Calendar2;  use Calendar2;
with Logging; use Logging;
with Ini;


procedure Sample_Percent_1 is

  package Ev renames Ada.Environment_Variables;
  --------------------------------------------------------------------------

  function "<" (Left,Right : Price_Histories.Price_History_Type) return Boolean is
  begin
    return Left.Backprice < Right.Backprice;
  end "<";
  --------------------------------------------
  package Backprice_Sorter is new Price_Histories.Lists.Generic_Sorting("<");

  type Best_Runners_Array_Type is array (1..4) of Price_Histories.Price_History_Type;

  Best_Runners_Win : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);
  Best_Runners_Plc : Best_Runners_Array_Type := (others => Price_Histories.Empty_Data);

  package Times is new Ada.Containers.Hashed_Maps
    (Marketname_Type,
     Interval_Type,
     Ada.Strings.Hash,
     "=",
     "=");

  Timemap : Times.Map;

  -------------------------------------------------------------------------
  procedure Sort_Array(List : in out Price_Histories.Lists.List ;
                       Bra  :    out Best_Runners_Array_Type ) is
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
  ---------------------------------------------------------------
  function To_Interval(S2: String) return Calendar2.Interval_Type is
    Tmp : Interval_Type;
    S : String (1.. S2'Length) := S2;
  begin
    -- '02:29.585'
    Tmp.Days := 0;
    Tmp.Hours := 0;
    Tmp.Minutes := Minute_Type'Value (S (1 .. 2));
    Tmp.Seconds :=  Minute_Type'Value (S (4 .. 5));
    Tmp.Milliseconds := Millisecond_Type'Value(S(7..9));
    return Tmp;
  end To_Interval;
  -----------------------------------------------------

  procedure Read_Avg_Race_Times (Tm : in out Times.Map; File : String) is
    use Gnat;
    Computer_File : Awk.Session_Type;
  begin
    Log("start Read_Avg_Race_Times");
    Awk.Set_Current (Computer_File);
    Awk.Open (Separators => "|",
              Filename   => File);

    while not Awk.End_Of_File loop
      Awk.Get_Line;
      Tm.Insert(Awk.Field(1), To_Interval(Awk.Field(2)));
    end loop;
    Awk.Close (Computer_File);

    for T in Tm.Iterate loop
      Log(Times.Key(T) & " -> " & String_Interval(Times.Element(T),Days => False,Hours => False));
    end loop;

    Log("stop  Read_Avg_Race_Times");
  end Read_Avg_Race_Times;

  -------------------------------------------

  function Is_Marketname_Ok(Name : Marketname_Type ; Tm : Times.Map) return Boolean is
  begin
   -- Log("start  Is_Marketname_Ok '" & Name & "'");
    for T in Tm.Iterate loop
      if Times.Key(T) = Name then
      --  Log("stop  Is_Marketname_Ok TRUE");
        return True;
      end if;
    end loop;
  --  Log("stop  Is_Marketname_Ok FALSE");
    return False;
  end Is_Marketname_Ok;

  -------------------------------------------

  Start_Date     : constant Calendar2.Time_Type := (2016,03,16,0,0,0,0);
  One_Day        : constant Calendar2.Interval_Type := (1,0,0,0,0);
  Current_Date   :          Calendar2.Time_Type := Start_Date;
  Stop_Date      :          Calendar2.Time_Type := (2018,03,01,0,0,0,0);
  T              :          Sql.Transaction_Type;
  Cmd_Line       :          Command_Line_Configuration;
  Sa_Logfilename : aliased  Gnat.Strings.String_Access;
  Sa_Datafilename : aliased  Gnat.Strings.String_Access;
  Ia_Percent      : aliased  Integer := 0;

begin
  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Define_Switch
    (Cmd_Line,
     Sa_Datafilename'Access,
     Long_Switch => "--datafile=",
     Help        => "path to avg times data file");

  Define_Switch
    (Cmd_Line,
     Ia_Percent'Access,
     Long_Switch => "--percent=",
     Help        => "percent of race (integer)");

  Getopt (Cmd_Line);  -- process the command line

  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","sp1");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log("main", "Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database_home", "host", ""),
     Port     => Ini.Get_Value("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value("database_home", "name", ""),
     Login    => Ini.Get_Value("database_home", "username", ""),
     Password => Ini.Get_Value("database_home", "password", ""));
  Log("main", "db Connected");

  Log("main", "parameters start");
  Log("main", "datafile " & Sa_Datafilename.all);
  Log("main", "percent" & Ia_Percent'Img);
  Log("main", "parameters stop");

  Read_Avg_Race_Times(Tm => Timemap, File => Sa_Datafilename.all);

  Date_Loop : loop
    T.Start;
    Log("start process " & Current_Date.String_Date_ISO);
    Sim.Fill_Data_Maps(Current_Date, Bot_Types.Horse);

    declare
      Cnt          : Integer := 0;
      Percent      : Fixed_Type := Fixed_Type(Ia_Percent)/100.0;
      Place_Exists : Boolean := False;
      Place_Market_Id : Marketid_Type := (others => ' ');
    begin
      Market_Loop : for Market of Sim.Market_With_Data_List loop
        if Market.Markettype(1..3) = "WIN" then
          Place_Market_Id := (others => ' ');
          begin
            Place_Market_Id := Sim.Win_Place_Map(Market.Marketid);
            Place_Exists := True;
          exception
            when others =>
              Place_Exists := False;
          end;

          if Is_Marketname_Ok(Market.Marketname, Timemap) and then Place_Exists then

            Cnt := Cnt + 1;
            --   Log( F8_Image(Fixed_Type(Cnt)*100.0/ Fixed_Type(Sim.Market_Id_With_Data_List.Length)) & " %");
            -- list of timestamps in this market
            begin
              declare
                Win_Market_Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                               Sim.Marketid_Timestamp_To_Prices_History_Map(Market.Marketid);
                Plc_Market_Timestamp_To_Prices_History_Map : Sim.Timestamp_To_Prices_History_Maps.Map :=
                                                               Sim.Marketid_Timestamp_To_Prices_History_Map(Place_Market_Id);
                Stop_Time                                  : Time_Type := Time_Type_Last;
                Last_Time                                  : Time_Type := Time_Type_First;
                So                                         : String_Object;
                Found                                      : Boolean := False;
              begin
                -- check start of race and add percetange of avf time to find sampletime
                Loop_Ts1 : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
                  Log("Timestamp: " & Timestamp.To_String(Milliseconds => True) & " Last_time: " & Last_Time.To_String(Milliseconds => True));
                  if Last_Time = Time_Type_First then
                    null; -- skip first sample
                  elsif not Found and then Timestamp - Last_Time < (0,0,0,1,0) then -- race started
                    Stop_Time := Timestamp + To_Interval(Seconds_Type(Percent * Fixed_Type(To_Seconds(Timemap(Market.Marketname)))));
                    Log("Stop_Time found " & Stop_Time.To_String(Milliseconds => True) & "/" & Timestamp.To_String(Milliseconds => True) );
                    Found := True;
                   -- exit Loop_Ts1;
                  end if;
                  Last_Time := Timestamp;
                end loop Loop_Ts1;

                -- Get the data for winner market
                Loop_Ts_Win : for Timestamp of Sim.Marketid_Pricets_Map(Market.Marketid) loop
                  if Timestamp >= Stop_Time then

                    So.Set("datapoint|" & Market.Marketid & "|" & Place_Market_Id & "|" & F8_Image(Percent) & "|");

                    declare
                      List : Price_Histories.Lists.List := Win_Market_Timestamp_To_Prices_History_Map(Timestamp.To_String);
                    begin
                      --  Log("in loop", Timestamp.To_String & "_" );

                      Sort_Array(List => List, Bra => Best_Runners_Win);
                      for I in 1..3 loop
                        So.Append(F8_Image(Best_Runners_Win(I).Backprice) & "|");
                        So.Append(Sim.Is_Race_Winner(Best_Runners_Win(I).Selectionid, Market.Marketid)'Img & "|");
                      end loop;

                    end;
                    exit Loop_Ts_Win ;
                  end if;
                end loop Loop_Ts_Win; --  Timestamp winner market

                -- Get the data for place market
                Loop_Ts_Plc : for Timestamp of Sim.Marketid_Pricets_Map(Place_Market_Id) loop
                  if Timestamp >= Stop_Time then
                    declare
                      List : Price_Histories.Lists.List := Plc_Market_Timestamp_To_Prices_History_Map(Timestamp.To_String);
                      Found : Boolean := False;
                    begin
                      --  Log("in loop", Timestamp.To_String & "_" );
                      Sort_Array(List => List, Bra => Best_Runners_Plc);
                      -- find the runner in same order as win market. check for sel id

                      for I in 1..3 loop
                        Found := False;
                        for Y in Best_Runners_Plc'Range loop
                          if Best_Runners_Win(I).Selectionid = Best_Runners_Plc(Y).Selectionid then
                            So.Append(F8_Image(Best_Runners_Plc(Y).Backprice) & "|");
                            So.Append(Sim.Is_Race_Winner(Best_Runners_Plc(Y).Selectionid, Place_Market_Id)'Img & "|");
                            Found := True;
                            exit;
                          end if;
                        end loop;
                        if not Found then
                            So.Append(F8_Image(1000.0) & "|");
                            So.Append("FALSE" & "|");
                        end if;
                      end loop;
                    end;
                    exit Loop_Ts_Plc ;
                  end if;
                end loop Loop_Ts_Plc; --  Timestamp place market
                So.Append(Market.Marketname & "|");

                Log(So.Fix_String);
              exception
                  -- usually key not in map ie no sample for plc
                when Constraint_Error =>
                  Log("CE1/plc/win markets " & Place_Market_Id & "/" & Market.Marketid);
              end;
            exception
                -- usually key not in map ie no sample for plc
              when Constraint_Error =>
                Log("CE2/plc/win markets " & Place_Market_Id & "/" & Market.Marketid);
            end;

          end if; -- Is_Marketname_Ok
        end if; -- WIN
      end loop Market_Loop;
    end;

    T.Commit;

    Current_Date := Current_Date + One_Day;
    exit when Current_Date = Stop_Date;

  end loop Date_Loop;

  Sql.Close_Session;    -- no need for db anymore

exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Sample_Percent_1;
