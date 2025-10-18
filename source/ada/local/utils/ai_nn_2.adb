with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Containers.Doubly_Linked_Lists;
--with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
with Ada.Strings ; use Ada.Strings;
with Ada.Strings.Fixed ; use Ada.Strings.Fixed;
with Text_Io;

with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Types;    use Types;
with Sql;
with Calendar2; use Calendar2;
--with Logging;               use Logging;
with Ini;
with Stacktrace;
with Table_Amarkets;
with Table_Arunners;
--with Markets;
with Table_Arewards;

with Table_Apriceshistory;
with Bot_Types;
with Sim;

procedure Ai_Nn_2 is
  package Ev renames Ada.Environment_Variables;
  package Ad renames Ada.Directories;
  Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type;
  Select_Runner_With_Price        : Sql.Statement_Type;
  -- Select_Markets        : Sql.Statement_Type;

  Sa_Startdate       : aliased Gnat.Strings.String_Access;
  Sa_Stopdate       : aliased Gnat.Strings.String_Access;
  --  Sa_Side             : aliased Gnat.Strings.String_Access;Ev.Value("BOT_HISTORY") & "/data/ai/pong/lay/win/"
  Ba_Train_Set       : aliased Boolean := False;
  Ba_Layprice        : aliased Boolean := False;

 --  Global_Side          : String (1..4) := "BOTH";

  Gdebug               : Boolean := True;

  type R_Type is record
    Runner  : Table_Arunners.Data_Type;
    --  Price   : Table_Aprices.Data_Type;
    History : Table_Apriceshistory.Data_Type;
    Market  : Table_Amarkets.Data_Type;
    Reward  : Table_Arewards.Data_Type;
  end record;

  package R_Pkg is new Ada.Containers.Doubly_Linked_Lists (R_Type);



  -------------------------------
  procedure Debug (What : String) is
  begin
    if Gdebug then
      Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_Iso (Clock, " " , "") & " " & What);
    end if;
  end Debug;
  pragma Warnings (Off, Debug);
  -------------------------------
  procedure Print (What : String) with Unreferenced is
  begin
    Text_Io.Put_Line (What);
  end Print;
  -------------------------------


  procedure Print (L  : R_Pkg.List ) is
    Cnt         : Integer := 0;
    --   Winners     : array (1..3) of Integer_4 := (others => -1);

    F           : Text_Io.File_Type with Warnings => Off;
    Path1       : String := (if Ba_Layprice then Ev.Value ("BOT_HISTORY") & "/data/ai/pong/1st/lay/win/" else
                               Ev.Value ("BOT_HISTORY") & "/data/ai/pong/1st/back/win/") ;
    Num_Real_Runners : Integer := 0;
    Pricets     : Calendar2.Time_Type := Calendar2.Time_Type_First;

    type Print_Data_Type is record
      Backprice   : Fixed_Type := 0.0;
      Layprice    : Fixed_Type := 0.0;
      Reward     : Fixed_Type := 0.0;
      Selectionid : Integer_4 := 0;
      Sortprio    : Integer_4 := 0;
    end record;

    type Data_Type is array (1 .. 16) of Print_Data_Type;
    Empty_Data  : constant Data_Type := ( 1.. 16 => (Backprice    => <>,
                                                     Layprice     => <>,
                                                     Reward       => <>,
                                                     Selectionid  => <>,
                                                     Sortprio     => <>)) ;
    Old_Data    : Data_Type;
    Data        : Data_Type;
    Marketid    : Bot_Types.Marketid_Type := (others => ' ');
    --  Markettype  : Bot_Types.Markettype_Type := (others => ' ');
    Marketname  : Bot_Types.Marketname_Type := (others => ' ');

    ------------------------------------------------
    procedure Do_Print_Line (F : Text_Io.File_Type) is
    begin
      --0-based idx

      for I in Data'Range loop
        Text_Io.Put (F, Data (I).Selectionid'Img);   --3-18
        Text_Io.Put (F, ",");
      end loop;

      for I in Data'Range loop  --19-34
        if Ba_Layprice then
          Text_Io.Put (F, Float'Image (Float (Data (I).Layprice)));
        else
          Text_Io.Put (F, Float'Image (Float (Data (I).Backprice)));
        end if;
        Text_Io.Put (F, ",");
      end loop;

      for I in Data'Range loop   --35-50
        if Ba_Layprice then
          Text_Io.Put (F, Float'Image (Float (Data (I).Layprice - Old_Data (I).Layprice)));
        else
          Text_Io.Put (F, Float'Image (Float (Data (I).Backprice - Old_Data (I).Backprice)));
        end if;

        if I = Data'Last then
          -- put here anything after the last array
          Text_Io.Put (F, "," & Pricets.String_Date_Time_Iso (T => " ", Tz => "")); --51
          Text_Io.Put_Line (F, "");  -- <-- last statement on this row
          Old_Data := Data;
        else
          Text_Io.Put (F, ",");
        end if;
      end loop;
    end Do_Print_Line;
    ------------------------------------------------

    function Fix_Path (P : String) return String is
      Lp : String := Trim (P, Right);
    begin
      for I in Lp'Range loop
        case Lp (I) is
          when ' ' => Lp (I) := '_';
          when others => null;
        end case;
      end loop;
      return Lp;
    end Fix_Path;
    ---------------------------------

  begin
    Text_Io.Put_Line (Text_Io.Standard_Error, "length list" & L.Length'Img);
    Data := Empty_Data;
    --count runners
    for R of L loop
      Num_Real_Runners := Integer (R.Market.Numactiverunners);
      Marketid := R.Market.Marketid;
      Marketname := R.Market.Marketname;
      exit;
    end loop;

    Text_Io.Put_Line (Text_Io.Standard_Error, "open '" & Marketid & "'");

    declare
      Path : String := (if Ba_Train_Set then
                          Fix_Path (Path1 & "train/" & Marketname)
                        else
                          Fix_Path (Path1 & "sample/" & Marketname));
    begin
      if not Ad.Exists (Path) then
        Ad.Create_Path (Path);
      end if;
      Text_Io.Create (F, Text_Io.Out_File, Path & "/" & Marketid & ".csv");
    end;

    for R of L loop
--      Debug (R.Market.To_String);
      Cnt := Cnt + 1;
--      Debug (Cnt'Img);
--      Debug (R.Runner.To_String);
      Data (Cnt).Backprice := R.History.Backprice;
    --  Data (Cnt).Layprice  := R.History.Layprice;
      Data (Cnt).Selectionid := R.Runner.Selectionid;
      Data (Cnt).Sortprio := R.Runner.Sortprio;
      Data (Cnt).Reward := R.Reward.Profit;

      if Cnt = Num_Real_Runners then
        Pricets := R.History.Pricets; -- update to this line's pricets
        Do_Print_Line (F);
        Cnt := 0;
      end if;

    end loop;

    Text_Io.Put_Line (Text_Io.Standard_Error, "close file");
    Text_Io.Close (F);
  end Print;

  --------------------------------------------------------
  procedure Get_Runner_Data (Marketid : Bot_Types.Marketid_Type ) is
    type Eos_Type is (History, Market, Runner, Areward);
    Eos                : array (Eos_Type'Range) of Boolean := (others => False);
    R_List             : R_Pkg.List;
    R_Data             : R_Type;
  begin
    R_Data.Market.Marketid := Marketid;
    R_Data.Market.Read (Eos (Market));

    Select_Runner_With_Price.Prepare ("select H.* " &
                                        "from ARUNNERS R, APRICESHISTORY H " &
                                        "where 1 = 1 " &
                                        "and H.MARKETID = R.MARKETID " &
                                        "and H.MARKETID = :MARKETID " &
                                        "and H.SELECTIONID = R.SELECTIONID " &
                                        "and R.STATUS <> 'REMOVED' " &
                                        "and H.MARKETID in (select marketid from OKMARKETS) " &
                                        "order by H.PRICETS, R.SORTPRIO " );

    Select_Runner_With_Price.Set ("MARKETID", Marketid);
    Select_Runner_With_Price.Open_Cursor;
    loop
      Select_Runner_With_Price.Fetch (Eos (History));
      exit when Eos (History);
      R_Data.History := Table_Apriceshistory.Get (Select_Runner_With_Price);
      R_Data.Runner.Marketid := R_Data.History.Marketid;
      R_Data.Runner.Selectionid := R_Data.History.Selectionid;
      R_Data.Runner.Read (Eos (Runner));

      R_Data.Reward.Marketid := R_Data.History.Marketid;
      R_Data.Reward.Selectionid := R_Data.History.Selectionid;
      R_Data.Reward.Pricets := R_Data.History.Pricets;
      R_Data.Reward.Side := (if Ba_Layprice then "LAY " else "BACK");
      R_Data.Reward.Read (Eos (Areward));

      R_List.Append (R_Data);
    end loop;
    Select_Runner_With_Price.Close_Cursor;
    if Integer (R_List.Length) > 500 then
      Print (R_List);
    else
      Text_Io.Put_Line (Text_Io.Standard_Error, Marketid & " had only" &  R_List.Length'Img & " lines");
    end if;

  end Get_Runner_Data;
  ------------------------------------------------------

  --  procedure Get_Market_Data(Market_List  : in out Table_Amarkets.Amarkets_List_Pack2.List) is
  --
  --  begin
  --
  --    if Ba_Train_Set then
  --      Select_Markets.Prepare( "select M.* " &
  --                                "from AMARKETS M " &
  --                                "where true " &
  --                                "and M.MARKETTYPE = 'WIN' " &
  --                                "and M.NUMACTIVERUNNERS >= 8 " &
  --                                "and M.NUMACTIVERUNNERS <= 16 " &
  --                              --  "and m.marketid = '1.151619897' " &
  --                                "and M.EVENTID not like '%2' " & --use the ones that and with 2 as test sample
  --                                "order by M.STARTTS");
  --    else -- to verify with - just 10 %
  --      Select_Markets.Prepare("select M.* " &
  --                               "from AMARKETS M " &
  --                               "where true " &
  --                               "and M.MARKETTYPE = 'WIN' " &
  --                               "and M.NUMACTIVERUNNERS >= 8 " &
  --                               "and M.NUMACTIVERUNNERS <= 16 " &
  --                               "and M.EVENTID like '%2' " & --use the ones that and with 2 as test sample
  --                               "order by M.STARTTS");
  --    end if;
  --
  --    Table_Amarkets.Read_List(Select_Markets, Market_List);
  --  end Get_Market_Data;
  ------------------------------------------------------

  --  Mlist  :  Table_Amarkets.Amarkets_List_Pack2.List;


  Start_Date                      :          Calendar2.Time_Type := (2019, 1, 1, 0, 0, 0, 0);
  One_Day                         : constant Calendar2.Interval_Type := (1, 0, 0, 0, 0);
  Current_Date                    :          Calendar2.Time_Type := Start_Date;
  Stop_Date                       :          Calendar2.Time_Type := (2022, 1, 1, 23, 59, 59, 999);

begin

  Define_Switch
    (Cmd_Line,
     Sa_Startdate'Access,
     Long_Switch => "--startdate=",
     Help        => "startdate");

  Define_Switch
    (Cmd_Line,
     Sa_Stopdate'Access,
     Long_Switch => "--stopdate=",
     Help        => "stopdate");


  Define_Switch
    (Cmd_Line,
     Ba_Train_Set'Access,
     Long_Switch => "--trainset",
     Help        => "Trainset - otherwise sample set");

  Define_Switch
    (Cmd_Line,
     Ba_Layprice'Access,
     Long_Switch => "--layprice",
     Help        => "Layprices - otherwise backprices");


  Getopt (Cmd_Line);  -- process the command line

   Ini.Load (Ev.Value ("BOT_HOME") & "/login.ini");

  Debug ("Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value ("database_home", "host", ""),
     Port     => Ini.Get_Value ("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value ("database_home", "name", ""),
     Login    => Ini.Get_Value ("database_home", "username", ""),
     Password => Ini.Get_Value ("database_home", "password", ""),
     Ssl_Mode => "prefer");
  Debug ("db Connected");

  if Sa_Startdate.all /= "" then
    Start_Date := Calendar2.To_Time_Type (Sa_Startdate.all, "");
  end if;

  if Sa_Stopdate.all /= "" then
    Stop_Date := Calendar2.To_Time_Type (Sa_Stopdate.all, "");
  end if;

  Debug ("main" & " params start");
  Debug ("main" & " start_date '" & Sa_Startdate.all & "'");
  Debug ("main" & " stop_date  '" & Sa_Stopdate.all & "'");
  Debug ("main" & " params stop");


  Current_Date := Start_Date;

  Date_Loop : loop
    T.Start;
    Debug ("start fill maps " & Current_Date.String_Date_Iso);
    Sim.Fill_Data_Maps (Current_Date, Bot_Types.Horse, Rewards => False, Racetimes => False);
    Debug ("start process maps");

    for C in Sim.Win_Place_Map.Iterate loop
      Get_Runner_Data (Sim.Win_Place_Maps.Key (C));
    end loop;

    T.Commit;
    Current_Date := Current_Date + One_Day;
    exit Date_Loop when Current_Date >= Stop_Date;

  end loop Date_Loop;

  Sql.Close_Session;

exception
  when E : others =>
    Stacktrace.Tracebackinfo (E);
end Ai_Nn_2;
