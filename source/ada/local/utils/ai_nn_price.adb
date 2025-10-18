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
with Table_Arewards;
with Markets;

--with Table_Apriceshistory;
with Table_Aprices;
with Bot_Types;


procedure Ai_Nn_price is
  package Ev renames Ada.Environment_Variables;
  package AD renames Ada.Directories;
  Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type;
  Select_Runner_With_Price        : Sql.Statement_Type;
  Select_Markets        : Sql.Statement_Type;

  Sa_Startdate       : aliased Gnat.Strings.String_Access;
--  Sa_Side             : aliased Gnat.Strings.String_Access;Ev.Value("BOT_HISTORY") & "/data/ai/pong/lay/win/"
  Ba_Train_Set       : aliased Boolean := False;
  Ba_Layprice        : aliased Boolean := False;
  Ia_Position        : aliased Integer := 0;

  Global_Start_Date    : Time_Type := Time_Type_First;

--  Global_Side          : String (1..4) := "BOTH";

  Gdebug : Boolean := True;

  type R_Type is record
    Runner  : Table_Arunners.Data_Type;
    Price   : Table_Aprices.Data_Type;
  --  History : Table_Apriceshistory.Data_Type;
    Market  : Table_Amarkets.Data_Type;
    Back_Reward : Table_Arewards.Data_Type;
    Lay_Reward : Table_Arewards.Data_Type;
  end record;

  package R_Pkg is new Ada.Containers.Doubly_Linked_Lists(R_Type);



  -------------------------------
  procedure Debug (What : String) is
  begin
    if Gdebug then
      Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_Iso (Clock, " " , "") & " " & What);
    end if;
  end Debug;
  pragma Warnings(Off, Debug);
  -------------------------------
  procedure Print (What : String) with Unreferenced is
  begin
    Text_Io.Put_Line (What);
  end Print;
  -------------------------------


  procedure Print(L  : R_Pkg.List ) is
    Cnt         : Integer := 0;
    Winners     : array (1..3) of Integer_4 := (others => -1);

    F           : Text_Io.File_Type with Warnings => Off;
    Path1       : String := (if Ba_Layprice then Ev.Value("BOT_HISTORY") & "/data/ai/pong/1st/lay/win/" else
                                                 Ev.Value("BOT_HISTORY") & "/data/ai/pong/1st/back/win/") ;
    Path2       : String := (if Ba_Layprice then Ev.Value("BOT_HISTORY") & "/data/ai/pong/2nd/lay/win/" else
                                                 Ev.Value("BOT_HISTORY") & "/data/ai/pong/2nd/back/win/") ;
    Num_Real_Runners : Integer := 0;
    Pricets     : Calendar2.Time_Type := Calendar2.Time_Type_First;

    type Print_Data_Type is record
      Backprice : Fixed_Type := 0.0;
      Layprice  : Fixed_Type := 0.0;
      Selectionid : Integer_4 := 0;
      Sortprio : Integer_4 := 0;
      Back_Reward : Fixed_Type := 0.0;
      Lay_Reward : Fixed_Type := 0.0;
    end record;

    type Data_Type is array(1..16) of Print_Data_Type;
  --  Old_Data    : Data_Type;
    Data        : Data_Type;
    Marketid    : Bot_Types.Marketid_Type := (others => ' ');
  --  Markettype  : Bot_Types.Markettype_Type := (others => ' ');
    Marketname  : Bot_Types.Marketname_Type := (others => ' ');

    --  Lowest_2nd  : Fixed_Type := 1_000_000.0 +1.0;
    --  Lowest_1st  : Fixed_Type := 1_000_000.0;
    --  Selid_1st   : Integer_4  := 0;
    --  Selid_2nd   : Integer_4  := 0;
    --  Python_1st  : Integer    := -1;  --number (idx) in python (zero-based) array
    --  Python_2nd  : Integer    := -1;  --number (idx) in python (zero-based) array


    ------------------------------------------------
    procedure Do_Print_Line(F : Text_Io.File_Type) is
    begin
                                        --0-based idx
      for I in Winners'Range loop
        Text_Io.Put(F, Winners(I)'Img);  --0-0
        Text_Io.Put(F, ",");
        exit;
      end loop;

      --  Text_Io.Put(F, Markettype(1));      --3
      --  Text_Io.Put(F, ",");
      --  Text_Io.Put(F, Num_Real_Runners'Img);  --4
      --  Text_Io.Put(F, ",");
      --  Text_Io.Put(F, Marketid);             --5
      --  Text_Io.Put(F, ",");


      --  case Ia_Position is
      --  when 1 =>
      --    Text_Io.Put(F, Float'Image(Float(Lowest_1st)));  --6
      --    Text_Io.Put(F, ",");
      --    Text_Io.Put(F, Selid_1st'Img);           --7
      --    Text_Io.Put(F, ",");
      --    Text_Io.Put(F, Python_1st'Img);       --8
      --    Text_Io.Put(F, ",");
      --  when 2 =>
      --    Text_Io.Put(F, Float'Image(Float(Lowest_2nd))); --6
      --    Text_Io.Put(F, ",");
      --    Text_Io.Put(F, Selid_2nd'Img);    --7
      --    Text_Io.Put(F, ",");
      --    Text_Io.Put(F, Python_2nd'Img);  --8
      --    Text_Io.Put(F, ",");
      --  when others =>
      --    raise Constraint_Error with "bad position - not supported" & Ia_Position'Img;
      --  end case;

      for I in Data'Range loop
        Text_Io.Put(F, Data(I).Selectionid'img);   --1-16
        Text_Io.Put(F, ",");
      end loop;

      for I in Data'Range loop  --17-32
        if Ba_Layprice then
          Text_Io.Put(F, Float'Image(Float(Data(I).Layprice)));
        else
          Text_Io.Put(F, Float'Image(Float(Data(I).Backprice)));
        end if;
        Text_Io.Put(F, ",");
      end loop;


      for I in Data'Range loop   --33-48
        if Ba_Layprice then
          Text_Io.Put(F, Float'Image(Float(Data(I).Lay_Reward)));
        else
          Text_Io.Put(F, Float'Image(Float(Data(I).Back_Reward)));
        end if;

        if I = Data'Last then
          -- put here anything after the last array
          Text_Io.Put(F, "," & Pricets.String_Date_Time_Iso(T => " ", Tz => "")); --51
          Text_Io.Put_Line(F, "");  -- <-- last statement on this row
        else
          Text_Io.Put(F, ",");
        end if;
      end loop;



      --  for I in Data'Range loop   --35-50
      --    if Ba_Layprice then
      --      Text_Io.Put(F, Float'Image(Float(Data(I).Layprice - Old_Data(I).Layprice)));
      --    else
      --      Text_Io.Put(F, Float'Image(Float(Data(I).Backprice - Old_Data(I).Backprice)));
      --    end if;
      --
      --    if I = Data'Last then
      --      -- put here anything after the last array
      --      Text_Io.Put(F, "," & Pricets.String_Date_Time_Iso(T => " ", Tz => "")); --51
      --      Text_Io.Put_Line(F, "");  -- <-- last statement on this row
      --      Old_Data := Data;
      --    else
      --      Text_Io.Put(F, ",");
      --    end if;
      --  end loop;
    end Do_Print_Line;
      ------------------------------------------------

    function Fix_Path(p : string) return string is
      lp : string := Trim(p,right);
    begin
      for i in lp'range loop
        case lp(i) is
          when ' ' => lp(i) := '_';
          when others => null;
        end case;
      end loop;
      return lp;
    end Fix_Path;
    ---------------------------------

  begin
     Text_Io.Put_Line(Text_Io.Standard_Error,"length list" & L.Length'Img);

    -- winner but use placeindex instead to get 1-16 for nn and Python uses 0-based arrays
    for R of L loop
      if R.Runner.Status(1) = 'W' then  -- fix for Place later on
        Winners(1) := R.Runner.Selectionid;
        Winners(2) := R.Runner.Selectionid;
        Winners(3) := R.Runner.Selectionid;
        Pricets := R.Price.Pricets;
        Marketid := R.Runner.Marketid;
--        Markettype := R.Market.Markettype;
        Marketname := R.Market.Marketname;
        exit;
      end if;
    end loop;

    --count runners
    for R of L loop
      exit when Pricets /= R.Price.Pricets;
      Num_Real_Runners := Num_Real_Runners + 1;
    end loop;

    Text_Io.Put_Line(Text_Io.Standard_Error,"open '" & Marketid & "'");

    case Ia_Position is
      when 1 =>
        if Ba_Train_Set then
          declare
            Path : String := Fix_Path(Path1 & "train/" & Marketname) ;
          begin
            if not AD.Exists(Path) then
              Ad.Create_Path(Path);
            end if;
            Text_Io.Create(F,Text_Io.Out_File, Path & "/" & Marketid & ".csv");
          end;
        else
          declare
            Path : String := Fix_Path(Path1 & "sample/" & Marketname);
          begin
            if not AD.Exists(Path) then
              Ad.Create_Path(Path);
            end if;
            Text_Io.Create(F,Text_Io.Out_File, Path & "/" & Marketid & ".csv");
          end;
        end if;
      when 2 =>
        if Ba_Train_Set then
          declare
            Path : String := Fix_Path(Path2 & "train/" & Marketname) ;
          begin
            if not AD.Exists(Path) then
              Ad.Create_Path(Path);
            end if;
            Text_Io.Create(F,Text_Io.Out_File, Path & "/" & Marketid & ".csv");
          end;
        else
          declare
            Path : String := Fix_Path(Path2 & "sample/" & Marketname) ;
          begin
            if not AD.Exists(Path) then
              Ad.Create_Path(Path);
            end if;
            Text_Io.Create(F,Text_Io.Out_File, Path & "/" & Marketid & ".csv");
          end;
        end if;
      when others =>
        raise Constraint_Error with "bad position - not supported" & Ia_Position'Img;
    end case;

    for R of L loop
      Cnt := Cnt + 1;

      Data(Cnt).Backprice := R.Price.Backprice;
      Data(Cnt).Layprice  := R.Price.Layprice;
      Data(Cnt).Selectionid := R.Runner.Selectionid;
      Data(Cnt).Sortprio := R.Runner.Sortprio;
      Data(Cnt).Back_Reward := R.Back_Reward.Profit;
      Data(Cnt).Lay_Reward := R.Lay_Reward.Profit;

      --  case Ia_Position is
      --  when 1 =>
      --    if Ba_Layprice then
      --      if Data(Cnt).Layprice > 0.0
      --        and then Data(Cnt).Layprice < Lowest_1st then
      --        Lowest_1st := Data(Cnt).Layprice;
      --        Selid_1st := Data(Cnt).Selectionid;
      --        Python_1st := Cnt -1;  --number (idx) in python (zero-based) array
      --      end if;
      --    else--back
      --      if Data(Cnt).Backprice > 0.0
      --        and then Data(Cnt).Backprice < Lowest_1st then
      --        Lowest_1st := Data(Cnt).Backprice;
      --        Selid_1st := Data(Cnt).Selectionid;
      --        Python_1st := Cnt -1;  --number (idx) in python (zero-based) array
      --      end if;
      --    end if;
      --
      --
      --  when 2 =>
      --    if Ba_Layprice then
      --
      --      if Data(Cnt).Layprice > 0.0
      --        and then Data(Cnt).Layprice < Lowest_1st
      --      then
      --        Lowest_1st := Data(Cnt).Layprice;
      --        Selid_1st := Data(Cnt).Selectionid;
      --        Python_1st := Cnt -1;  --number (idx) in python (zero-based) array
      --      end if;
      --
      --      if Data(Cnt).Layprice > 0.0
      --        and then Data(Cnt).Layprice >= Lowest_1st
      --        and then Data(Cnt).Layprice < Lowest_2nd
      --      then
      --        Lowest_2nd := Data(Cnt).Layprice;
      --        Selid_2nd := Data(Cnt).Selectionid;
      --        Python_2nd := Cnt -1;  --number (idx) in python (zero-based) array
      --      end if;
      --    else --back
      --      if Data(Cnt).Backprice > 0.0
      --        and then Data(Cnt).Backprice < Lowest_1st
      --      then
      --        Lowest_1st := Data(Cnt).Backprice;
      --        Selid_1st := Data(Cnt).Selectionid;
      --        Python_1st := Cnt -1;  --number (idx) in python (zero-based) array
      --      end if;
      --
      --      if Data(Cnt).Backprice > 0.0
      --        and then Data(Cnt).Backprice >= Lowest_1st
      --        and then Data(Cnt).Backprice < Lowest_2nd
      --      then
      --        Lowest_2nd := Data(Cnt).Backprice;
      --        Selid_2nd := Data(Cnt).Selectionid;
      --        Python_2nd := Cnt -1;  --number (idx) in python (zero-based) array
      --      end if;
      --    end if;
      --
      --  when others =>
      --    raise Constraint_Error with "bad position - not supported" & Ia_Position'Img;
      --  end case;

      if Cnt = Num_Real_Runners then
        Pricets := R.Price.Pricets; -- update to this line's pricets
        Do_Print_Line(F);
        Cnt := 0;
        --  Lowest_1st := 1_000_000.0;
        --  Selid_1st   := 0;
        --  Python_1st  := -1;
        --  Lowest_2nd := 1_000_000.0 +1.0;
        --  Selid_2nd   := 0;
        --  Python_2nd  := -1;
      end if;

    end loop;

    Text_Io.Put_Line(Text_Io.Standard_Error,"close file");
    Text_Io.Close(F);
  end Print;

  --------------------------------------------------------
  procedure Get_Runner_Data(Market_Data : Table_Amarkets.Data_Type) is
    --history
    type Eos_Type is (price,market,Runner,Back_Reward,Lay_Reward);
    Eos                : array(Eos_Type'range) of Boolean := (others => False);
    R_List             : R_Pkg.List;
    R_Data             : R_Type;
    Mkt                : Markets.Market_Type;
  begin
    Mkt.Marketname := Market_Data.Marketname;
    if not Mkt.Marketname_Ok then
    --  Debug("bad name: " & Mkt.Marketname );
      return;
    end if;

    R_Data.Market.Marketid := Market_Data.Marketid;
    R_Data.Market.Read(Eos(market));

    Select_Runner_With_Price.Prepare("select P.* " &
                                       "from ARUNNERS R, APRICES P " &
                                       "where 1 = 1 " &
                                       "and P.MARKETID = R.MARKETID " &
                                       "and P.MARKETID = :MARKETID " &
                                       "and P.SELECTIONID = R.SELECTIONID " &
                                       "and R.STATUS <> 'REMOVED' " &
                                       "order by P.PRICETS, R.SORTPRIO " );

    Select_Runner_With_Price.Set("MARKETID", Market_Data.Marketid);
    Select_Runner_With_Price.Open_Cursor;
    loop
      Select_Runner_With_Price.Fetch(Eos(price));
      exit when Eos(price);

      R_Data.Price := Table_Aprices.Get(Select_Runner_With_Price);
      R_Data.Runner.Marketid := R_Data.Price.Marketid;
      R_Data.Runner.Selectionid := R_Data.Price.Selectionid;

      R_Data.Runner.Read(Eos(Runner));

      R_Data.Back_Reward.Marketid := R_Data.Price.Marketid;
      R_Data.Back_Reward.Selectionid := R_Data.Price.Selectionid;
      R_Data.Back_Reward.Pricets := R_Data.Price.Pricets;

      R_Data.Back_Reward.Side := "BACK";
      R_Data.Back_Reward.Read(Eos(Back_Reward));


      R_Data.Lay_Reward.Marketid := R_Data.Price.Marketid;
      R_Data.Lay_Reward.Selectionid := R_Data.Price.Selectionid;
      R_Data.Lay_Reward.Pricets := R_Data.Price.Pricets;

      R_Data.Lay_Reward.Side := "LAY ";
      R_Data.Lay_Reward.Read(Eos(Lay_Reward));

      R_List.Append(R_Data);
    end loop;
    Select_Runner_With_Price.Close_Cursor;
  --  if Integer(R_List.Length) > 500 then
    Print(R_List);
 --   else
    --  Text_Io.Put_Line(Text_Io.Standard_Error,Market_Data.Marketid & " had only" &  R_List.Length'Img & " lines");
  --  end if;

  end Get_Runner_Data;
  ------------------------------------------------------

  procedure Get_Market_Data(Market_List  : in out Table_Amarkets.Amarkets_List_Pack2.List) is

  begin

    if Ba_Train_Set then
      Select_Markets.Prepare( "select M.* " &
                                "from AMARKETS M " &
                                "where true " &
                                "and M.MARKETTYPE = 'WIN' " &
                                "and M.NUMACTIVERUNNERS >= 8 " &
                                "and M.NUMACTIVERUNNERS <= 16 " &
                                "and M.STARTTS >= :STARTDATE " &
                              --  "and m.marketid = '1.151619897' " &
                                "and M.EVENTID not like '%2' " & --use the ones that and with 2 as test sample
                                "order by M.STARTTS");
    else -- to verify with - just 10 %
      Select_Markets.Prepare("select M.* " &
                               "from AMARKETS M " &
                               "where true " &
                               "and M.MARKETTYPE = 'WIN' " &
                               "and M.NUMACTIVERUNNERS >= 8 " &
                               "and M.NUMACTIVERUNNERS <= 16 " &
                                "and M.STARTTS >= :STARTDATE " &
                               "and M.EVENTID like '%2' " & --use the ones that and with 2 as test sample
                               "order by M.STARTTS");
    end if;
    Select_Markets.Set("STARTDATE", Global_Start_Date);
    Table_Amarkets.Read_List(Select_Markets, Market_List);
  end Get_Market_Data;
  ------------------------------------------------------

  Mlist  :  Table_Amarkets.Amarkets_List_Pack2.List;

begin

  Define_Switch
    (Cmd_Line,
     Sa_Startdate'Access,
     Long_Switch => "--startdate=",
     Help        => "startdate");

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

 Define_Switch
    (Cmd_Line,
     Ia_Position'Access,
     Long_Switch => "--position=",
     Help        => "lay/back 1=leader, 2=2nd etc");

  Getopt (Cmd_Line);  -- process the command line

  if Sa_Startdate.all /= "" then
    declare
      S : String (1 .. Sa_Startdate.all'Length) := Sa_Startdate.all;
    begin
      Global_Start_Date.Year := Year_Type'Value(S(1..4));
      Global_Start_Date.Month := Month_Type'Value(S(6..7));
      Global_Start_Date.Day := Day_Type'Value(S(9..10));
    end;
  else
    Global_Start_Date := (2018,11,15,0,0,0,0);
  end if;

  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");

  Debug("Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database","host",""),
     Port     => Ini.Get_Value("database","port", 5432),
     Db_Name  => Ini.Get_Value("database","name",""),
     Login    => Ini.Get_Value("database","username",""),
     Password => Ini.Get_Value("database","password",""),
     Ssl_Mode => "prefer");
  Debug("db Connected");

  T.Start;
  Get_Market_Data(Mlist);

  for M of Mlist loop
    Get_Runner_Data(M);
  end loop;

  T.Commit;
  Sql.Close_Session;

exception
  when GNAT.COMMAND_LINE.EXIT_FROM_COMMAND_LINE =>
    null;
  when E: others =>
    Stacktrace.Tracebackinfo(E);

end Ai_Nn_price;
