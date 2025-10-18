with Gnat.Command_Line; use Gnat.Command_Line;
with Types;    use Types;
with Gnat.Strings;
with Sql;
with Calendar2; use Calendar2;
with Logging;               use Logging;
with Text_IO;
with Ini;
with Ada.Containers.Doubly_Linked_Lists;
with  Ada.Environment_Variables;
--with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
--with Bot_Types;
with Utils; use Utils;
with Tics;

procedure Graph_Data_3 is
  package EV renames Ada.Environment_Variables;
  Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type;
  Select_Graph_Data1    : Sql.Statement_Type;

  Sa_Betname            : aliased Gnat.Strings.String_Access;
  Sa_Logfilename        : aliased Gnat.Strings.String_Access;
  Sa_Percent            : aliased Gnat.Strings.String_Access;
  Sa_Markettype         : aliased Gnat.Strings.String_Access;

  Ia_Runner             : aliased Integer := 0;

  GDebug      : Boolean := False;

  type Graph_Data_Type is record
    Cnt     : Integer_4 := 0;
    Odds    : Fixed_Type := 0.0;
    Hitrate : Fixed_Type := 0.0;
    Tic     : Tics.Tics_Type;
  end record;

  package Graph_Data_Pack is new Ada.Containers.Doubly_Linked_Lists(Graph_Data_Type);
  Winners_list : Graph_Data_Pack.List;
  Loosers_List : Graph_Data_Pack.List;
  Grand_List : Graph_Data_Pack.List;


  -------------------------------
  procedure Debug (What : String) is
  begin
    if GDebug then
      Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_ISO (Clock, " " , "") & " " & What);
    end if;
  end Debug;
  pragma Warnings (Off, Debug);
  -------------------------------
  procedure Print (What : String) is
  begin
    Text_Io.Put_Line (What);
  end Print;
  -------------------------------



  procedure Graph_Data(Percent    : in Fixed_Type;
                       Markettype : in String ;
                       Runner     : in Integer ;
                       List       : out Graph_Data_Pack.List ;
                       Won        : in Boolean) is
     Gd : Graph_Data_Type;
     Eos : Boolean := False;

  begin

    if Runner = 1 then

      if Markettype = "WIN" then
        Select_Graph_Data1.Prepare ("select count('a') cnt,r1pricewin odds " &
                                       "from AMIDRACE where r1wonwin = :WON and fraction = :FRACTION " &
                                       "group by r1pricewin " &
                                       "order by r1pricewin");
      elsif Markettype = "PLC" then
        Select_Graph_Data1.Prepare ("select count('a') cnt,r1priceplc odds " &
                                    "from AMIDRACE where r1wonplc = :WON and fraction = :FRACTION " &
                                    "group by r1priceplc " &
                                    "order by r1priceplc");
      else
        raise Constraint_Error with "bad markettype 1'" & Markettype & "'";
      end if;

    elsif Runner = 2 then

      if Markettype = "WIN" then
        Select_Graph_Data1.Prepare ("select count('a') cnt,r2pricewin odds " &
                                       "from AMIDRACE where r2wonwin = :WON and fraction = :FRACTION " &
                                       "group by r2pricewin " &
                                       "order by r2pricewin");
      elsif Markettype = "PLC" then
        Select_Graph_Data1.Prepare ("select count('a') cnt,r2priceplc odds " &
                                    "from AMIDRACE where r2wonplc = :WON and fraction = :FRACTION " &
                                    "group by r2priceplc " &
                                    "order by r2priceplc");
      else
        raise Constraint_Error with "bad markettype 2'" & Markettype & "'";
      end if;

    elsif Runner = 3 then

      if Markettype = "WIN" then
        Select_Graph_Data1.Prepare ("select count('a') cnt,r3pricewin odds " &
                                       "from AMIDRACE where r3wonwin = :WON and fraction = :FRACTION " &
                                       "group by r3pricewin " &
                                       "order by r3pricewin");
      elsif Markettype = "PLC" then
        Select_Graph_Data1.Prepare ("select count('a') cnt,r3priceplc odds " &
                                    "from AMIDRACE where r3wonplc = :WON and fraction = :FRACTION " &
                                    "group by r3priceplc " &
                                    "order by r3priceplc");
      else
        raise Constraint_Error with "bad markettype 3'" & Markettype & "'";
      end if;

    else
      raise Constraint_Error with "bad runner" & Runner'Img ;
    end if;


    Select_Graph_Data1.Set ("WON", Won);
    Select_Graph_Data1.Set ("FRACTION", Percent);
    Select_Graph_Data1.Open_Cursor;
    loop
      Select_Graph_Data1.Fetch (Eos);
      exit when Eos;
      Select_Graph_Data1.Get ("cnt", Gd.Cnt);
      Select_Graph_Data1.Get ("odds", Gd.Odds);
      List.Append (Gd);
    end loop;
    Select_Graph_Data1.Close_Cursor;

  end Graph_Data;

  ------------------------------------------------------



begin
  Define_Switch
    (Cmd_Line,
     Sa_Logfilename'Access,
     Long_Switch => "--logfile=",
     Help        => "name of log file");

  Define_Switch
    (Cmd_Line,
     Sa_Betname'Access,
     Long_Switch => "--betname=",
     Help        => "betname");

  Define_Switch
    (Cmd_Line,
     Ia_Runner'Access,
     Long_Switch => "--runner=",
     Help        => "runner, 1,2,3,13,12 1=1,..,13=1-3, 12=1-2" );

  Define_Switch
    (Cmd_Line,
     Sa_Markettype'Access,
     Long_Switch => "--markettype=",
     Help        => "PLC,WIN");

  Define_Switch
    (Cmd_Line,
     Sa_Percent'Access,
     Long_Switch => "--percent=",
     Help        => "percent of racetime run");


  Getopt (Cmd_Line);  -- process the command line


  if not Ev.Exists("BOT_NAME") then
    Ev.Set("BOT_NAME","graph_data_3");
  end if;

  Logging.Open(Ev.Value("BOT_HOME") & "/log/" & Sa_Logfilename.all & ".log");

  Ini.Load (Ev.Value ("BOT_HOME") & "/login.ini");

  Debug ("Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("local", "host", ""),
     Port     => Ini.Get_Value("local", "port", 5432),
     Db_Name  => Ini.Get_Value("local", "name", ""),
     Login    => Ini.Get_Value("local", "username", ""),
     Password => Ini.Get_Value("local", "password", ""));
  Debug ("db Connected");

  Log("parameters start");
  Log("runner" & Ia_Runner'img);
  Log("markettype " & Sa_markettype.all);
  Log("percent " & Sa_percent.all);

  Log("parameters stop");

  T.Start;

  Graph_Data(Percent    => Fixed_Type'Value(Sa_Percent.all),
             Markettype => Sa_Markettype.all,
             Runner     => Ia_Runner,
             List       => Winners_List,
             Won        => True);

  Graph_Data(Percent    => Fixed_Type'Value(Sa_Percent.all),
             Markettype => Sa_Markettype.all,
             Runner     => Ia_Runner,
             List       => Loosers_List,
             Won        => False);

  T.Commit;
  Sql.Close_Session;



  for Looser of Loosers_List loop
    for Winner of Winners_List loop
      if Winner.Odds = Looser.Odds then
        declare
         Gt : Graph_Data_Type;
        begin
          Gt.Cnt := Winner.Cnt + Looser.Cnt;
          Gt.Odds := Winner.Odds;
          Gt.Hitrate := Fixed_Type(Winner.Cnt) / Fixed_Type(Gt.Cnt);
          Gt.Tic := Tics.Get_Tic_Index(Price => Gt.Odds);
          Grand_List.Append(Gt);
        end;
      end if;
    end loop;
  end loop;


  for Gt of Grand_List loop
    if Gt.Cnt >= 1 then
      Print (
             Gt.Cnt'Img   & " | " &
               F8_Image (Gt.Odds)  & " | " &
               Gt.Tic'Img  & " | " &
               F8_Image (Gt.Hitrate) & " | " &
               F8_Image (1.0 - Gt.Hitrate)
            ) ;
    end if;
  end loop;


end Graph_Data_3;
