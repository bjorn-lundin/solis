with Gnat.Command_Line; use Gnat.Command_Line;
with Types;    use Types;
with Gnat.Strings;
with Sql;
with Calendar2; use Calendar2;
--with Logging;               use Logging;
with Text_IO;
with Ini;
with Ada.Containers.Doubly_Linked_Lists;
with  Ada.Environment_Variables;
--with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
--with Bot_Types;
with Utils; use Utils;
with Config;
with Bot_Types;
procedure Graph_Data_Ael is
  package EV renames Ada.Environment_Variables;
  Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type;
  Select_Lapsed_Date    : Sql.Statement_Type;
  Select_Profit_Date    : Sql.Statement_Type;
  Select_Avg_Price_Date : Sql.Statement_Type;
  Select_Equity_Date    : Sql.Statement_Type;
  Select_Histogram_Data : Sql.Statement_Type;

  Sa_Betname           : aliased Gnat.Strings.String_Access;
  Sa_Betname1          : aliased Gnat.Strings.String_Access;
  Sa_Betname2          : aliased Gnat.Strings.String_Access;
  Sa_Side              : aliased Gnat.Strings.String_Access;
  Sa_Price             : aliased Gnat.Strings.String_Access;
  Sa_Min_Price_Matched : aliased Gnat.Strings.String_Access;
  Ba_Print_Strategies  : aliased Boolean := False;
  Ba_Avg_Price         : aliased Boolean := False;
  Ba_Profit            : aliased Boolean := False;
  Ba_Lapsed            : aliased Boolean := False;
  Ba_Equity            : aliased Boolean := False;
  Ba_Equity2           : aliased Boolean := False;
  Ba_Histogram         : aliased Boolean := False;
  Ba_Noloss           : aliased Boolean := False;

  Ia_Days             : aliased Integer := 42;
  Ia_Maxprice         : aliased Integer := 1000;

  GDebug      : Boolean := TRue;
  One_Hundred : constant Fixed_Type := 100.0;
  type Days_Result_Type is record
    Lapsed       : Integer_4 := 0;
    Settled      : Integer_4 := 0;
    Ts           : Calendar2.Time_Type := Calendar2.Time_Type_First;
  end record;

  type Profit_Result_Type is record
    Profit       : Fixed_Type   := 0.0;
    Size_Matched : Fixed_Type   := 0.0;
    Ts           : Calendar2.Time_Type := Calendar2.Time_Type_First;
  end record;

  type Avg_Price_Result_Type is record
    Avg_Price    : Fixed_Type   := 0.0;
    --  Size_Matched : Fixed_Type   := 0.0;
    Ts           : Calendar2.Time_Type := Calendar2.Time_Type_First;
  end record;

  type Equity_Result_Type is record
    Ts           : Calendar2.Time_Type := Calendar2.Time_Type_First;
    Equity       : Fixed_Type := 0.0;
  end record;

  type Histogram_Result_Type is record
    Name       : String_Object;
    Profit     : Fixed_Type := 0.0;
  end record;


  package Days_Result_Pack is new Ada.Containers.Doubly_Linked_Lists (Days_Result_Type);
  Days_Result_List   : Days_Result_Pack.List;

  package Profit_Result_Pack is new Ada.Containers.Doubly_Linked_Lists (Profit_Result_Type);
  Profit_Result_List   : Profit_Result_Pack.List;

  package Avg_Price_Result_Pack is new Ada.Containers.Doubly_Linked_Lists (Avg_Price_Result_Type);
  Avg_Price_Result_List   : Avg_Price_Result_Pack.List;

  package Equity_Result_Pack is new Ada.Containers.Doubly_Linked_Lists (Equity_Result_Type);
  Equity_Result_List   : Equity_Result_Pack.List;

  package Histogram_Result_Pack is new Ada.Containers.Doubly_Linked_Lists (Histogram_Result_Type);
  Histogram_Result_List   : Histogram_Result_Pack.List;

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

  procedure Day_Statistics_Lapsed_Vs_Settled (
                                              Betname : in     String;
                                              Days    : in     Integer_4;
                                              A_List  : in out Days_Result_Pack.List) is
    Eos                : Boolean := False;
    Days_Result_Record : Days_Result_Type;
  begin
    Select_Lapsed_Date.Prepare (
                                "select count('a'), STARTTS::date " &
                                  "from ABETS " &
                                  "where BETNAME = :BETNAME " &
                                  "and STATUS = :STATUS " &
                                  "and STARTTS::date > (select CURRENT_DATE - interval ':SOME days') " &
                                  "group by STARTTS::date " &
                                  "order by STARTTS::date " );

    Select_Lapsed_Date.Set ("BETNAME", Betname);
    Select_Lapsed_Date.Set ("STATUS", "SETTLED");
    Select_Lapsed_Date.Set ("SOME", Days);

    Select_Lapsed_Date.Open_Cursor;
    loop
      Select_Lapsed_Date.Fetch (Eos);
      exit when Eos;
      Select_Lapsed_Date.Get (1, Days_Result_Record.Settled);
      Select_Lapsed_Date.Get_Date (2, Days_Result_Record.Ts);
      A_List.Append (Days_Result_Record);
    end loop;
    Select_Lapsed_Date.Close_Cursor;

    Select_Lapsed_Date.Set ("STATUS", "LAPSED");
    Select_Lapsed_Date.Open_Cursor;
    loop
      Select_Lapsed_Date.Fetch (Eos);
      exit when Eos;
      Select_Lapsed_Date.Get (1, Days_Result_Record.Lapsed);
      Select_Lapsed_Date.Get_Date (2, Days_Result_Record.Ts);

      for R of A_List loop
        if R.Ts = Days_Result_Record.Ts then
          R.Lapsed := Days_Result_Record.Lapsed;
        end if;
      end loop;
    end loop;
    Select_Lapsed_Date.Close_Cursor;
  end Day_Statistics_Lapsed_Vs_Settled;

  --------------------------------------------------------
  procedure Day_Statistics_Profit_Vs_Matched (
                                              Betname : in     String;
                                              Days    : in     Integer_4;
                                              A_List  : in out Profit_Result_Pack.List) is
    Eos                  : Boolean := False;
    Profit_Result_Record : Profit_Result_Type;
  begin
    Select_Profit_Date.Prepare (
                                "select sum(PROFIT), sum(SIZEMATCHED), STARTTS::date " &
                                  "from ABETS " &
                                  "where BETNAME = :BETNAME " &
                                  "and STATUS = :STATUS " &
                                  "and STARTTS::date > (select CURRENT_DATE - interval ':SOME days') " &
                                  "group by STARTTS::date " &
                                  "order by STARTTS::date " );

    Select_Profit_Date.Set ("BETNAME", Betname);
    Select_Profit_Date.Set ("STATUS", "SETTLED");
    Select_Profit_Date.Set ("SOME", Days);

    Select_Profit_Date.Open_Cursor;
    loop
      Select_Profit_Date.Fetch (Eos);
      exit when Eos;
      Select_Profit_Date.Get (1, Profit_Result_Record.Profit);
      Select_Profit_Date.Get (2, Profit_Result_Record.Size_Matched);
      Select_Profit_Date.Get_Date (3, Profit_Result_Record.Ts);
      A_List.Append (Profit_Result_Record);
    end loop;
    Select_Profit_Date.Close_Cursor;
  end Day_Statistics_Profit_Vs_Matched;
  ------------------------------------------------------

  --------------------------------------------------------
  procedure Avg_Price_For_Settled_Bets (
                                        Betname : in     String;
                                        Days    : in     Integer_4;
                                        A_List  : in out Avg_Price_Result_Pack.List) is
    Eos                     : Boolean := False;
    Avg_Price_Result_Record : Avg_Price_Result_Type;
  begin
    Select_Avg_Price_Date.Prepare (
                                   "select BETNAME, avg(B.PRICEMATCHED) as AVGODDS, B.STARTTS::date as DATE " &
                                     "from ABETS B " &
                                     "where B.BETNAME = :BETNAME " &
                                     "and B.BETWON " &
                                     "and STATUS = :STATUS " &
                                     "and B.STARTTS >= (select CURRENT_DATE - interval ':SOME days') " &
                                     "and extract(year from B.STARTTS) = extract(year from (select CURRENT_DATE )) " &
                                     "group by BETNAME, B.STARTTS::date " &
                                     "order by B.STARTTS::date, BETNAME");

    Select_Avg_Price_Date.Set ("BETNAME", Betname);
    Select_Avg_Price_Date.Set ("STATUS", "SETTLED");
    Select_Avg_Price_Date.Set ("SOME", Days);

    Select_Avg_Price_Date.Open_Cursor;
    loop
      Select_Avg_Price_Date.Fetch (Eos);
      exit when Eos;
      Select_Avg_Price_Date.Get ("AVGODDS", Avg_Price_Result_Record.Avg_Price);
      Select_Avg_Price_Date.Get_Date ("DATE", Avg_Price_Result_Record.Ts);
      A_List.Append (Avg_Price_Result_Record);
    end loop;
    Select_Avg_Price_Date.Close_Cursor;
  end Avg_Price_For_Settled_Bets;
  ------------------------------------------------------

  --------------------------------------------------------
  procedure Equity_Data2 (
                         Betname1,Betname2 : in     String;
                         A_List  : in out Equity_Result_Pack.List) is
    Eos           : Boolean := False;
    Equity_Result : Equity_Result_Type;
    Profit        : Fixed_Type := 0.0;
  begin
    Select_Equity_Date.Prepare (
                                "select B.STARTTS, PROFIT " &
                                  "from ABETS B " &
                                  "where true " &
                                  "and B.BETNAME >= :BETNAME1 " &
                                  "and B.BETNAME <= :BETNAME2 " &
                                  "and B.STATUS ='M' " &
                                  "order by STARTTS");
    Select_Equity_Date.Set ("BETNAME1", Betname1);
    Select_Equity_Date.Set ("BETNAME2", Betname2);
    Select_Equity_Date.Open_Cursor;
    loop
      Select_Equity_Date.Fetch (Eos);
      exit when Eos;
      Select_Equity_Date.Get ("STARTTS", Equity_Result.Ts);
      Select_Equity_Date.Get ("PROFIT", Profit);
      Equity_Result.Equity := Equity_Result.Equity + Profit;
      A_List.Append (Equity_Result);
    end loop;
    Select_Equity_Date.Close_Cursor;
  end Equity_Data2;
  --------------------------------------------------------
  procedure Equity_Data (
                         Betname : in     String;
                         A_List  : in out Equity_Result_Pack.List) is
    Eos               : Boolean := False;
    Equity_Result     : Equity_Result_Type;
    Profit            : Fixed_Type := 0.0;
    Min_Price_Matched : Fixed_Type := 1.01;
   -- Side              : String(1..4) := "BACK";
    Curr_Dat          : Calendar2.Time_Type := Calendar2.Time_Type_First;
  begin
    Select_Equity_Date.Prepare (
                                "select B.STARTTS, B.PROFIT " &
                                  "from ABETS B " &
                                  "where true " &
                               --   "and B.SIDE = :SIDE " &
                                  "and B.BETNAME = :BETNAME " &
                                  "and B.STATUS in ('SETTLED','MATCHED') " &
                                  "and B.PRICEMATCHED >= :PRICEMATCHED " &
                                  "order by B.STARTTS");
    Select_Equity_Date.Set ("BETNAME", Betname);

    if Sa_Min_Price_Matched.all /= "" then
      Min_Price_Matched := Fixed_Type'Value(Sa_Min_Price_Matched.all);
    end if;
    Select_Equity_Date.Set ("PRICEMATCHED", Min_Price_Matched);

--    if Sa_Side.all /= "" then
--      Select_Equity_Date.Set ("SIDE", Sa_Side.all);
--    else
--      Select_Equity_Date.Set ("SIDE", Side);
--    end if;

    Select_Equity_Date.Open_Cursor;
    loop
      Select_Equity_Date.Fetch (Eos);
      exit when Eos;
      Select_Equity_Date.Get ("STARTTS", Equity_Result.Ts);
      Select_Equity_Date.Get ("PROFIT", Profit);

      if Ba_Noloss then
        if Profit < 0.0 then
          Equity_Result.Equity := Equity_Result.Equity + Profit; --add the bad result
          A_List.Append (Equity_Result);
          Curr_Dat := Equity_Result.Ts;                          --stop the rest of this day
        end if;

        if Curr_Dat.Year = Equity_Result.Ts.Year and then
          Curr_Dat.Month = Equity_Result.Ts.Month and then
          Curr_Dat.Day = Equity_Result.Ts.Day then
          null; -- bad day
        else
          Equity_Result.Equity := Equity_Result.Equity + Profit;
          A_List.Append (Equity_Result);
        end if;
      else
        Equity_Result.Equity := Equity_Result.Equity + Profit;
        A_List.Append (Equity_Result);
      end if;

    end loop;
    Select_Equity_Date.Close_Cursor;
  end Equity_Data;
  --------------------------------------------------------
  procedure Histogram_Data (
                         Betname : in     String;
                         A_List  : in out  Histogram_Result_Pack.List) is
    Eos                : Boolean := False;
    Histogram_Result   : Histogram_Result_Type;
--    Profit        : Fixed_Type := 0.0;
    Name : Bot_Types.Betname_Type := (others => ' ');
  begin
    Select_Histogram_Data.Prepare (
                                "select B.BETNAME, Sum(B.PROFIT) profit " &
                                  "from ABETS B " &
                                  "where true " &
                                  "and B.BETNAME like :BETNAME " &
                                  "and B.STATUS ='M' " &
                                  "group by B.BETNAME " &
                                  "order by B.BETNAME");
    Select_Histogram_Data.Set ("BETNAME", '%' & Betname & '%');
    Select_Histogram_Data.Open_Cursor;
    loop
      Select_Histogram_Data.Fetch (Eos);
      exit when Eos;
      Select_Histogram_Data.Get ("BETNAME", Name);
      Select_Histogram_Data.Get ("PROFIT", Histogram_Result.Profit);
      Histogram_Result.Name.Set(Name);
      A_List.Append (Histogram_Result);
    end loop;
    Select_Histogram_Data.Close_Cursor;
  end Histogram_Data ;
  ------------------------------------------------------

begin
  Define_Switch
    (Cmd_Line,
     Sa_Betname1'Access,
     Long_Switch => "--betname1=",
     Help        => "betname1 for equity (use with betname2)");

  Define_Switch
    (Cmd_Line,
     Sa_Betname2'Access,
     Long_Switch => "--betname2=",
     Help        => "betname2 for equity (use with betname1)");

  Define_Switch
    (Cmd_Line,
     Sa_Betname'Access,
     Long_Switch => "--betname=",
     Help        => "betname for equity");

  Define_Switch
    (Cmd_Line,
     Ba_Equity'Access,
     Long_Switch => "--equity",
     Help        => "equity diagram");

  Define_Switch
    (Cmd_Line,
     Ba_Equity2'Access,
     Long_Switch => "--equity2",
     Help        => "equity diagram");

  Define_Switch
    (Cmd_Line,
     Ba_Profit'Access,
     Long_Switch => "--profit",
     Help        => "profit stats");

  Define_Switch
    (Cmd_Line,
     Ba_Lapsed'Access,
     Long_Switch => "--lapsed",
     Help        => "lapsed stats");

  Define_Switch
    (Cmd_Line,
     Ba_Avg_Price'Access,
     Long_Switch => "--avg_price",
     Help        => "avg_price  stats");

  Define_Switch
    (Cmd_Line,
     Ia_Days'Access,
     Long_Switch => "--days=",
     Help        => "days of stats");

  Define_Switch
    (Cmd_Line,
     Ba_Print_Strategies'Access,
     Long_Switch => "--print_strategies",
     Help        => "print strategies");

  Define_Switch
    (Cmd_Line,
     Ba_Histogram'Access,
     Long_Switch => "--histogram",
     Help        => "histogram data");

  Define_Switch
    (Cmd_Line,
     Ba_Noloss'Access,
     Long_Switch => "--noloss",
     Help        => "skip rest of day if loss");

  Define_Switch
    (Cmd_Line,
     Ia_Maxprice'Access,
     Long_Switch => "--max_price=",
     Help        => "for laybets - default 1000");

  Define_Switch
    (Cmd_Line,
     Sa_Price'Access,
     Long_Switch => "--price=",
     Help        => "price of bet");

  Define_Switch
    (Cmd_Line,
     Sa_Min_Price_Matched'Access,
     Long_Switch => "--min_price_matched=",
     Help        => "min price matched of bet");

  Define_Switch
    (Cmd_Line,
     SA_Side'Access,
     Long_Switch => "--side=",
     Help        => "BACK or LAY");

  Getopt (Cmd_Line);  -- process the command line


  if Ba_Print_Strategies then
    Config.Print_Strategies;
    return;
  end if;

  Ini.Load (Ev.Value ("BOT_HOME") & "/login.ini");

  Debug ("Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database_home", "host", ""),
     Port     => Ini.Get_Value("database_home", "port", 5432),
     Db_Name  => Ini.Get_Value("database_home", "name", ""),
     Login    => Ini.Get_Value("database_home", "username", ""),
     Password => Ini.Get_Value("database_home", "password", ""),
     SSL_Mode => "require");
  Debug ("db Connected");

  T.Start;
  if Ba_Lapsed then
    Day_Statistics_Lapsed_Vs_Settled (Betname => Sa_Betname.all,
                                      Days    => Integer_4 (Ia_Days),
                                      A_List  => Days_Result_List);
  elsif Ba_Profit then
    Day_Statistics_Profit_Vs_Matched (Betname => Sa_Betname.all,
                                      Days    => Integer_4 (Ia_Days),
                                      A_List  => Profit_Result_List);
  elsif Ba_Avg_Price then
    Avg_Price_For_Settled_Bets (Betname => Sa_Betname.all,
                                Days    => Integer_4 (Ia_Days),
                                A_List  => Avg_Price_Result_List);
  elsif Ba_Equity then
    Equity_Data (Betname => Sa_Betname.all,
                 A_List  => Equity_Result_List);

  elsif Ba_Equity2 then
    Equity_Data2 (Betname1 => Sa_Betname1.all,
                  Betname2 => Sa_Betname2.all,
                  A_List  => Equity_Result_List);

  elsif Ba_Histogram then
    Histogram_Data (Betname => Sa_Betname.all,
                    A_List  => Histogram_Result_List);
  end if;
  T.Commit;
  Sql.Close_Session;

  for R of Days_Result_List loop
    Print (
           R.Ts.String_Date_ISO & " | " &
             R.Lapsed'Img   & " | " &
             R.Settled'Img   & " | " &
             F8_Image (Fixed_Type (R.Settled * 100 / ( R.Settled + R.Lapsed )))
          ) ;
  end loop;

  for R of Profit_Result_List loop
    Print (
           R.Ts.String_Date_ISO & " | " &
             F8_Image (R.Profit) & " | " &
             F8_Image (R.Size_Matched) & " | " &
             F8_Image (One_Hundred * Fixed_Type (R.Profit / R.Size_Matched))
          ) ;
  end loop;

  for R of Avg_Price_Result_List loop
    Print (
           R.Ts.String_Date_ISO & " | " &
             F8_Image (R.Avg_Price )
          ) ;
  end loop;

  for R of Equity_Result_List loop
    Print (
           R.Ts.To_String (Milliseconds => False) & " | " &
             F8_Image (R.Equity )
          ) ;
  end loop;

  for R of Histogram_Result_List loop
    Print (
           R.Name.Fix_String & " | " &
             F8_Image (R.Profit )
          ) ;
  end loop;

end Graph_Data_Ael;
