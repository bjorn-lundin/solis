
with Ada;
with Ada.Strings;
with Ada.Strings.Hash;

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Command_Line;
with Ada.Exceptions;

with Types ; use Types;
with Bot_Types ; use Bot_Types;
with Stacktrace;
with Sql;
with Calendar2;
with Logging; use Logging;
with Utils;
with Text_Io; use Text_Io;

procedure Test_Sum_Sql is

   T                   : Sql.Transaction_Type;
   Select_Profit_Days  : Sql.Statement_Type;
   Select_Profit_Weeks : Sql.Statement_Type;

   Eos : Boolean := True;
   --------------------------------------------------------------------------

   type Days_Summation_Type is record
     Betname   : String (Bet_Name_Type'range) := (others => ' ');
     Sumprofit : Fixed_Type := 0.0;
     Count     : Integer_4 := 0;
   end record;

  package Days_Summation_Pack is new Ada.Containers.Hashed_Maps
        (Bet_Name_Type,
         Days_Summation_Type,
         Ada.Strings.Hash,
         "=",
         "=");

  Data : Days_Summation_Type;

  Days  : array (0 .. 6 ) of Days_Summation_Pack.Map;
  Weeks : array (0 .. 6 ) of Days_Summation_Pack.Map;

  package Bet_List_Pack is new Ada.Containers.Doubly_Linked_Lists(Bet_Name_Type);

  Bet_List : Bet_List_Pack.List;

  package Bet_List_Sorter is new Bet_List_Pack.Generic_Sorting("<");

  type Monday_And_Sunday_Type is record
    M,S :  Calendar2.Time_Type ;
  end record;

  function Monday_And_Sunday(Weeks_Back : Natural) return Monday_And_Sunday_Type is
    use Calendar2;
    Now : Time_Type := Clock;
    Tmp: Monday_And_Sunday_Type;
  begin
    Now := Now - (Integer_4(Weeks_Back * 7), 0, 0, 0, 0);

    loop
      exit when Week_Day_Of(Now) = Sunday;
      Now := Now - (1,0,0,0,0);
    end loop;
    Tmp.M := Now;
    Tmp.S := Now + (7,0,0,0,0);
    return Tmp;
  end Monday_And_Sunday;

  Week_Boundaries : Monday_And_Sunday_Type ;


begin
  Sql.Connect
    (Host     => "db.nonodev.com",
     Port     => 5432,
     Db_Name  => "bnl",
     Login    => "bnl",
     Password => "BettingFotboll1$");

  T.Start;
      Select_Profit_Days.Prepare(
        "select " &
          "BETNAME, " &
          "sum(PROFIT) as SUMPROFIT, " &
          "count('a') as CNT " &
        "from " &
          "ABETS " &
        "where BETPLACED::date = (select CURRENT_DATE) - interval ':DAYS days' " &
          "and BETWON is not null " &
          "and EXESTATUS = 'SUCCESS' " &
          "and STATUS in ('SETTLED') " &
        "group by " &
          "BETNAME " &
        "order by " &
          "sum(PROFIT) desc, " &
          "BETNAME");

      Select_Profit_Weeks.Prepare(
        "select " &
          "BETNAME, " &
          "sum(PROFIT) as SUMPROFIT, " &
          "count('a') as CNT " &
        "from " &
          "ABETS " &
        "where BETPLACED >= :MONDAY " &
          "and BETPLACED <= :SUNDAY " &
          "and BETWON is not null " &
          "and EXESTATUS = 'SUCCESS' " &
          "and STATUS in ('SETTLED') " &
        "group by " &
          "BETNAME " &
        "order by " &
          "sum(PROFIT) desc, " &
          "BETNAME");

    for i in 0 .. 6 loop
      Select_Profit_Days.Set("DAYS", (Integer_4(i)));
      Select_Profit_Days.Open_Cursor;
      loop
        Select_Profit_Days.Fetch(Eos);
        exit when Eos;
        Data.Betname := (others => ' ');
        Select_Profit_Days.Get("BETNAME", Data.Betname);
        Select_Profit_Days.Get("SUMPROFIT", Data.Sumprofit);
        Select_Profit_Days.Get("CNT", Data.Count);

        Days(i).Insert(Data.Betname, Data);
      end loop;
      Select_Profit_Days.Close_Cursor;
    end loop;

    for i in 0 .. 6 loop
      Week_Boundaries := Monday_And_Sunday(i);
      Select_Profit_Weeks.Set("MONDAY", Week_Boundaries.M);
      Select_Profit_Weeks.Set("SUNDAY", Week_Boundaries.S);
      Select_Profit_Weeks.Open_Cursor;
      loop
        Select_Profit_Weeks.Fetch(Eos);
        exit when Eos;
        Data.Betname := (others => ' ');
        Select_Profit_Weeks.Get("BETNAME", Data.Betname);
        Select_Profit_Weeks.Get("SUMPROFIT", Data.Sumprofit);
        Select_Profit_Weeks.Get("CNT", Data.Count);

        Weeks(i).Insert(Data.Betname, Data);
      end loop;
      Select_Profit_Weeks.Close_Cursor;
    end loop;
  T.Commit;
  Sql.Close_Session;

  for i in 0 .. 6 loop
    for Bet of Days(i) loop
      if not Bet_List.Contains(Bet.Betname) then
        Bet_List.Append(Bet.Betname);
      end if;
    end loop;
  end loop;

  Bet_List_Sorter.Sort(Bet_List);
  --uniqe betnames
  declare
   R : Days_Summation_Type;
   Col : Positive_Count := 1;
   Total_Day : array (0..7) of Fixed_Type := (others => 0.0);
   Total_Week : array (0..7) of Fixed_Type := (others => 0.0);

  begin
    for Name of Bet_List loop
      Total_Week := (others => 0.0);
      Col := 1;
      Set_Col(col);
      Put (Utils.Trim(Name));
      Col := 40;
      for i in 0 .. 6 loop
        if Days(i).Contains(Name) then
          R := Days(i)(Name);
        else
          R.Sumprofit := 0.0;
        end if;
        Total_Day(i)  := Total_Day(i)  + R.Sumprofit;
        Total_Week(i) := Total_Week(i) + R.Sumprofit;
        Set_Col(Col);
        Put ( Integer(R.Sumprofit)'Img);
        Col := Col +6;
        Total_Day(7)  := Total_Day(7)  + Total_Day(i);
        Total_Week(7) := Total_Week(7) + Total_Week(i);
      end loop;
      Set_Col(Col);
      Put (Integer(Total_Week(7))'Img);
      New_Line;
    end loop;

    Col := 1;
    Total_Day(7) := 0.0;
    Set_Col(Col);
    Put ("Daily total");
    Col := 40;
    for i in 0 .. 6 loop
      Set_Col(Col);
      Put(Integer(Total_Day(i))'Img);
      Col := Col +6;
      Total_Day(7) := Total_Day(7) + Total_Day(i);
    end loop;
    Set_Col(Col);
    Put(Integer(Total_Day(7))'Img);
    New_Line;
  end;

  New_Line;
  Bet_List.Clear;
  for i in 0 .. 6 loop
    for Bet of Weeks(i) loop
      if not Bet_List.Contains(Bet.Betname) then
        Bet_List.Append(Bet.Betname);
      end if;
    end loop;
  end loop;

  Bet_List_Sorter.Sort(Bet_List);
  --unique betnames
  declare
   R : Days_Summation_Type;
   Col : Positive_Count := 1;
   Total_Day  : array (0..7) of Fixed_Type := (others => 0.0);
   Total_Week : array (0..7) of Fixed_Type := (others => 0.0);

  begin
    for Name of Bet_List loop
      Total_Week := (others => 0.0);
      Col := 1;
      Set_Col(col);
      Put (Utils.Trim(Name));
      Col := 40;
      for i in 0 .. 6 loop
        if Weeks(i).Contains(Name) then
          R := Weeks(i)(Name);
        else
          R.Sumprofit := 0.0;
        end if;
        Total_Day(i)  := Total_Day(i)  + R.Sumprofit;
        Total_Week(i) := Total_Week(i) + R.Sumprofit;
        Set_Col(Col);
        Put ( Integer(R.Sumprofit)'Img);
        Col := Col +6;
        Total_Day(7)  := Total_Day(7)  + Total_Day(i);
        Total_Week(7) := Total_Week(7) + Total_Week(i);
      end loop;
      Set_Col(Col);
      Put (Integer(Total_Week(7))'Img);
      New_Line;
    end loop;
    Col := 1;
    Total_Day(7) := 0.0;
    Set_Col(Col);
    Put ("Weekly total");
    Col := 40;
    for i in 0 .. 6 loop
      Set_Col(Col);
      Put(Integer(Total_Day(i))'Img);
      Col := Col +6;
      Total_Day(7) := Total_Day(7) + Total_Day(i);
    end loop;
    Set_Col(Col);
    Put(Integer(Total_Day(7))'Img);
    New_Line;
  end;














exception
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
end Test_Sum_Sql;

