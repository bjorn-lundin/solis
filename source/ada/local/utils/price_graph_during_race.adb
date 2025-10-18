--with Gnat.Command_Line; use Gnat.Command_Line;
--with Gnat.Strings;

with Ada.Exceptions;
with Stacktrace;
with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;

with Sql;
with Calendar2; use Calendar2;
--with Logging;               use Logging;
with Text_Io;
with Ini;
with  Ada.Environment_Variables;
--with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
--with Bot_Types;
with Utils; use Utils;
with Price_Histories;
with Markets;
with Runners;
with Types; use Types;

procedure Price_Graph_During_Race is
  package Ev renames Ada.Environment_Variables;
  -- Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type;

  --Sa_Marketid            : aliased Gnat.Strings.String_Access;
  --  Ba_Print_Bet           : aliased Boolean := False;
  Ml                     : Markets.Lists.List;
  Select_Market          : Sql.Statement_Type;
  Select_Markets         : Sql.Statement_Type;
  Find_Plc_Market        : Sql.Statement_Type;
  Find_Placement         : Sql.Statement_Type;
  No_Place_Market_Exists : exception;

  Cnt,Tot                : Integer_4 := 0;
  Gdebug                 : Boolean := True;

  type Placement_Type is (Winner, Place, Loser);

  -------------------------------
  procedure Debug (What : String) is
  begin
    if Gdebug then
      Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_Iso (Clock, " " , "") & " " & What);
    end if;
  end Debug;
  pragma Warnings(Off, Debug);
  -------------------------------


  function Is_Ok(M : Markets.Market_Type) return Boolean is
    pragma Unreferenced (M);
  begin
    return True;
  end Is_Ok;

  function Get_Runners(M : Markets.Market_Type; Placement : Placement_Type) return Runners.Lists.List is
    Mp                              : Markets.Market_Type;
    Eos                             : Boolean := True;
    Runner, Tmp_R                   : Runners.Runner_Type;
    Winners, Placers, Losers, Tmp_L : Runners.Lists.List;
  begin
--    Debug ("Get_Runners - start " & Placement'Img);
    T.Start;

    Find_Plc_Market.Prepare ("select MP.* from AMARKETS MW, AMARKETS MP " &
                               "where MW.EVENTID = MP.EVENTID " &
                               "and MW.STARTTS = MP.STARTTS " &
                               "and MW.MARKETID = :WINMARKETID " &
                               "and MP.MARKETTYPE = 'PLACE' " &
                               "and MP.NUMWINNERS = :NUM " &
                               "and MW.MARKETTYPE = 'WIN' ");
    Find_Plc_Market.Set ("NUM", Integer_4(3));
    Find_Plc_Market.Set ("WINMARKETID", M.Marketid);
    Find_Plc_Market.Open_Cursor;
    Find_Plc_Market.Fetch (Eos);
    if not Eos then
      Mp := Markets.Get(Find_Plc_Market);
      if M.Startts /= Mp.Startts then
        Eos := True;
      end if;
    end if;
    Find_Plc_Market.Close_Cursor;
    if Eos then
      T.Commit;
      raise No_Place_Market_Exists;
    end if;
--    Debug ("Get_Runners - Place market is found");


    Find_Placement.Prepare("select * from ARUNNERS where MARKETID=:MARKETID and STATUS =:STATUS");


    case Placement is
      when Winner =>
  --      Debug ("Get_Runners - market " & M.To_String);
        Find_Placement.Set("MARKETID",  M.Marketid);
        Find_Placement.Set("STATUS", "WINNER");
  --      Debug ("Get_Runners - open cursor");

        Find_Placement.Open_Cursor;
        loop
          Find_Placement.Fetch (Eos);
          exit when Eos;
          --Debug ("Get_Runners - in loop");
          Runner := Runners.Get(Find_Placement);
          Winners.Append(Runner);
        end loop;
        Find_Placement.Close_Cursor;
   --     Debug ("Get_Runners - clse cursor");


      when Place =>
        -- find the winner
        Find_Placement.Set("MARKETID", M.Marketid);
        Find_Placement.Set("STATUS", "WINNER");
        Find_Placement.Open_Cursor;
        loop
          Find_Placement.Fetch (Eos);
          exit when Eos;
          Runner := Runners.Get(Find_Placement);
          Winners.Append(Runner);
        end loop;
        Find_Placement.Close_Cursor;
        -- find the winner + #2 & #3
        Find_Placement.Set("MARKETID", Mp.Marketid);
        Find_Placement.Set("STATUS", "WINNER");
        Find_Placement.Open_Cursor;
        loop
          Find_Placement.Fetch (Eos);
          exit when Eos;
          Runner := Runners.Get(Find_Placement);
          Tmp_L.Append(Runner);
        end loop;
        Find_Placement.Close_Cursor;
        -- remove the winner
        for R of Winners loop
          Tmp_R := R;
        end loop;
        -- just add the non-winners
        for R of Tmp_L loop
          if R.Selectionid /= Tmp_R.Selectionid then
            Placers.Append(R);
          end if;
        end loop;

      when Loser =>
        Find_Placement.Set("MARKETID", Mp.Marketid);
        Find_Placement.Set("STATUS", "LOSER");
        Find_Placement.Open_Cursor;
        loop
          Find_Placement.Fetch (Eos);
          exit when Eos;
          Runner := Runners.Get(Find_Placement);
          Losers.Append(Runner);
        end loop;
        Find_Placement.Close_Cursor;
    end case;


   -- Debug ("Get_Runners -  stop " & Placement'Img);
    T.Commit;

    case Placement is
      when Winner => return Winners;
      when Place => return Placers;
      when Loser => return Losers;
    end case;

  end Get_Runners;
  ----------------------------------------------

  procedure Print_To_File(M : Markets.Market_Type; Placement : Placement_Type) is
    Rl       : Runners.Lists.List;
    Filename : String_Object;
    F        : Text_Io.File_Type;
    Phl      : Price_Histories.Lists.List;
  begin
 --   Debug ("Print_To_File - start " & Placement'Img);

    T.Start;

    Rl := Get_Runners(M,Placement);
    Select_Market.Prepare("select * from APRICESHISTORY " &
                            "where MARKETID =:MARKETID " &
                            "and SELECTIONID = :SELECTIONID " &
                            "order by PRICETS");
    Select_Market.Set("MARKETID",M.Marketid);

    for R of Rl loop
      Select_Market.Set("SELECTIONID",R.Selectionid);
      Phl.Clear;
      Price_Histories.Read_List(Select_Market,Phl);
      if Phl.Length > 500 then
        Filename.Set("/home/bnl/bnlbot/botstart/bot-1-0/script/plot/price_plots/dats/" &
                       M.Marketid & "_" & Trim(R.Selectionid'Img) & "_" & Placement'Img & ".dat");

        Text_Io.Create(File => F,
                       Mode => Text_Io.Out_File,
                       Name => Filename.Lower_Case);
        for Ph of Phl loop
          Text_Io.Put_Line(F,Ph.Pricets.String_Date_Time_Iso(T => " ", Tz => "") & " |" &
                             Ph.Selectionid'Img & " | " &
                             F8_Image(Ph.Backprice) & " | " &
                             F8_Image(Ph.Layprice));
        end loop;
        Text_Io.Close(F);
      end if;
    end loop;
    T.Commit;

  --  Debug ("Print_To_File -  stop " & Placement'Img);

  end Print_To_File;
  -----------------------------------------

begin
  --      Define_Switch
  --        (Cmd_Line,
  --         Sa_Marketid'Access,
  --         Long_Switch => "--marketid=",
  --         Help        => "which market?")--;

  --      Define_Switch
  --        (Cmd_Line,
  --         Ba_Print_Bet'Access,
  --         Long_Switch => "--print_bet",
  --         Help        => "include when bets are lay in graph");
  --
  --      Getopt (Cmd_Line);  -- process the command line




  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");

  Debug("Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("database_home","host",""),
     Port     => Ini.Get_Value("database_home","port", 5432),
     Db_Name  => Ini.Get_Value("database_home","name",""),
     Login    => Ini.Get_Value("database_home","username",""),
     Password => Ini.Get_Value("database_home","password",""));
  Debug("db Connected");


  T.Start;
  Select_Markets.Prepare("select * from AMARKETS M, AEVENTS E " &
                           "where M.EVENTID = E.EVENTID " &
                           "and E.EVENTTYPEID = 7 " &
                           "and M.NUMRUNNERS >= 7 " &
                           "and M.MARKETTYPE = 'WIN' " &
                           "order by M.STARTTS");

  Markets.Read_List(Select_Markets,Ml);
  T.Commit;

  Tot := Integer_4(Ml.Length);
  Debug ("# Markets =" & Tot'Img);

  for M of Ml loop
    Cnt := Cnt +1;
    if Cnt mod 100 = 0 then
      Debug (Cnt'Img & "/" & Tot'Img & " -> " &
               F8_Image(Fixed_Type(100.0 * Float(Cnt) / Float(Tot))) & " %");
    end if;


    if Is_Ok(M) then
      begin
        Print_To_File(M,Winner);
        Print_To_File(M,Place);
        Print_To_File(M,Loser);
      exception
        when No_Place_Market_Exists => null;
        --  Debug("No placemarket for winmarket " & M.To_String);
      end;
    end if;
  end loop;

  Sql.Close_Session;

exception
  when E : others =>
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name (E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message (E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information (E);
    begin
      Debug (Last_Exception_Name);
      Debug ("Message : " & Last_Exception_Messsage);
      Debug (Last_Exception_Info);
      Debug ("addr2line" & " --functions --basenames --exe=" &
             Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump (Last_Exception_Info));
    end ;

end Price_Graph_During_Race;
