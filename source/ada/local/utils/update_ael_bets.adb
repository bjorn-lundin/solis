
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
--with Ada.Containers.Doubly_Linked_Lists;

with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

with Stacktrace;
with Types; use Types;
--with Bot_Types; use Bot_Types;
with Sql;
with Calendar2; use Calendar2;
--with Bot_Messages;
--with Rpc;
--with Lock ;
with Posix;
with Ini;
with Logging; use Logging;
--with Process_IO;
--with Core_Messages;
with Markets;
--with Table_Aevents;
--with Table_Aprices;
--with Table_Abalances;
with Table_Arunners;
with Table_Abets;
with Table_Apriceshistory;
with Bot_Svn_Info;
with Utils; use Utils;
--with Bot_System_Number;


procedure Update_Ael_Bets is
  package EV renames Ada.Environment_Variables;

  T                 : Sql.Transaction_Type;
  Select_Pm         : Sql.Statement_Type;
  Select_Bets_All   : Sql.Statement_Type;
  Select_Bets_Month : Sql.Statement_Type;
  Rows_Affected     : Natural := 0;
  Me                : constant String := "Update_Ael_Bets.";
  Comission         : constant Fixed_Type := 6.5 / 100.0;
  Cmd_Line          : Command_Line_Configuration;
  SA_Month          : aliased Gnat.Strings.String_Access;

  -------------------------------------------------------------

  procedure Check_Bet_Won ( R : in     Table_Arunners.Data_Type;
                            B : in out Table_Abets.Data_Type) is
  begin
    if R.Status(1..7) = "REMOVED" then
      return;
    end if;
    B.Runnername := R.Runnernamestripped;    
    if B.Side(1..4) = "BACK" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := True;
        elsif R.Status(1..5) = "LOSER" then
          B.Betwon := False;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
    elsif B.Side(1..3) = "LAY" then
        if R.Status(1..6) = "WINNER" then
          B.Betwon := False;
        elsif R.Status(1..5) = "LOSER" then
          B.Betwon := True;
        elsif R.Status(1..7) = "REMOVED" then
          B.Status(1) := 'R';
          B.Betwon := True;
        end if;
    end if;
  end Check_Bet_Won;
  ----------------------------------------------------------------
  procedure Check_Bet_Profit (B : in out Table_Abets.Data_Type) is
  begin
    if B.Pricematched < 0.5 then
      B.Profit := 0.0;
      return;
    end if;

    if B.Side(1..4) = "BACK" then
      if B.Betwon then
        B.Profit := (1.0 - Comission) * B.Sizematched * (B.Pricematched - 1.0);
      else
        B.Profit := -B.Sizematched;
      end if;
    elsif B.Side(1..3) = "LAY" then
      if B.Betwon then
        B.Profit := (1.0 - Comission) * B.Sizematched;
      else
        B.Profit := -B.Sizematched * (B.Pricematched - 1.0);
      end if;
    end if;
  end Check_Bet_Profit;
  ----------------------------------------------------------------

begin
  if not EV.Exists("BOT_NAME") then
    EV.Set("BOT_NAME","update_ael_bets");
  end if;

  --Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");
  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
        (Host     => Ini.Get_Value("stats", "host", ""),
         Port     => Ini.Get_Value("stats", "port", 5432),
         Db_Name  => Ini.Get_Value("stats", "name", ""),
         Login    => Ini.Get_Value("stats", "username", ""),
         Password => Ini.Get_Value("stats", "password", ""));
  Log(Me, "db Connected");


  Define_Switch
     (Cmd_Line,
      SA_Month'access,
      Long_Switch => "--month=",
      Help        => "Numeric - 1 .. 12");

  Getopt (Cmd_Line);  -- process the command line



  Select_Bets_All.Prepare(
      "select * " &
      "from ABETS " &
      "where ((STATUS = '') or (STATUS like '-%')) " &
      "order by BETPLACED"
    );
  Select_Bets_Month.Prepare(
      "select * " &
      "from ABETS " &
      "where ((STATUS = '') or (STATUS like '-%')) " &
      "and extract(month from BETPLACED) = :MONTH " &
      "order by BETPLACED"
    );
  Select_Pm.Prepare(
      "select * " &
      "from APRICESHISTORY  " &
      "where MARKETID = :MARKETID " &
      "and SELECTIONID = :SELECTIONID " &
      "and PRICETS between :BETPLACED1 and :BETPLACED2 " &
      "order by PRICETS "
    );
    T.Start;
    declare
      Ph_Data  : Table_Apriceshistory.Data_Type;
      type Eos_Type is (AHistory, ARunner);
      Eos      : array (Eos_Type'range) of Boolean := (others => True);
      Bet_List : Table_Abets.Abets_List_Pack2.List;
      Runner : Table_Arunners.Data_Type;
      Cnt    : Natural := 0;
      Was_Matched : Boolean := False;
    begin

      if SA_Month.all /= "" then
        Log(Me & "Main" , "start read Month '" & SA_Month.all & "'");
        Select_Bets_Month.Set("MONTH", Integer_4'Value(SA_Month.all));
        Table_Abets.Read_List(Select_Bets_Month, Bet_List);
      else
        Log(Me & "Main" , "start read all");
        Table_Abets.Read_List(Select_Bets_All, Bet_List);
      end if;
      Log(Me & "Main" , "read_all done, #bets:" & Bet_List.Length'Img);
      Rows_Affected := 0;

      for Bet of Bet_List loop
      --if not already treated no status
        Cnt := Cnt +1;
        if Cnt rem 10_000 = 0 then
          T.Commit;
          Log(Me & "Select_Pm" , "treat: " & Bet.To_String);
          Log(Me & "Select_Pm" , "treated: " & F8_Image(100.0 * Fixed_Type(Cnt)/Fixed_Type(Bet_List.Length)) & " %");
          T.Start;
        end if;
        -- check lost/won
        Runner.Marketid := Bet.Marketid;
        Runner.Selectionid := Bet.Selectionid;
        Runner.Read(Eos(Arunner));
        Check_Bet_Won(R => Runner, B=> Bet);

        -- calculate likely odds
        Select_Pm.Set("MARKETID",Bet.Marketid);
        Select_Pm.Set("SELECTIONID",Bet.Selectionid);
        Select_Pm.Set("BETPLACED1",Bet.Betplaced + (0,0,0,1,0));    --1.0 s
        Select_Pm.Set("BETPLACED2",Bet.Betplaced + (0,9,0,1,500));  --9h1.5 s
        -- look 1 after bet is placed until finish
        Was_Matched := False;
        Select_Pm.Open_Cursor;
        loop
          Select_Pm.Fetch(Eos(Ahistory));
          exit when Eos(Ahistory);
          Ph_Data := Table_Apriceshistory.Get(Select_Pm);
          if Bet.Side(1..4) = "BACK" then
            if Ph_Data.Backprice >= Bet.Price and then
               Ph_Data.Backprice <= Fixed_Type(1000.0) then
              Bet.Pricematched := Ph_Data.Backprice;
              Rows_Affected := Rows_Affected +1;
              Was_Matched := True;
              exit;
            end if;
          elsif Bet.Side(1..3) = "LAY" then
            if Ph_Data.Layprice <= Bet.Price and then
               Ph_Data.Layprice >= Fixed_Type(1.01) then
              Bet.Pricematched := Ph_Data.Layprice;
              Rows_Affected := Rows_Affected +1;
              Was_Matched := True;
              exit;
            end if;
          end if;
        end loop;
        Select_Pm.Close_Cursor;

        if Was_Matched then
          Move("SUCCESS",Bet.Status);
        else
          Move("LAPSED",Bet.Status);
          Bet.Pricematched := 0.0;
          Bet.Sizematched := 0.0;
        end if;

        -- calculate profit
        Check_Bet_Profit(B=> Bet);
        declare
          Market : Markets.Market_Type;
          Eos    : Boolean := False;
        begin 
          if Bet.Startts = Time_Type_First then
            Market.Marketid := Bet.Marketid;
            Market.Read(Eos);
            if not Eos then
              Bet.Startts := Market.Startts;
              Bet.Fullmarketname := Market.Marketname;
            end if;
          end if;
          Bet.Update_Withcheck;
        exception
          when  Sql.No_Such_Row =>
            Log(Me & "No_Such_Row", Bet.To_String);
            T.Rollback;
            raise;
        end;    
      end loop;
      T.Commit;
      Log(Me & "Select_Pm" , "Done - Rows_Affected:" & Rows_Affected'Img);
    end ;


exception
--  when Lock.Lock_Error =>
--    Log(Me, "lock error, exit");
--    Logging.Close;
--    Posix.Do_Exit(0); -- terminate
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
    Posix.Do_Exit(0); -- terminate

end Update_Ael_Bets;
