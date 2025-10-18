
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
--with Ada.Containers.Doubly_Linked_Lists;

--with Gnat.Command_Line; use Gnat.Command_Line;
--with Gnat.Strings;

with Stacktrace;
with Types; use Types;
with Bot_Types; use Bot_Types;
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
with Table_Amarkets;
--with Table_Aevents;
--with Table_Aprices;
--with Table_Abalances;
with Table_Arunners;
with Table_Abets;
with Table_Apriceshistory;
with Bot_Svn_Info;
--with Utils; use Utils;
with Bot_System_Number;


procedure Check_for_Greenup_Win2 is
  package EV renames Ada.Environment_Variables;
  T : Sql.Transaction_Type;
  Select_Cand         : Sql.Statement_Type;
  Select_Ph           : Sql.Statement_Type;
  Select_Markets      : Sql.Statement_Type;
  Select_Selectionids : Sql.Statement_Type;
  Me            : constant String := "Check_for_Greenup_Win2.";

  procedure Add_Bet(M : Markets.Market_Type;
                    R : Table_Arunners.Data_Type;
                    P : Table_Apriceshistory.Data_Type) is
    Bet : Table_Abets.Data_Type;
    Now : Calendar2.Time_Type := Calendar2.Clock;
    Bet_Name : Bet_Name_Type := (others => ' ');
  begin
    Move("LAY_1_90_2_00_PLC",Bet_Name);
    Bet := (
      Betid          => Integer_8(Bot_System_Number.New_Number(Bot_System_Number.Betid)),
      Marketid       => M.Marketid,
      Betmode        => Bot_Mode(Simulation),
      Powerdays      => 0,
      Selectionid    => R.Selectionid,
      Reference      => (others => '-'),
      Size           => Fixed_Type(30.0),
      Price          => Fixed_Type(1.9),
      Side           => "LAY ",
      Betname        => Bet_Name,
      Betwon         => False,
      Profit         => 0.0,
      Status         => (others => ' '),
      Exestatus      => (others => ' '),
      Exeerrcode     => (others => ' '),
      Inststatus     => (others => ' '),
      Insterrcode    => (others => ' '),
      Startts        => M.Startts,
      Betplaced      => P.Pricets,
      Pricematched   => P.Layprice,
      Sizematched    => Fixed_Type(30.0),
      Runnername     => R.Runnername,
      Fullmarketname => (others => ' '),
      Svnrevision    => Bot_Svn_Info.Revision,
      Ixxlupd        => (others => ' '), --set by insert
      Ixxluts        => Now              --set by insert
    );
    Bet.Insert;
  end Add_Bet;
  --------------------------------------------------

begin
  if not EV.Exists("BOT_NAME") then
    EV.Set("BOT_NAME","check_for_green");
  end if;
 -- Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");
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

  Select_Markets.Prepare(
      "select * " &
      "from " &
      "AMARKETS " &
      "where MARKETTYPE = 'WIN' " &
      "order by STARTTS"
  );

  Select_Selectionids.Prepare(
      "select * " &
      "from " &
      "ARUNNERS " &
      "where MARKETID = :MARKETID " &
      "and STATUS in ('WINNER','LOSER') " --& 
     -- "order by SORTPRIO"
  );

  Select_Cand.Prepare(
      "select * " &
      "from APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "and SELECTIONID = :SELECTIONID " &
      "order by PRICETS"
  );

  Select_Ph.Prepare(
      "select * " &
      "from " &
      "APRICESHISTORY " &
      "where MARKETID = :MARKETID " &
      "and SELECTIONID = :SELECTIONID " &
      "and PRICETS >= :PRICETS " &
      "order by PRICETS"
  );

  T.Start;
    declare
      Ph_List     : Table_Apriceshistory.Apriceshistory_List_Pack2.List;
      Market_List : Table_Amarkets.Amarkets_List_Pack2.List;
      Runner_List : Table_Arunners.Arunners_List_Pack2.List;
      type Outcome_Type is (Win, Lose);
      type Ok_Type is (Backed, Laid);
      --Ts : Calendar2.Time_Type;

      Num : array (Ok_Type'range, Outcome_Type'range) of Natural := (others => (others => 0));
      Has : array (Ok_Type'range) of Boolean := (others => False);
      Cnt    : Natural := 0;
    begin
      Log(Me & "Main" , "read start");
      Table_Amarkets.Read_List(Select_Markets, Market_List);
      Log(Me & "Main" , "read done");

      Market_Loop : for Market of Market_List loop
        Cnt := Cnt +1;
        if Cnt rem 100 = 0 then
          Log(Me & "Main" , "treat: " & Market.To_String);
        end if;
        Select_Selectionids.Set("MARKETID",Market.Marketid);
        Runner_List.Clear;
        Table_Arunners.Read_List(Select_Selectionids, Runner_List);

        Runner_Loop : for Runner of Runner_List loop
          Log(Me & "try runner ", Runner.To_String);              
        
          Has := (others => False);
          Ph_List.Clear;
          Select_Cand.Set("MARKETID",Market.Marketid);
          Select_Cand.Set("SELECTIONID",Runner.Selectionid);
          Table_Apriceshistory.Read_List(Select_Cand, Ph_List);

          Ph_Loop : for Ph of Ph_List loop
            if not Has(Laid) and then
               Ph.Layprice >= Fixed_Type(1.01) and then
               Ph.Layprice >= Fixed_Type(1.01) and then
               Ph.Backprice <= Fixed_Type(1.90) then

              Has(Laid) := True;
              --Ts := Ph.Pricets;
            end if;

            if Has(Laid) then
              -- taken care of in update_ael_bets
              -- Ph.Pricets >= Ts + (0,0,0,1,0) and then
              -- Ph.Layprice >= Fixed_Type(1.01) and then
              -- Ph.Layprice <= Fixed_Type(1.9) then

              if Runner.Status(1..6) = "WINNER" then
                 Num(Laid,Win) := Num(Laid,Win) +1;
              elsif Runner.Status(1..5) = "LOSER" then
                 Num(Laid,Lose) := Num(Laid,Lose) +1;
              end if;
              Has(Backed) := True;
              Add_Bet(Market,Runner,Ph);
              Log(Me & "used runner ", Runner.To_String);              
            end if;

           -- exit Ph_Loop when Has(Backed);
            exit Runner_Loop when Has(Backed); -- high profit
          end loop Ph_Loop;
        end loop Runner_Loop;
      end loop Market_Loop;

      for i in Ok_Type'range loop
        for j in Outcome_Type'range loop
           Log(Me & "Select_Ph" , "Num(" & i'Img & "," & j'Img & "):" & Num(i,j)'Img);
        end loop;
      end loop;
    end ;
  T.Commit;


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

    Log(Me, "Closed log and die");
    Logging.Close;
    Posix.Do_Exit(0); -- terminate

end Check_for_Greenup_Win2;





