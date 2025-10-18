
--with Gnat.Command_Line; use Gnat.Command_Line;
--with Gnat.Strings;

with Ada.Exceptions;
with Stacktrace;
with Ada.Command_Line;

with Sql;
with Calendar2; use Calendar2;
--with Logging;               use Logging;
with Text_Io;
with Ini;
with  Ada.Environment_Variables;
--with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
--with Bot_Types;
with Utils; use Utils;
with Types; use Types;
with Bets;
with Prices;


procedure Update_Back_Plc_Bets  is
  package Ev renames Ada.Environment_Variables;
  -- Cmd_Line              : Command_Line_Configuration;
  T                     : Sql.Transaction_Type;

  --Sa_Marketid            : aliased Gnat.Strings.String_Access;
  --  Ba_Print_Bet           : aliased Boolean := False;
  Bet_List             : Bets.Lists.List;
  Select_Bets          : Sql.Statement_Type;
  Delete_Laybets       : Sql.Statement_Type;



  Cnt,Tot                : Integer_4 := 0;
  Gdebug                 : Boolean := True;


  -------------------------------
  procedure Debug (What : String) is
  begin
    if Gdebug then
      Text_Io.Put_Line (Text_Io.Standard_Error, Calendar2.String_Date_Time_Iso (Clock, " " , "") & " " & What);
    end if;
  end Debug;
  pragma Warnings(Off, Debug);
  -------------------------------

  Price : Prices.Price_Type;
  Eos : Boolean := False;
  D : Calendar2.Time_Type := (2017,01,01,00,00,00,000);
  Laybet : Bets.Bet_Type;


  procedure Delete_All_Laybets is

  begin
    T.Start;
    Delete_Laybets.Prepare("delete from ABETS where BETNAME like 'HORSE_LAY__PLC%'");
    begin
      Delete_Laybets.Execute;
    exception
      when Sql.No_Such_Row => null;
    end;
    T.Commit;
  end Delete_All_Laybets;


begin


  Ini.Load(Ev.Value("BOT_HOME") & "/login.ini");

  Debug("Connect Db");
  Sql.Connect
    (Host     => Ini.Get_Value("local","host",""),
     Port     => Ini.Get_Value("local","port", 5432),
     Db_Name  => Ini.Get_Value("local","name",""),
     Login    => Ini.Get_Value("local","username",""),
     Password => Ini.Get_Value("local","password",""));
  Debug("db Connected");


  Delete_All_Laybets;

  T.Start;

  Select_Bets.Prepare("select * from ABETS where STARTTS >= :DATE and BETNAME like 'HORSE_BACK_PLC%' order by STARTTS");
  Select_Bets.Set("DATE", D);
  Bets.Read_List(Select_Bets,Bet_List);
  T.Commit;
  Tot := Integer_4(Bet_List.Length);

  for B of Bet_List loop
    Cnt := Cnt +1;
    if Cnt mod 100 = 0 then
      Debug (Cnt'Img & "/" & Tot'Img & " -> " &
               F8_Image(Fixed_Type(100.0 * Float(Cnt) / Float(Tot))) & " %");
    end if;

    T.Start;

    Price.Marketid := B.Marketid;
    Price.Selectionid := B.Selectionid;
    Price.Read(Eos);

    if not Eos then
      B.Sizematched := 30.0;
      B.Pricematched := Price.Backprice;
      B.Check_Outcome;
      B.Update_Withcheck;

      Laybet := B;
      Laybet.Betname(7..10) := "LAY_";
      Laybet.Betid := Integer_8(Cnt);
      Laybet.Side := "LAY ";
      Laybet.Sizematched := 30.0;
      Laybet.Pricematched := Price.Layprice;
      Laybet.Insert;
      Laybet.Check_Outcome;
      Laybet.Update_Withcheck;
    else
      Debug("no price! " & B.To_String);
    end if;
    T.Commit;
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
    T.Rollback;
    Sql.Close_Session;

end Update_Back_Plc_Bets;
