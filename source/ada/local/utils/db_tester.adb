with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Types; use Types;
with Bot_Config;
--with Lock; 
with Text_io;
with Sql;
with Logging; use Logging;
with Table_Abets;
--with Table_Aprices;
with Ada.Exceptions;
--with Bet_Handler;

with Ada.Directories;

procedure Db_Tester is
  Me : constant String := "Db_Tester.";  

  T               : Sql.Transaction_Type;
  Select_Exists   : Sql.Statement_Type;
  Eos             : Boolean := False;
--  Aprices         : Table_Aprices.Data_Type;
  Abet            : Table_Abets.Data_Type;
--  Sel_All : Sql.Statement_Type;
begin
  begin 
    declare
      package AD renames Ada.Directories;
       use AD;
       FullFilename : string := "/usr2/betbot/source/ada/bot_ws/html/";
     begin
       text_io.put_line(FullFilename);
       text_io.put_line("exist " & AD.Exists(FullFilename)'image);
       text_io.put_line("size " & Boolean'image(AD.Size(FullFilename) > 0));
       text_io.put_line("kind " & Boolean'image(AD.Kind(FullFilename) = AD.Ordinary_File));
    end ; 
exception
    when E : others =>
      Text_Io.Put_Line(Ada.Exceptions.Exception_Information (E));
end;
return;

--  Bot_Config.Config.Read; -- even from cmdline
--
--  Log(Me, "Connect Db");
--  Sql.Connect
--        (Host     => To_String(Bot_Config.Config.Database_Section.Host),
--         Port     => 5432,
--         Db_Name  => To_String(Bot_Config.Config.Database_Section.Name),
--         Login    => To_String(Bot_Config.Config.Database_Section.Username),
--         Password => To_String(Bot_Config.Config.Database_Section.Password));
--  Log(Me, "db Connected");
--
--
--  T.Start;
--  
--      Select_Exists.Prepare(
--         "select * " & 
--         "from " &
--           "ABETS  limit 1"); 
--         -- &
--         -- "where MARKETID = :MARKETID " & 
--         -- " and BETNAME = :BETNAME");
--           
----      Select_Exists.Set("BETNAME", "DR_HOUNDS_WINNER_BACK_BET_45_07");
-- --     Select_Exists.Set("MARKETID", "1.110172643");
-- 
--      Select_Exists.Open_Cursor;     
--      Select_Exists.Fetch( Eos);     
--      Select_Exists.Close_Cursor;     
--      Log(Me & "Exists", "Eos: " & Eos'Img);
--      if not Eos then
--        Abet := Table_Abets.Get(Select_Exists);
--        Log(Me & "Exists", "Bet does exist " & Table_Abets.To_String(Abet));
--      else  
--        Log(Me & "Exists", "Bet does not exist");
--      end if;
--      Bet_Handler.Test_Bet;
--      F8.Put(Item => Abet.Size); Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>2, Aft =>6, Exp => 2);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>2, Aft =>4, Exp => 2);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>4, Aft =>6, Exp => 2);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>4, Aft =>4, Exp => 2);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>2, Aft =>6, Exp => 4);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>2, Aft =>4, Exp => 4);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>4, Aft =>6, Exp => 4);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>4, Aft =>4, Exp => 4);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>2, Aft =>6, Exp => 0);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>2, Aft =>4, Exp => 0);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>4, Aft =>6, Exp => 0);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>4, Aft =>4, Exp => 0);Text_io.New_Line;
--      F8.Put(Item => Abet.Size, Fore =>0, Aft =>2, Exp => 0);Text_io.New_Line;
--      F8.Put(Item => 3.4456, Fore =>0, Aft =>2, Exp => 0);Text_io.New_Line;
--      F8.Put(Item => 300.54333, Fore =>0, Aft =>2, Exp => 0);Text_io.New_Line;
--      F8.Put(Item => -Abet.Size, Fore =>0, Aft =>2, Exp => 0);Text_io.New_Line;
--      F8.Put(Item => -3.4456, Fore =>0, Aft =>2, Exp => 0);Text_io.New_Line;
--      F8.Put(Item => -300.54333, Fore =>0, Aft =>2, Exp => 0);Text_io.New_Line;
--  T.Commit;
--  
--  Log(Me, "Close Db");
--  Sql.Close_Session;
--  Log(Me, "Db Closed");
--  Logging.Close;
end Db_Tester;