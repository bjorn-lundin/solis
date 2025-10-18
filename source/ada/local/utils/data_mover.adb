with Ada.Exceptions;
with Ada.Command_Line;
with Stacktrace;
with Sql;
with Types; use Types;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Lock ;
with Ini;
with Logging; use Logging;
with Ada.Environment_Variables;
with Bot_Svn_Info;
with Posix;

procedure Data_Mover is
  package EV renames Ada.Environment_Variables;


  Me : constant String := "Data_Mover.";
  type Tables_Type is (Amarkets, Aevents, Arunners, Aprices);

  My_Lock          : Lock.Lock_Type;
  Do_Move          : array (Tables_Type'range) of Sql.Statement_Type;
  Do_Delete        : array (Tables_Type'range) of Sql.Statement_Type;
  Do_Delete_Old    : array (Tables_Type'range) of Sql.Statement_Type;
  
  Sa_Par_Bot_User  : aliased Gnat.Strings.String_Access;
  Sa_Par_Inifile   : aliased Gnat.Strings.String_Access;
  Ba_Daemon        : aliased Boolean := False;
  Cmd_Line         : Command_Line_Configuration;

  -------------------------------------------------------------

  -------------------------------------------------------------
  procedure Run is
    T : Sql.Transaction_Type;
    Num : Integer_4 := 30;
    Rows_Inserted,
    Rows_Deleted : Natural := 0;
    Is_Data_Collector : Boolean := EV.Value("BOT_USER") = "dry" ;
    
    
  begin
    Outer_Loop : for Table in Tables_Type'range loop
       Num := 30;
       Inner_Loop : loop
           Log("about to insert into " & Table'Img & "OLD " & " in chunks of 1 days worth of data, Num =" & Num'Img);      
           T.Start;
             Do_Move(Table).Prepare(
               "insert into " & Table'Img & "OLD " & 
               "select * from " & Table'Img & " " &
               "where IXXLUTS < current_timestamp - interval ':NUM days' "
             );
            Do_Move(Table).Set("NUM",Num); 
            begin 
              Do_Move(Table).Execute(Rows_Inserted);
              Log("Moved into " & Table'Img & "OLD " & Rows_Inserted'Img);      
            exception
              when Sql.No_Such_Row => Rows_Inserted := 0;
            end ;       
            
            Do_Delete(Table).Prepare(
               "delete from " & Table'Img & " " & 
               "where IXXLUTS < current_timestamp - interval ':NUM days' " 
             );
            Do_Delete(Table).Set("NUM",Num); 
            begin 
              Do_Delete(Table).Execute(Rows_Deleted);
              Log("Deleted from " & Table'Img & Rows_Deleted'Img);      
            exception
              when Sql.No_Such_Row => Rows_Deleted := 0;
            end ;       
            
           T.Commit;
           Log("chunk ready, Moved" & Rows_Inserted'Img & " and deleted" & Rows_Deleted'Img);
           Num := Num -1;
           exit Inner_Loop when Num = 7; -- leave a week              
       end loop Inner_Loop ;
    end loop Outer_Loop;   
    
    -- for ordinary users , keep a month only, so the tables does not grow too large
    if not Is_Data_Collector then
      Num := 30000;
      T.Start;
      Delete_Loop : for Table in Tables_Type'range loop
        Do_Delete_Old(Table).Prepare(
           "delete from " & Table'Img & "OLD " & 
           "where IXXLUTS < current_timestamp - interval ':NUM days' " 
         );
         Do_Delete_Old(Table).Set("NUM",Num); 
         begin 
           Do_Delete_Old(Table).Execute(Rows_Deleted);
         exception
           when Sql.No_Such_Row => Rows_Deleted := 0;
         end ;       
         Log("Deleted from " &  Table'Img & "OLD :" & Rows_Deleted'Img);      
      end loop Delete_Loop;
      T.Commit;
    end if;
    
    
  end Run;
  ---------------------------------------------------------------------
  use type Sql.Transaction_Status_Type;
------------------------------ main start -------------------------------------

begin

   Define_Switch
    (Cmd_Line,
     Sa_Par_Bot_User'access,
     Long_Switch => "--user=",
     Help        => "user of bot");

   Define_Switch
     (Cmd_Line,
      Ba_Daemon'access,
      Long_Switch => "--daemon",
      Help        => "become daemon at startup");

   Define_Switch
     (Cmd_Line,
      Sa_Par_Inifile'access,
      Long_Switch => "--inifile=",
      Help        => "use alternative inifile");

  Getopt (Cmd_Line);  -- process the command line

  if Ba_Daemon then
    Posix.Daemonize;
  end if;
  Logging.Open(EV.Value("BOT_HOME") & "/log/" & EV.Value("BOT_NAME") & ".log");

   --must take lock AFTER becoming a daemon ...
   --The parent pid dies, and would release the lock...
  My_Lock.Take(EV.Value("BOT_NAME"));


  Log("Bot svn version:" & Bot_Svn_Info.Revision'Img);

  Ini.Load(Ev.Value("BOT_HOME") & "/" & "login.ini");
  Log(Me, "Connect Db");
  Sql.Connect
        (Host     => Ini.Get_Value("database", "host", ""),
         Port     => Ini.Get_Value("database", "port", 5432),
         Db_Name  => Ini.Get_Value("database", "name", ""),
         Login    => Ini.Get_Value("database", "username", ""),
         Password =>Ini.Get_Value("database", "password", ""));
  Log(Me, "db Connected");
  
  Run;
  
  Log(Me, "Close Db");
  Sql.Close_Session;
  Logging.Close;
  Posix.Do_Exit(0); -- terminate

exception
  when Lock.Lock_Error =>
    Log(Me, "lock error, exit");
    Logging.Close;
    Posix.Do_Exit(0); -- terminate
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
end Data_Mover;

