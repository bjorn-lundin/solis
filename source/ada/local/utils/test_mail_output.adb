with Ada;
--with Ada.Environment_Variables;
--with Ada.Directories;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Unbounded;

with GNAT;
with GNAT.Sockets;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;


with Stacktrace;
with Calendar2; use Calendar2;

with Rpc;
with Lock ;
with Posix;
with Logging; use Logging;

--with Process_IO;
--with Core_Messages;
with Text_io; use Text_io;

procedure Test_Mail_Output is
  --package EV renames Ada.Environment_Variables;
  use type Rpc.Result_Type;
  
  Me : constant String := "Main.";  

  Sa_Par_Subject : aliased Gnat.Strings.String_Access;
  Cmd_Line        : Command_Line_Configuration;
---------------------------------------------------------


  function Get_From_Std_In return String is
    use  Ada.Strings.Unbounded;
    US : Unbounded_String;
    Ch : Character := ' ';
  begin
    loop
      exit when End_Of_File;
      Get_Immediate(Ch);
      if Ch /= Ascii.LF then
        Append(US, Ch);
      else
        Log (Me & "Mail", "got LF");
        Append(US, Ascii.CR);
        Append(US, Ch);
      end if;       
    end loop;
    return Ada.Strings.Unbounded.To_String(US);    
  end Get_From_Std_In;



  procedure Mail(Subject : String) is
     T       : Calendar2.Time_Type := Calendar2.Clock;
    -- SMTP_Server_Name : constant String := "email-smtp.eu-west-1.amazonaws.com"; 
  begin
    declare
      use Ada.Characters.Latin_1;
      Msg : constant String := Get_From_Std_In &
          Cr & Lf &          
          "timestamp: " & Calendar2.String_Date_Time_ISO (T, " ", " ") & Cr & Lf &
          "sent from: " & GNAT.Sockets.Host_Name ;
    begin     
      Log (Me & "Mail", "subject: " & Subject);
      Log (Me & "Mail", "body: " & Msg);
    end;                   
  end Mail;

---------------------------------  
     
------------------------------ main start -------------------------------------
begin
 
  --Logging.Open(EV.Value("BOT_HOME") & "/log/aws_mailer.log");
  
  Define_Switch
   (Cmd_Line,
    Sa_Par_Subject'access,
    Long_Switch => "--subject=",
    Help        => "subject to mail");
    
  Getopt (Cmd_Line);  -- process the command line

  Log(Me, "start mail");
  Mail(Sa_Par_Subject.all);
  Log(Me, "do_exit");
  Posix.Do_Exit(0); -- terminate
 
exception
  when Lock.Lock_Error => 
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
    Posix.Do_Exit(0); -- terminate
end Test_Mail_Output;

