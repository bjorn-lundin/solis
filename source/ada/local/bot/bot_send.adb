with Ada.Exceptions;
with Ada.Command_Line;
with Logging; use Logging;
with Types; use Types;
with Stacktrace;
with Ada.Strings ; use Ada.Strings;
with Ada.Strings.Fixed ; use Ada.Strings.Fixed;
with Text_io;
with Bot_Messages;
with Process_Io;
with Core_Messages;
--with General_Routines; use General_Routines;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Utils;
procedure Bot_Send is
  Sa_Par_Rec  : aliased Gnat.Strings.String_Access;
  Sa_Par_Msg  : aliased Gnat.Strings.String_Access;
  Sa_Par_Data : aliased Gnat.Strings.String_Access;
  Cmd_Line : Command_Line_Configuration;
  Receiver : Process_Io.Process_Type := ((others => ' '),(others => ' '));
  
begin
    Define_Switch
       (Cmd_Line,
        Sa_Par_Rec'access,
        "-r:",
        Long_Switch => "--receiver=",
        Help        => "receiver of msg");
  
    Define_Switch
        (Cmd_Line,
         Sa_Par_Msg'access,
         "-m:",
         Long_Switch => "--message=",
         Help        => "what message to send");
         
    Define_Switch
        (Cmd_Line,
         Sa_Par_Data'access,
         "-d:",
         Long_Switch => "--data=",
         Help        => "what data to send, first field");

    Getopt (Cmd_Line);  -- process the command line 

    
    if Sa_Par_Rec.all = "" then
       Display_Help (Cmd_Line);
      return;
    end if;
    if Sa_Par_Msg.all = "" then
       Display_Help (Cmd_Line);
      return;
    end if;

    
    if    Utils.Lower_Case(Sa_Par_Msg.all) = "re_read_config" then
      declare
        RCR : Core_Messages.Read_Config_Record;
      begin
        if Sa_Par_Data.all /= "" then
          RCR.Dummy := Integer_4'Value(Sa_Par_Data.all);
        end if;
        Move(Sa_Par_Rec.all, Receiver.Name);
        Core_Messages.Send(Receiver, RCR);
      end;
    elsif Utils.Lower_Case(Sa_Par_Msg.all) = "exit" then
      declare
        ER : Core_Messages.Exit_Record;
      begin
        if Sa_Par_Data.all /= "" then
          ER.Dummy := Integer_4'Value(Sa_Par_Data.all);
        end if;
        Move(Sa_Par_Rec.all, Receiver.Name);
        Core_Messages.Send(Receiver, ER);
      end;
    elsif Utils.Lower_Case(Sa_Par_Msg.all) = "market_notification" then
      declare
        MNR : Bot_Messages.Market_Notification_Record;
      begin
        if Sa_Par_Data.all /= "" then
          Move(Sa_Par_Data.all, MNR.Market_Id);
        else
          Text_Io.Put_Line(Text_Io.Standard_Error, "Market_Notification_Record needs data");
          return;
        end if;
        
        Move(Sa_Par_Rec.all, Receiver.Name);
        Bot_Messages.Send(Receiver, MNR);
      end;
    elsif Utils.Lower_Case(Sa_Par_Msg.all) = "place_back_bet" then
      declare
        PBB : Bot_Messages.Place_Back_Bet_Record;
      begin
        if Sa_Par_Data.all /= "" then
          Move(Sa_Par_Data.all, PBB.Market_Id);
          Move("TEST_BOT_NAME_FOR_A_HORSE_MARKET", PBB.Bet_Name);
          Move("200.26", PBB.Price);
          Move("1.34", PBB.Size);
          PBB.Selection_Id := 12650;          
        else
          Text_Io.Put_Line(Text_Io.Standard_Error, "Place_Back_Bet_Record needs data");
          return;
        end if;
        
        Move(Sa_Par_Rec.all, Receiver.Name);
        Bot_Messages.Send(Receiver, PBB);
      end;
    
    else
      Text_Io.Put_Line(Text_Io.Standard_Error, "only these messges are supported");
      Text_Io.Put_Line(Text_Io.Standard_Error, "re_read_config");
      Text_Io.Put_Line(Text_Io.Standard_Error, "exit");
      Text_Io.Put_Line(Text_Io.Standard_Error, "market_notification");
    
    end if;

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
end Bot_Send;
