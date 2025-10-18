
with Lock;
with Ada.Command_Line;   use Ada.Command_Line;
with Gnat.Command_Line;  use Gnat.Command_Line;
with Gnat.Strings;
with Text_Io; use Text_Io;
with Logging;
procedure Check_Bot_Running is
  My_Lock    : Lock.Lock_Type with Warnings => Off;
  Sa_Par_Bot : aliased Gnat.Strings.String_Access;
  Ba_Debug   : aliased Boolean := False;
  Config     : Command_Line_Configuration;
begin
  Set_Exit_Status(Success);
  Define_Switch
    (Config,
     Sa_Par_Bot'access,
     "-b:",
     Long_Switch => "--botname=",
     Help        => "what bot to check");
  Define_Switch
    (Config,
     Ba_Debug'access,
     "-d",
     Long_Switch => "--debug",
     Help        => "Print stuff on stderr");
   Getopt (Config);  -- process the command line

  if Sa_Par_Bot.all = "" then
    Display_Help (Config);
    return;
  end if;
  if Ba_Debug then
    Put_Line(Standard_Error, "get lock: '" & Sa_Par_Bot.all & "'");
  else
    Logging.Set_Quiet(True);
  end if;
  My_Lock.Take(Sa_Par_Bot.all);
  if Ba_Debug then
    Put_Line(Standard_Error, "got lock - return SUCCESS (0)");
  end if;
exception
  when Lock.Lock_Error =>
    Set_Exit_Status(Failure);
    if Ba_Debug then
      Put_Line(Standard_Error, "did NOT get lock - return FAILURE (1)");
    end if;
end Check_Bot_Running;






