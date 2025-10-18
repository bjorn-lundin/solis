with Ada.Strings;                  use Ada.Strings;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Calendar2;
with Ada.Command_Line;
with Text_Io;

package body Stacktrace is

  procedure Log (W : string) is
  begin
    Text_Io.Put_Line(W);
  end Log;

  ------------------------------------------------------------------------------

  function Pure_Hexdump(Input : in String) return String is
    Found_Hex        : Boolean := False;
    Start_Of_Hex     : Integer := 0;
  begin
    if Input'Length > 0 then
      for i in Input'first +1 .. Input'last  loop
        if Input(i-1..i) = "0x" then
          Found_Hex := True;
          Start_Of_Hex := i-1;
          exit;
        end if;
      end loop;

      if Found_Hex then
        return Input(Start_Of_Hex .. Input'Last);
      end if;
    end if;
    return Input;
  exception
    when others => return Input;
  end Pure_Hexdump;

  ------------------------------------------------------------------------------

  procedure Tracebackinfo(E : Ada.Exceptions.Exception_Occurrence) is
    Now                     : constant String  := Calendar2.String_Date_And_Time(Milliseconds => True);
  begin
    Log(".....  Tracebackinfo at: " & Now & " .....");
    Log("Program terminated by an exception propagated out of the main subprogram.");
    Log("Exception raised : ");
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
      Command                 : Unbounded_String := Null_Unbounded_String;
    begin
      Log("Linux :");
      Log("");
      Log("");
      Log(Last_Exception_Name);
      Log("Message : " & Last_Exception_Messsage);
      Log(Last_Exception_Info);
      Append(Command, "addr2line" &
                    " --functions --basenames --exe=" &
                    Ada.Command_Line.Command_Name & " " &
                    Pure_Hexdump(Last_Exception_Info));

      Log ( To_String (Command));

      Command := Null_Unbounded_String;
      Log("-----------------");
      Log("Os X");

      Append(Command, "atos -o" &
                    Ada.Command_Line.Command_Name & " " &
                    Pure_Hexdump(Last_Exception_Info));
      Log ( To_String (Command));


    end ;
  end Tracebackinfo;
  ------------------------------------------------------------------------------

end Stacktrace;
