
with Ada.Directories;
with Text_Io;
with Calendar2;
with Ada.Io_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Stacktrace;
with Binary_Semaphores;

package body Logging is

   Quiet : Boolean := False;
   Global_Indent : Integer := 0;

   Global_File : Text_Io.File_Type;
   Global_Name : Unbounded_String :=  Null_Unbounded_String;
   Global_New_Log_File_On_Exit : Boolean := True;

   Dummy_Finalizer : Dummy_Type;
   pragma Warnings(Off, Dummy_Finalizer);
   Semaphore : Binary_Semaphores.Semaphore_Type;


   ---------------------------------------------
   procedure Change_Indent(How_Much : Integer) is
   begin
    Global_Indent := Global_Indent + How_Much;
   end Change_Indent;
   ---------------------------------------------

   function Indent return String is
    S : String (1 .. Global_Indent) := (others => ' ');
   begin
    return S;
   end Indent;

   ---------------------------------------------
   procedure New_Log_File_On_Exit(N : Boolean) is
     pragma Unreferenced(N);
   begin
     Global_New_Log_File_On_Exit := False;
   end New_Log_File_On_Exit;
   ---------------------------------------------
  --9.8-17200 new proc
  procedure Rename_Log_File (Name : in String; Copy : Boolean := False) is
    --------------------------
    function Timestamp_Format(T : in Calendar2.Time_Type) return String is
      use Calendar2;
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Ts  : String(1..18) := (others => '0');  -- YYYYMMDD_HHMMSSMMM
    begin
      Ada.Strings.Fixed.Move(Trim(Year_Type'Image(T.Year), Left), Ts(1..4), Justify => Right, Pad => '0');
      Ada.Strings.Fixed.Move(Trim(Month_Type'Image(T.Month), Left), Ts(5..6), Justify => Right, Pad => '0');
      Ada.Strings.Fixed.Move(Trim(Day_Type'Image(T.Day), Left), Ts(7..8), Justify => Right, Pad => '0');
      Ts(9) := '_';
      Ada.Strings.Fixed.Move(Trim(Hour_Type'Image(T.Hour), Left), Ts(10..11), Justify => Right, Pad => '0');
      Ada.Strings.Fixed.Move(Trim(Minute_Type'Image(T.Minute), Left), Ts(12..13), Justify => Right, Pad => '0');
      Ada.Strings.Fixed.Move(Trim(Second_Type'Image(T.Second), Left), Ts(14..15), Justify => Right, Pad => '0');
      Ada.Strings.Fixed.Move(Trim(Millisecond_Type'Image(T.Millisecond), Left), Ts(16..18), Drop => Right,
                             Justify => Right, Pad => '0');
      return Ts;
    end Timestamp_Format;

    function Timestamp return String is
    begin
      return Timestamp_Format(Calendar2.Clock);
    end Timestamp;

    --------------------------
    OK : Boolean := False;
  begin
    if not Ada.Directories.Exists(Name) then
      return;
    end if;

    declare
      use Ada.Directories;
      Dir  : String := Containing_Directory(Name);
      Base : String := Base_Name(Name);
      Ext  : String := Extension(Name);
      Rename_Log_File_Error : exception ; --9.8-18163
    begin
       --9.8-18163 loop
      for i in 1 .. 10 loop --9.8-18163
        declare
          Ts       : String := Timestamp;
          New_Name : String := Compose(Dir, Base & "_" & Ts & "." & Ext,"");
        begin
          if not Exists(New_Name) then
            if Copy then
              Copy_File(Name,New_Name);
            else
              Rename(Name,New_Name);
            end if;
            OK := True;
          end if;
        exception
          when Ada.Io_Exceptions.Name_Error =>
            Text_Io.Put_Line("Problems ren/cpy file '" & Name & "'");
            Text_Io.Put_Line("to                    '" & New_Name & "'");
          when E: others =>
             Stacktrace.Tracebackinfo(E);
        end;
        exit when OK;
 --9.8-18163        delay 0.001; -- wait for new millisecond
         delay 1.1; -- wait for new second --9.8-18163
      end loop;
      --9.8-18163
      if not OK then
        raise Rename_Log_File_Error with "Problems ren/cpy file '" & Name & "' Copy=: " & Boolean'Image(Copy);
      end if;
    end;
  end Rename_Log_File;

   --------------------------------------------
   procedure Set_Quiet (Q: Boolean) is
   begin
     Quiet := Q;
   end Set_Quiet;
   ---------------------------------------------
   procedure Log (Who, What : in String) is
   begin
     Log(Who & " : " & What);
   end Log;
   -------------------------------------------
   procedure Log (What : in String) is
     use Calendar2;
     use type Ada.Directories.File_Size;
   begin
      if Quiet then
        return;
      end if;
      Semaphore.Seize;
      if Text_Io.Is_Open(Global_File) then
        if Ada.Directories.Exists(To_String(Global_Name)) and then
           Ada.Directories.Size(To_String(Global_Name)) > 10_000_000 then
          Close;
          Rename_Log_File(To_String(Global_Name));
          Open(To_String(Global_Name));
        end if;
        Text_Io.Put_Line (Global_File, String_Date_Time_ISO (Clock, " " , "") & " " & What);
        Text_Io.Flush (Global_File);
      else
        Text_Io.Put_Line (Text_Io.Standard_Error, String_Date_Time_ISO (Clock, " " , "") & " " & What);
      end if;
      Semaphore.Release;
   end Log;
   ---------------------------------------------
   procedure Print (What : in String) is
   begin
      Text_Io.Put_Line (What);
   end Print;

   ---------------------------------------------------------
   procedure Open(Name : String) is
   begin
     if Ada.Directories.Exists(Name) then
       Text_Io.Open(Global_File, Text_Io.Append_File, Name);
     else
       Text_Io.Create(Global_File, Text_Io.Out_File, Name);
     end if;
     Global_Name := To_Unbounded_String(Name);
   end Open;
   ----------------------------------------------------------

   procedure Close is
   begin
     if Text_Io.Is_Open(Global_File) then
       Text_Io.Close(Global_File);
     end if;
   end Close;

   ----------------------------------------------
   overriding procedure Finalize(D : in out Dummy_Type) is
     pragma Warnings(Off,D);
   begin
     if Global_New_Log_File_On_Exit and then Text_Io.Is_Open(Global_File) then
       Rename_Log_File(To_String(Global_Name), True);
     end if;
   end Finalize;
   -------------------------------------------------------------------

end Logging;
