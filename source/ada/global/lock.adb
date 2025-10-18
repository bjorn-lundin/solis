with Ada.Environment_Variables;
with Logging; use Logging;
with Utils; use Utils;
with Interfaces.C.Strings;
with Calendar2;

package body Lock is
  Me : constant String := "Lock.";

  package EV renames Ada.Environment_Variables;

  procedure Take(A_Lock : in out Lock_Type; Name : in String) is
    Result : Int;
    use Interfaces.C.Strings;
  begin

    A_Lock.Name := To_Unbounded_String(Ev.Value("BOT_HOME") & "/locks/" & Name);
    Log(Me & "Take", "Take lock: '"  & To_String(A_Lock.Name) & "'");
    declare
      C_Name : Chars_Ptr := New_String (To_String(A_Lock.Name)) with Warnings => Off;
    begin
      A_Lock.Fd := Posix.Open(C_name, O_WRONLY + O_CREAT , 8#644#); --O_NONBLOCK is not needed
      Free(C_Name);
    end;

    Result := Posix.Lockf(A_Lock.Fd, F_TLOCK, 0); -- test lock, and lock if unlocked, error if already locked
    A_Lock.Currently_Holding_Lock := True;
    if Result = -1 then
      Log(Me & "Take", "Take lock failed, Errno =" & Errno'Img);
      A_Lock.Currently_Holding_Lock := False;
      raise Lock_Error with "Errno =" & Errno'Img ;
    end if;
  -- file is now locked
    --put pid in file
    declare
      --use Interfaces.C;
      use Calendar2;
      Str : String := Trim(Posix.Getpid'img & "|" &
                      Calendar2.String_Date_Time_ISO(Calendar2.Clock, " ","") & "|" &  -- now
                      Calendar2.String_Date_Time_ISO(Calendar2.Clock + (0,0,10,0,0), " ","") & "|" & --expire lock
                      Ascii.LF);
      C_Pid_Str : Chars_Ptr := New_String (Str) with Warnings => Off;
      Size      : Posix.Size_t;
    begin
      Size := Posix.Write(A_Lock.Fd, C_Pid_Str, Str'Length);
      Free(C_Pid_Str);
      if Integer(Size) = -1 then
        Log(Me & "Take", "write pid Errno =" & Errno'Img);
      end if;
      if Integer(Size) /= Str'length then
        Log(Me & "Take", "Size/str'length =" & Size'Img & "/" & Str'Length'Img);
      end if;
    end;
    Log(Me & "Take", "Lock taken");
  exception
    when others => raise Lock_Error;
  end Take;

  ------------------------------------------------------------------

  overriding procedure Finalize(A_Lock : in out Lock_Type) is
    Result : int;
  begin
    Log(Me & "Finalize", " start Remove loc");
      -- unlock file
    if A_Lock.Currently_Holding_Lock then
      Result := Posix.Lockf(A_Lock.Fd, F_ULOCK, 0); --unlock
      if Result = -1 then
        Log(Me & "Finalize", "Lockf failed in unlock/Finalize, Errno =" & Errno'Img);
      end if;
      Log(Me & "Finalize", "Lock removed");
    end if;
    Log(Me & "Finalize", " end Remove loc");
  end Finalize;
  ------------------------------------------------------------------


  procedure Write_File(Name : String;  Content : String) is
    Fd,
    Result : Posix.Int := Posix.Int'First;
    use Interfaces.C.Strings;
  begin
    declare
      C_Name : Chars_Ptr := New_String (Name) with Warnings => Off;
    begin
      Fd := Posix.Open(C_name, O_WRONLY + O_CREAT , 8#644#);
      Free(C_Name);
    end;

    Result := Posix.Lockf(Fd, F_TLOCK, 0); -- test lock, and lock if unlocked, error if already locked
    if Result = -1 then
      Log(Me & "Write_File", "Take lock failed, Errno =" & Errno'Img);
    end if;

    declare
      Str   : String := Content & Ascii.LF;
      C_Str : Chars_Ptr := New_String (Str) with Warnings => Off;
      Size  : Posix.Size_t;
    begin
      Size := Posix.Write(Fd, C_Str, Str'Length);
      Free(C_Str);
      if Integer(Size) = -1 then
        Log(Me & "Write_File", "write pid Errno =" & Errno'Img);
      end if;
      if Integer(Size) /= Str'length then
        Log(Me & "Write_File", "Size/str'length =" & Size'Img & "/" & Str'Length'Img);
      end if;
      Result := Posix.Lockf(Fd, F_ULOCK, 0); --unlock
      if Result = -1 then
        Log(Me & "Write_File", "Lockf failed in unlock, Errno =" & Errno'Img);
      end if;
      Result := Posix.Close(Fd);
      if Result = -1 then
        Log(Me & "Write_File", "close failed, Errno =" & Errno'Img);
      end if;
    end;

  end Write_File;


  function Read_File(Name : String ) return String is
    Fd,
    Result : Posix.Int := Posix.Int'First;
    use Interfaces.C.Strings;
  begin
    declare
      C_Name : Chars_Ptr := New_String (Name) with Warnings => Off;
    begin
      Fd := Posix.Open(C_Name,O_WRONLY , 8#644#);
      Free(C_Name);
    end;

    Result := Posix.Lockf(Fd, F_TLOCK, 0); -- test lock, and lock if unlocked, error if already locked
    if Result = -1 then
      Log(Me & "Read_File", "Take lock failed, Errno =" & Errno'Img);
      Result := Posix.Close(Fd);
      if Result = -1 then
        Log(Me & "Read_File", "close failed, Errno =" & Errno'Img);
      end if;
      return "";
    end if;
    -- ok, no-one else has the lock, relase it
    Result := Posix.Lockf(Fd, F_ULOCK, 0); --unlock
    if Result = -1 then
      Log(Me & "Read_File", "Lockf failed in unlock, Errno =" & Errno'Img);
    end if;

    -- ok, no-one else has the lock, close it
    Result := Posix.Close(Fd);
    if Result = -1 then
      Log(Me & "Read_File", "close failed, Errno =" & Errno'Img);
    end if;

    --reopen for read
    declare
      C_Name : Chars_Ptr := New_String (Name) with Warnings => Off;
    begin
      Fd := Posix.Open(C_Name, O_RDONLY, 8#644#);
      Free(C_Name);
    end;

    declare
      --type Buffer_String_Type is String(1..2_048):
      --type Buffer_String_Pointer is access all Buffer_String_Type;
      Str   : aliased Posix.Buffer_String_Type := (others => ' ') ;
      Size  : Posix.Size_t;
    begin
      Size := Posix.Read(Fd, Str'unchecked_access, Str'Length);
      if Integer(Size) = -1 then
        Posix.Perror("Posix.Read");
        Log(Me & "Read_File", "read =" & Errno'Img);
      end if;
      Result := Posix.Close(Fd);
      if Result = -1 then
        Log(Me & "Read_File", "close failed, Errno =" & Errno'Img);
      end if;
      return Str(1 .. Integer(Size));
    end;
  end Read_File;

  ------------------------------------------------------------------

end Lock;
