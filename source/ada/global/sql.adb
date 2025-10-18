------------------------------------------------------------------------------
--                                                                          --
--                                  SQL                                     --
--                                                                          --
--                                 Body                                     --
--                                                                          --
--  Copyright (c) Björn Lundin 2014                                         --
--  All rights reserved.                                                    --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions      --
--  are met:                                                                --
--  1. Redistributions of source code must retain the above copyright       --
--     notice, this list of conditions and the following disclaimer.        --
--  2. Redistributions in binary form must reproduce the above copyright    --
--     notice, this list of conditions and the following disclaimer in      --
--     the documentation and/or other materials provided with the           --
--     distribution.                                                        --
--  3. Neither the name of Björn Lundin nor the names of its contributors   --
--     may be used to endorse or promote products derived from this         --
--     software without specific prior written permission.                  --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY BJÖRN LUNDIN AND CONTRIBUTORS ``AS         --
--  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       --
--  FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL BJÖRN       --
--  LUNDIN OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,              --
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES                --
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR      --
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)      --
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN               --
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR            --
--  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,          --
--  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Characters.Handling;
with Unchecked_Deallocation;
with Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Utils;
with Logging; use Logging;
--with text_io;
with C_Constants;

package body Sql is
  Me : constant String := "Sql";

 -- type Float_8 is new Long_Float;


  Global_Statement_Index_Counter : Natural := 0;
  Global_Connection              : Connection_Type;  -- the default connection;
  Global_Transaction             : Transaction_Type ;  -- the only REAL allowed transaction;

  Global_Indent_Level            : Integer := 0;
  Global_Indent_Step             : Integer := 2;
  Global_Transaction_Identity    : Transaction_Identity_Type;


  Global_Transaction_Counter_Current : Integer_4 := 0;
  Global_Transaction_Counter_Max     : Integer_4 := 100_000;

  type Error_Type is (Error_Duplicate_Index, Error_No_Such_Object, Error_No_Such_Column);
  type Error_Array_Type is array (Error_Type'Range) of Boolean;


  ------------------------------------------------------------
  procedure Decrease_Global_Indent is
  begin
    Global_Indent_Level := Global_Indent_Level - Global_Indent_Step;
  end Decrease_Global_Indent;

  procedure Increase_Global_Indent is
  begin
    Global_Indent_Level := Global_Indent_Level + Global_Indent_Step;
  end Increase_Global_Indent;

  ------------------------------------------------------------
  function Make_Dollar_Variable (Idx : Natural ) return String is
  begin
    return Utils.Skip_All_Blanks (" $" & Idx'Img);
  end Make_Dollar_Variable ;
  ------------------------------------------

  --   procedure Finalize (T : in out Transaction_Type) is
  --   begin -- we do not want to leave scoop with a running transaction !!
  --     null;
  --     return;
  --     if T.Counter > 0 then
  --       raise Transaction_Error with "Uncommited Transaction went out of scoop!";
  --     end if;
  --   end Finalize;

  ------------------------------------------

  procedure Free is new Unchecked_Deallocation (Private_Statement_Type, Private_Statement_Type_Ptr);

  --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
  -- called by prepare. We dont get elaborate warnings now
  procedure Do_Initialize (Statement : in out Statement_Type) is
  begin
    if Statement.Private_Statement = null then
      Statement.Private_Statement := new Private_Statement_Type;
      Statement.Private_Statement.Do_Initialize;
    end if;
  end Do_Initialize;

  overriding procedure Finalize (Statement : in out Statement_Type) is
  begin
    Free (Statement.Private_Statement);
  end Finalize;
  --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++

  -- called by prepare. We dont get elaborate warnings now
  procedure Do_Initialize (Private_Statement : in out Private_Statement_Type) is
  begin
    Global_Statement_Index_Counter := Global_Statement_Index_Counter + 1;
    Private_Statement.Index                   := Global_Statement_Index_Counter;
    --    Log(Me, "Initialize Private_Statement_Type # " & Private_Statement.Index'Img);
    Ada.Strings.Fixed.Move ("S" & Utils.Trim (Private_Statement.Index'Img), Private_Statement.Statement_Name);

    Private_Statement.Cursor_Name     := Private_Statement.Statement_Name;
    Private_Statement.Cursor_Name (1) := 'C';
  end Do_Initialize;

  overriding procedure Finalize (Private_Statement : in out Private_Statement_Type) is
  begin
    null;
    -- Map.Release (Private_Statement.Parameter_Map);
  end Finalize;
  --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++

  procedure Associate (Private_Statement : in out Private_Statement_Type;
                       Bind_Varible      : String;
                       Idx               : Natural) is
    Local_Map_Item : Parameter_Map_Type;
  begin
    Local_Map_Item := (
                       Index          => Idx,
                       Name           => To_Unbounded_String (Bind_Varible),
                       Value          => To_Unbounded_String (""),
                       Parameter_Type => Not_Set
                      );
    --      Log(Me, "Associate : '" & Bind_Varible & " -> " & Idx'Img);
    Private_Statement.Parameter_Map.Append (Local_Map_Item);

  end	Associate;

  --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++

  procedure Update_Map (Private_Statement      : in out Private_Statement_Type;
                        Bind_Varible           : in     String;
                        Value                  : in     String;
                        Parameter_Type         : in     Parameter_Type_Type) is
    Found    : Boolean := False;
  begin

    -- gnat 4.6.3 workaround  start

    -- error: assignment to loop parameter not allowed
    --   for Local_Map_Item of Private_Statement.Parameter_Map loop
    --      -- updates in list, directly
    --      if To_String (Local_Map_Item.Name) = Bind_Varible then
    --         Local_Map_Item.Value := To_Unbounded_String (Value);
    --         Local_Map_Item.Parameter_Type := Parameter_Type;
    --         Found := True;
    --      end if;
    --   end loop;

    declare
      use Map;
      C : Cursor;

      Local_Map_Item : Parameter_Map_Type;
    begin
      C := First (Private_Statement.Parameter_Map);
      while Has_Element (C) loop -- Print current value

        if To_String (Element (C).Name) = Bind_Varible then

          Local_Map_Item :=  Element (C);

          Local_Map_Item.Value := To_Unbounded_String (Value);
          Local_Map_Item.Parameter_Type := Parameter_Type;

          Replace_Element (Private_Statement.Parameter_Map, C, Local_Map_Item);

          Found := True;
        end if;

        Next (C);
      end loop;
    end;



    -- gnat 4.6.3 workaround  stop

    if not Found then
      raise No_Such_Parameter with Bind_Varible;
    end if;


  end Update_Map;
  --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++

  procedure Check_Is_Prepared (Private_Statement : in out Private_Statement_Type) is
  begin
    if not Private_Statement.Is_Prepared then
      raise Sequence_Error;
    end if;
  end Check_Is_Prepared;
  --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++

  procedure Exchange_Binder_Variables (Private_Statement : in out Private_Statement_Type) is
    Index                          : Natural                       := 0;
    Orig_Stm                       : String                        := To_String (Private_Statement.Original_Statement);
    Cmd                            : String (1 .. Orig_Stm'Last + 1) := Orig_Stm & " ";
    Binder_Parameter_Position_Stop : Integer := 0;
    ------------------------------------------
  begin
    -- Log(Me, "Exchange_Binder_Variables-start (Original_Statement) '" & Orig_Stm & "'");
    Command_Loop : for I in Cmd'Range loop
      --allow postgresql's '::' casting by check char before and after this one,
      -- if not at ends of cmd
      if Cmd (I) = ':' and then
        I /= Cmd'Last and then Cmd (I + 1) /= ':' and then
        I /= Cmd'First and then Cmd (I - 1) /= ':' then
        Index := Index + 1;
        Associate_Loop : for J in I + 1 .. Cmd'Last loop
          case Cmd (J) is
            when ' ' | ')' | ',' =>
              Private_Statement.Associate (Cmd (I + 1 .. J - 1), Index);
              Append (Private_Statement.Pg_Prepared_Statement, Make_Dollar_Variable (Index) );
              Binder_Parameter_Position_Stop := J;
              exit Associate_Loop;
              when others => null;
          end case;
        end loop Associate_Loop;
        --        Log(Me, "Binder_Parameter_Position_Stop :" & Binder_Parameter_Position_Stop'Img);
      else
        -- we skip the part replaced by eg $2
        -- ...      and XLOCID = :XLOCID and XLOCSIZ >= :XLOCSIZ turns to
        -- ...      and XLOCID = $2 and XLOCSIZ >= $3
        -- so skip (on first line  ^^^^^    and            ^^^^^
        -- by setting Binder_Parameter_Position_Stop to the char AFTER the bindword
        if I >= Binder_Parameter_Position_Stop then
          Append (Private_Statement.Pg_Prepared_Statement, Cmd (I));
        end if;
      end if;
    end loop Command_Loop;
    Private_Statement.Number_Parameters := Index;
    Private_Statement.Is_Prepared := True;
    --  Log(Me, "Exchange_Binder_Variables-stop (PG_Prepared_Statement) '" & To_String (Private_Statement.Pg_Prepared_Statement) & "'");

  end Exchange_Binder_Variables;

  ------------------------------------------------------------
  procedure Fill_Data_In_Prepared_Statement (Private_Statement : in out Private_Statement_Type) is
    Tmp         : Unbounded_String := Private_Statement.Pg_Prepared_Statement;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Replace_Dollar_Place_Holder (S   : in out Unbounded_String;
                                           Lmi : in     Parameter_Map_Type) is
      Cmd         : String  := To_String (S);
      Start, Stop : Integer := 0;
      Look_For    : String  := Make_Dollar_Variable (Lmi.Index);
    begin
      Start := Utils.Position (Cmd, Look_For);
      if Start > Cmd'First - 1 then
        Stop := Start + Look_For'Length ;
        --        Log(Me, "Cmd(Cmd'First .. Start-1) " & Cmd(Cmd'First .. Start-1) );
        --        Log(Me, "LMI.Value                 " & To_String(LMI.Value));
        --        Log(Me, "Cmd(Stop .. Cmd'Last)     " & Cmd(Stop .. Cmd'Last) );
        case Lmi.Parameter_Type is
          when An_Integer | A_Float  =>
            S := To_Unbounded_String (Cmd (Cmd'First .. Start - 1)) &
              Lmi.Value &
              To_Unbounded_String (Cmd (Stop .. Cmd'Last));
          when A_Character | A_Date | A_Time | A_Timestamp =>
            S := To_Unbounded_String (Cmd (Cmd'First .. Start - 1) & "'") &
              Lmi.Value &
              To_Unbounded_String ("'" & Cmd (Stop .. Cmd'Last));
          when A_String   =>
            declare
              Trimmed_Value : String := Utils.Trim (To_String (Lmi.Value));
            begin
              S := To_Unbounded_String (
                                        Cmd (Cmd'First .. Start - 1) &
                                          "" & Trimmed_Value & "" &
                                          Cmd (Stop .. Cmd'Last));
            end;
          when Null_Type =>
            S := To_Unbounded_String (
                                      Cmd (Cmd'First .. Start - 1) &
                                        "null" &
                                        Cmd (Stop .. Cmd'Last));
          when Not_Set   => raise Sequence_Error with Look_For & " is NOT set";
        end case;
        --     else
        --        Log(Me, "Fill_Data_In_Prepared_Statement.Replace_Dollar_Place_Holder Did not find '" & Look_For & "' in '" & Cmd & "'");
      end if;

    end Replace_Dollar_Place_Holder;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
  begin


    if not Private_Statement.Is_Prepared then
      Log (Me, "Fill_Data_In_Prepared_Statement. Was not prepared: '" & To_String (Tmp) & "'");
      raise Sequence_Error;
    end if;

    --    Log(Me, "Fill_Data_In_Prepared_Statement.start: '" & To_String (Tmp) & "'");

    -- gnat 4.6.3 workaround  start
    --      for Local_Map_Item of Private_Statement.Parameter_Map loop
    --         Replace_Dollar_Place_Holder (Tmp, Local_Map_Item);
    --      end loop;

    declare
      use Map;
      C : Cursor;
    begin
      C := First (Private_Statement.Parameter_Map);
      while Has_Element (C) loop -- Print current value
        Replace_Dollar_Place_Holder (Tmp, Element (C));
        Next (C);
      end loop;
    end;
    -- gnat 4.6.3 workaround  stop

    Private_Statement.Prepared_Statement := Tmp;
    --    Log(Me, "Fill_Data_In_Prepared_Statement.stop: '" & To_String (Tmp) & "'");
  end Fill_Data_In_Prepared_Statement;

  ------------------------------------------------------------
  -- start local procs
  ------------------------------------------------------------

  function Get_New_Transaction return Transaction_Identity_Type is
  begin
    Global_Transaction_Identity := Transaction_Identity_Type'Succ (Global_Transaction_Identity);
    return Global_Transaction_Identity;
  end Get_New_Transaction;

  ------------------------------------------------------------

  procedure Print_Errors (Myunit   : in String;
                          Mystatus : in Exec_Status_Type) is
  begin
    if ((Mystatus /= Command_Ok) and
           (Mystatus /= Tuples_Ok)) then
      Log (Me,  Myunit);
      Log (Me,  Error_Message (Global_Connection));
      Log (Me, Mystatus'Img);
    end if;
  end Print_Errors;

  ------------------------------------------------------------

  function Pgerror (Local_Status : Exec_Status_Type) return Boolean is
    Failure : Boolean := True;
  begin
    Failure := ((Local_Status /= Command_Ok) and (Local_Status /= Tuples_Ok));
    if Failure then
      Log (Me, "PGerror: " & Local_Status'Img);
    end if;
    return Failure;
  end Pgerror;

  --------------------------------------------------------------------------------

  function Convert_To_Time (Mytime : String) return Calendar2.Time_Type is
    Local_Time : Calendar2.Time_Type := Calendar2.Time_Type_First;
  begin -- '11:22:32'
    Local_Time.Hour   := Calendar2.Hour_Type'Value (Mytime (1 .. 2));
    Local_Time.Minute := Calendar2.Minute_Type'Value (Mytime (4 .. 5));
    Local_Time.Second := Calendar2.Second_Type'Value (Mytime (7 .. 8));
    return Local_Time;
  exception
    when Constraint_Error =>
      raise Conversion_Error with "Failed to convert : '" & Mytime & "' to a time";
  end Convert_To_Time;

  --------------------------------------------------------------------------------

  function Convert_To_Date (Mydate : String) return Calendar2.Time_Type is
    Local_Date : Calendar2.Time_Type := Calendar2.Time_Type_First;
  begin  -- '2002-01-06'
    Local_Date.Year  := Calendar2.Year_Type'Value (Mydate (1 .. 4));
    Local_Date.Month := Calendar2.Month_Type'Value (Mydate (6 .. 7));
    Local_Date.Day   := Calendar2.Day_Type'Value (Mydate (9 .. 10));
    return Local_Date;
  exception
    when Constraint_Error =>
      raise Conversion_Error with "Failed to convert : '" & Mydate & "' to a date";
  end Convert_To_Date;

  ------------------------------------------------------------
  function Convert_To_Timestamp (Mytimestamp : String) return Calendar2.Time_Type is
    Local_Timestamp : Calendar2.Time_Type := Calendar2.Time_Type_First;
  begin -- '2002-01-06 11:22:32.123' or
    -- '2002-01-06 11:22:32.12' or
    -- '2002-01-06 11:22:32.1' or
    -- '2002-01-06 11:22:32'
    Local_Timestamp.Year        := Calendar2.Year_Type'Value (Mytimestamp (1 .. 4));
    Local_Timestamp.Month       := Calendar2.Month_Type'Value (Mytimestamp (6 .. 7));
    Local_Timestamp.Day         := Calendar2.Day_Type'Value (Mytimestamp (9 .. 10));
    Local_Timestamp.Hour        := Calendar2.Hour_Type'Value (Mytimestamp (12 .. 13));
    Local_Timestamp.Minute      := Calendar2.Minute_Type'Value (Mytimestamp (15 .. 16));
    Local_Timestamp.Second      := Calendar2.Second_Type'Value (Mytimestamp (18 .. 19));
    Local_Timestamp.Millisecond := 0;
    if Mytimestamp'Length = 21 then -- ms like '.1'
      Local_Timestamp.Millisecond := Calendar2.Millisecond_Type'Value (Mytimestamp (21 .. 21) & "00");
    elsif Mytimestamp'Length = 22 then -- ms like '.14'
      Local_Timestamp.Millisecond := Calendar2.Millisecond_Type'Value (Mytimestamp (21 .. 22) & "0");
    elsif Mytimestamp'Length = 23 then -- ms like '.143'
      Local_Timestamp.Millisecond := Calendar2.Millisecond_Type'Value (Mytimestamp (21 .. 23));
    end if;
    return Local_Timestamp;
  exception
    when Constraint_Error =>
      raise Conversion_Error with "Failed to convert : '" & Mytimestamp & "' to a timestamp";
  end Convert_To_Timestamp;

  ------------------------------------------------------------


  -- end local procs
  ------------------------------------------------------------

  ------------------------------------------------------------
  -- start connection related proces
  --------------------------------------------------------------

  procedure Connect (Host       : in String  := "";
                     Port       : in Natural := 5432;
                     Options    : in String  := "";
                     Tty        : in String  := "";
                     Db_Name    : in String  := "";
                     Login      : in String := "";
                     Password   : in String := "";
                     SSL_Mode   : in String := "prefer") is
    Local_Status : Connection_Status_Type;
  begin

    if Global_Connection.Get_Connected then
      Log (Me, "Already connected, disconnect first : db_name,login,: '" & Db_Name & "', '" & Login & "'");
      return;
    end if;

    Global_Connection.Set_Host (Host);
    Global_Connection.Set_Port (Port);
    Global_Connection.Set_Options (Options);
    Global_Connection.Set_Tty (Tty);
    Global_Connection.Set_Db_Name (Db_Name);
    Global_Connection.Set_User (Login);
    Global_Connection.Set_Password (Password);

    declare
      Login_String : String :=  "host=" & Host & " " &
                       "port=" & Port'Img & " " &
                       "dbname=" & Db_Name & " " &
                       "user=" & Login & " " &
                       "password=" & Password & " " &
                       "sslmode=" & SSL_Mode;
    begin
      if Ada.Environment_Variables.Exists ("BOT_NAME") then
        Global_Connection.Login (Conn_Info => Login_String & " application_name=" &
                                   Ada.Characters.Handling.To_Lower (Ada.Environment_Variables.Value ("BOT_NAME")));
      else
        Global_Connection.Login (Conn_Info => Login_String & " application_name=no_name");
      end if;

      Local_Status := Status (Global_Connection);
      case Local_Status is
        when Connection_Ok =>
          Global_Connection.Set_Connected (True);
          Set_Transaction_Isolation_Level (Read_Commited, Session);
          --      declare
          --        Enc : String := Global_Connection.Database_Encoding;
          begin
            case C_Constants.Os is
              when C_Constants.Mac_X64 => null;
              when C_Constants.Lnx_X64 | C_Constants.Lnx_A32 =>
                Global_Connection.Set_Encoding (Latin_1);
                Global_Connection.Set_Client_Encoding ("LATIN1");
            end case;
          end;

        when Connection_Bad =>
          Global_Connection.Set_Connected (False);
          Log (Me, "Connect : db_name,login,password ->: '" & Db_Name & "', '" & Login & "', '" & Password & "'");
          Log (Me, "Connect : Login_String ->: '" & Login_String & "'");
          Log (Me, Error_Message (Global_Connection));
          raise Not_Connected with "Sql.Connect: Not_Connected" ;
      end case;
    end;
  end Connect;
  --------------------------------------------------------------
  procedure Reconnect is
    Local_Status : Connection_Status_Type;
  begin
    Log (Me, "Reconnect");
    Global_Connection.Login;

    Local_Status := Status (Global_Connection);
    case Local_Status is
      when Connection_Ok =>
        Global_Connection.Set_Connected (True);
        Set_Transaction_Isolation_Level (Read_Commited, Session);
        begin
            case C_Constants.Os is
              when C_Constants.Mac_X64 => null;
              when C_Constants.Lnx_X64 | C_Constants.Lnx_A32 =>
                Global_Connection.Set_Encoding (Latin_1);
                Global_Connection.Set_Client_Encoding ("LATIN1");
            end case;
        end;

      when Connection_Bad =>
        Global_Connection.Set_Connected (False);
        Log (Me, Error_Message (Global_Connection));
        raise Not_Connected with "Sql.Connect: Not_Connected" ;
    end case;
  end Reconnect;

  ---------------------------------------------------------------

  function Transaction_In_Progress return Boolean ;

  procedure Close_Session is
  begin
    if Is_Session_Open then
      if Transaction_In_Progress then
        --reset transactions
        begin
          Rollback (Global_Transaction);
        exception
          when Transaction_Error => null; --rollback what we did not start...
        end;
        Global_Transaction.Counter := 0;
      end if;
      Global_Statement_Index_Counter := 0;
      Global_Connection.Finish;
      Global_Connection.Set_Connected (False);
      Global_Transaction.Status := None;
    end if;
    --  Log(Me, "Session closed");
  end Close_Session;

  --------------------------------------------------------------

  function Database return Database_Type is
  begin
    return Postgresql;
  end Database;


  ----------------------------------------------------------------
  function Is_Session_Open return Boolean is
  begin
    return Global_Connection.Get_Connected;
  end Is_Session_Open;

  --------------------------------------------------------------
  -- end connection related procs
  --------------------------------------------------------------

  --------------------------------------------------------------
  -- start transaction handling procs
  --------------------------------------------------------------
  function Transaction_In_Progress return Boolean is
  begin
    return Global_Transaction.Counter > 0;
  end Transaction_In_Progress;

  --------------------------------------------------------------
  procedure Check_Is_Connected is
  begin
    if not Global_Connection.Get_Connected then
      raise Not_Connected;
    end if;
  end Check_Is_Connected;

  ------------------------------------------------------------

  procedure Check_Transaction_In_Progress is
  begin
    if not Transaction_In_Progress then
      raise No_Transaction;
    end if;
  end Check_Transaction_In_Progress;

  ------------------------------------------------------------


  procedure Start_Transaction (T  : in out Transaction_Type;
                               Ts : in Transaction_Status_Type) is
    Dml_Status  : Exec_Status_Type;
    Dml_Result  : Result_Type;
  begin
    Increase_Global_Indent;
    Check_Is_Connected;
    -- check if transaction already in progress
    case Global_Transaction.Status is
      when None =>
        -- reconnect to kill libpq process. It grows ...
        Global_Transaction_Counter_Current := Global_Transaction_Counter_Current + 1;
        if Global_Transaction_Counter_Current >= Global_Transaction_Counter_Max then
          Global_Transaction_Counter_Current := 0;
          Close_Session;
          Reconnect;
        end if;

        T.Counter := Get_New_Transaction;
        Global_Transaction.Counter := T.Counter;
      when Read_Only =>
        if Ts = Read_Write then
          raise Transaction_Error with "current transaction is Read_Only, new is Read_Write!";
        end if;
        T.Counter := Get_New_Transaction;
        return;  -- do nothing

      when Read_Write =>
        if Ts = Read_Only then
          raise Transaction_Error with "current transaction is Read_Write, new is Read_Only!";
        end if;
        T.Counter := Get_New_Transaction;
        return;  -- do nothing
    end case;

    Global_Connection.Exec ("begin", Dml_Result);
    Dml_Status := Dml_Result.Result_Status;
    Dml_Result.Clear;
    if Pgerror (Dml_Status) then
      Print_Errors ("Start_Transaction", Dml_Status);
      raise Postgresql_Error;
    end if;

    Global_Transaction.Status := Ts;
  end Start_Transaction;

  ---------------------------------------------------------------

  procedure Start_Read_Only_Transaction (T : in out Transaction_Type) is
  begin
    Start_Transaction (T, Read_Only);
  end Start_Read_Only_Transaction;

  ---------------------------------------------------------------

  procedure Start_Read_Write_Transaction (T : in out Transaction_Type) is
  begin
    Start_Transaction (T, Read_Write);
  end Start_Read_Write_Transaction;

  ------------------------------------------------------------

  procedure Commit (T : in out Transaction_Type) is
    Dml_Status  : Exec_Status_Type;
    Dml_Result  : Result_Type;
  begin
    Check_Is_Connected;
    Check_Transaction_In_Progress;
    -- check if transaction already in progress
    case Global_Transaction.Status is
      when None =>
        raise No_Transaction with "Commit: No_Transaction";

      when Read_Only | Read_Write =>
        -- check for ownership
        if T.Counter /= Global_Transaction.Counter then
          T.Counter := 0;
          -- not the owner, do nothing
          -- Log(Me, "not the owner tries to commit");
          return;
        end if;
    end case;

    --Log(Me, "commit");
    Global_Connection.Exec ("commit", Dml_Result);
    Dml_Status := Dml_Result.Result_Status;
    Dml_Result.Clear;

    if Pgerror (Dml_Status) then
      Print_Errors ("commit", Dml_Status);
      raise Postgresql_Error;
    end if;

    T.Counter := 0;
    Global_Transaction.Counter := 0;
    Global_Transaction.Status := None;
    --    Log(Me, "the owner commits");
    Decrease_Global_Indent;
  end Commit;

  ------------------------------------------------------------

  procedure Rollback (T : in out Transaction_Type) is
    Dml_Status  : Exec_Status_Type;
    Dml_Result  : Result_Type;
  begin
    Check_Is_Connected;
    Check_Transaction_In_Progress;

    case Global_Transaction.Status is
      when None =>
        raise No_Transaction with "Rollback: No_Transaction";

      when Read_Only | Read_Write =>
        -- check for ownership
        if T.Counter /= Global_Transaction.Counter then
          -- not the owner
          T.Counter := 0;
          raise Transaction_Error with "not the owner tries to rollback";
        end if;
    end case;
    -- Log(Me, "rollback");
    Global_Connection.Exec ("rollback", Dml_Result);
    Dml_Status := Dml_Result.Result_Status;
    Dml_Result.Clear;
    if Pgerror (Dml_Status) then
      Print_Errors ("rollback", Dml_Status);
      raise Postgresql_Error;
    end if;

    T.Counter := 0;
    Global_Transaction.Counter := 0;
    Global_Transaction.Status := None;
    Decrease_Global_Indent;
  end Rollback;

  --------------------------------------------------------------

  function Transaction_Status return Transaction_Status_Type is
  begin
    return Global_Transaction.Status;
  end Transaction_Status;

  --------------------------------------------------------------
  procedure Set_Transaction_Isolation_Level (Level : in Transaction_Isolation_Level_Type;
                                             Scope : in Transaction_Isolation_Level_Scope_Type) is
    Local_Transaction   : Transaction_Type with Warnings => Off;
    Transaction_Setting : array (Transaction_Isolation_Level_Type'Range,
                                  Transaction_Isolation_Level_Scope_Type'Range) of Statement_Type;
  begin
    Start_Read_Write_Transaction (Local_Transaction);
    case Level is
      when Read_Commited =>
        case Scope is
          when Transaction =>
            Prepare (Transaction_Setting (Level, Scope), "SET TRANSACTION ISOLATION LEVEL READ COMMITTED");
          when Session =>
            Prepare (Transaction_Setting (Level, Scope), "SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL READ COMMITTED");
        end case;
      when Serializable =>
        case Scope is
          when Transaction =>
            Prepare (Transaction_Setting (Level, Scope), "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE");
          when Session =>
            Prepare (Transaction_Setting (Level, Scope), "SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL SERIALIZABLE");
        end case;
    end case;
    Execute (Transaction_Setting (Level, Scope));
    Commit (Local_Transaction);
  end Set_Transaction_Isolation_Level;


  --------------------------------------------------------------
  -- end transaction handling procs
  --------------------------------------------------------------


  --------------------------------------------------------------
  -- start public cursor handling procs
  --------------------------------------------------------------

  procedure Prepare (Private_Statement : in out Private_Statement_Type;
                     Command           : in String) is
    use Ada.Characters.Handling;
    Stm : String := Utils.Trim (To_Lower (Command));
  begin
    if not Private_Statement.Is_Prepared then
      Private_Statement.Do_Initialize; -- instead of using Initialize, and get warnings
      -- Log(Me, "Prepare - First time Stm: '" & Stm & "'");
      if    Stm (1 .. 6) = "select" then
        Private_Statement.Type_Of_Statement := A_Select;
      elsif Stm (1 .. 6) = "insert" then
        Private_Statement.Type_Of_Statement := An_Insert;
      elsif Stm (1 .. 6) = "update" then
        Private_Statement.Type_Of_Statement := An_Update;
      elsif Stm (1 .. 6) = "delete" then
        Private_Statement.Type_Of_Statement := A_Delete;
      else
        Private_Statement.Type_Of_Statement := A_Ddl;
      end if;
      Private_Statement.Original_Statement := To_Unbounded_String (Command) ;
      Private_Statement.Exchange_Binder_Variables; -- sets Is_Prepared
      -- Log(Me, "Prepare - PGPrepared_stm: '" & To_String (Private_Statement.Pg_Prepared_Statement) & "'");
      --      declare
      --        use Interfaces.C, Interfaces.C.Strings;
      --        Types_Array    : Pgada.Thin.Int_Array_Type(1..3) := (0,0,0);
      --      begin
      --        Prepare(Connection    => Global_Connection.Connection,
      --                Statment_Name => Private_Statement.Statement_Name,
      --                Query         => To_String(Private_Statement.Prepared_Statement),
      --                Number_Params => Private_Statement.Number_Parameters,
      --                Param_Types   => Types_Array,
      --                Result        => DML_Result);
      --        Status := Result_Status(DML_Result);
      --        Clear(DML_Result);
      --        if PGerror(Status) then
      --          Print_Errors("Prepare",Status);--          raise PostgreSQL_Error;
      --        end if;
      --      end;
      --      Private_Statement.Is_Prepared := True;
      --  else
      --     Log(Me, "Prepare - Already prepared Stm: '" & Stm & "'");
    else
      if To_String(private_Statement.Original_Statement) /= Command then
        raise Sequence_Error with "statement differs from old '" & To_String(Private_Statement.Original_Statement) & "'";
      end if;
    end if;
  end Prepare;
  ------------------------------------------------------------
  procedure Prepare (Statement : in out Statement_Type;
                     Command   : in String) is
  begin
    Statement.Do_Initialize;
    Prepare (Statement.Private_Statement.all, Command);
  end Prepare;
  ------------------------------------------------------------

  function Determine_Errors (Result : Result_Type; Stm_String : String) return Error_Array_Type is
    -- see http://www.postgresql.org/docs/9.3/interactive/errcodes-appendix.html
    Local_Array : Error_Array_Type := (others => False);
    Sql_State   : String := Error_Sql_State (Result);
    -----------------------------------------------------
    procedure Print_Diagnostics (Label, Content : String) is
    begin
      if Content'Length > 0 then
        Log (Me, Label & ": '" & Content & "'");
      end if;
    end Print_Diagnostics;
    -----------------------------------------------------
    procedure Print_All_Diagnostics is
    begin
      Print_Diagnostics ("Sql_State", Sql_State);
      Print_Diagnostics ("Severity", Error_Severity (Result));
      Print_Diagnostics ("Message Primary", Error_Message_Primary (Result));
      Print_Diagnostics ("Message Detail", Error_Message_Detail (Result));
      Print_Diagnostics ("Message Hint", Error_Message_Hint (Result));
      Print_Diagnostics ("Statement Position", Error_Statement_Position (Result));
      Print_Diagnostics ("Internal Position", Error_Internal_Position (Result));
      Print_Diagnostics ("Internal Query", Error_Internal_Query (Result));
      Print_Diagnostics ("Context", Error_Context (Result));
      -- Print_Diagnostics("Source File", Error_Source_File(Result));
      -- Print_Diagnostics("Source Line", Error_Source_Line(Result));
      -- Print_Diagnostics("Source Function", Error_Source_Function(Result));
      Print_Diagnostics ("Statement", Stm_String);

    end Print_All_Diagnostics;
  begin
    if Sql_State = "23505" then                    -- UNIQUE VIOLATION
      Local_Array (Error_Duplicate_Index) := True;
    elsif Sql_State = "42703" then                 --	UNDEFINED COLUMN
      Local_Array (Error_No_Such_Column) := True;
    elsif Sql_State = "42P01" then                 --	UNDEFINED TABLE
      Local_Array (Error_No_Such_Object) := True;
    else
      Print_All_Diagnostics;                       -- only print info on unknown errors
    end if;
    return Local_Array;
  end Determine_Errors;
  ---------------------------------------
  function Determine_Errors (Private_Statement : Private_Statement_Type) return Error_Array_Type is
    Ea : Error_Array_Type := Determine_Errors (Private_Statement.Result, To_String (Private_Statement.Prepared_Statement));
  begin
    --      for i in EA'range loop
    --        if EA(i) then
    --          Log(Me, To_String(Private_Statement.Prepared_Statement));
    --          exit;
    --        end if;
    --      end loop;
    return Ea;
  end Determine_Errors;
  --------------------------------------------------
  procedure Open_Cursor (Private_Statement : in out Private_Statement_Type) is
    Status           : Exec_Status_Type;
    Savepoint_Result : Result_Type;
  begin
    -- check for open database and prepared Statement too!!
    -- declare/open the cursor and execute the Statement
    Check_Is_Connected;
    Check_Transaction_In_Progress;
    if Private_Statement.Is_Open then
      raise Sql_Error with "Open_Cursor: Cursor already open";
    end if;
    Private_Statement.Is_Open := True;
    Private_Statement.Fill_Data_In_Prepared_Statement;

    -- Log(Me, "SQL.OPEN_CURSOR: " & To_String (Private_Statement.Original_Statement));

    Global_Connection.Exec ("savepoint A_SELECT", Savepoint_Result);
    Status := Savepoint_Result.Result_Status;
    Savepoint_Result.Clear;
    if Pgerror (Status) then
      Print_Errors ("Open_Cursor savepoint A_SELECT", Status);
      raise Postgresql_Error;
    end if;

    declare
      Declare_String   : String :=
                           "declare " & Private_Statement.Cursor_Name & " cursor without hold for " &
                           To_String (Private_Statement.Prepared_Statement);
      Dml_Result       : Result_Type;
    begin
      Global_Connection.Exec (Declare_String, Dml_Result);
      -- Log(Me, "SQL.OPEN_CURSOR: " & Declare_String);
      Status := Dml_Result.Result_Status;
      if Pgerror (Status) then
        Print_Errors ("Open_Cursor", Status);
        declare
          Errors : Error_Array_Type := Determine_Errors (Dml_Result, Declare_String);
        begin
          Dml_Result.Clear;
          Global_Connection.Exec ("rollback to savepoint A_SELECT", Savepoint_Result);
          Status := Savepoint_Result.Result_Status;
          Savepoint_Result.Clear;
          if Pgerror (Status) then
            Print_Errors ("Open_Cursor rollback to savepoint A_SELECT", Status);
            raise Postgresql_Error;
          end if;

          if    Errors (Error_Duplicate_Index) then
            raise Duplicate_Index;
          elsif Errors (Error_No_Such_Object) then
            raise No_Such_Object;
          elsif Errors (Error_No_Such_Column) then
            raise No_Such_Column;
          end if;
        end;
        raise Postgresql_Error with To_String (Private_Statement.Prepared_Statement);
      else
        Dml_Result.Clear;
      end if;
    end;

    Global_Connection.Exec ("release savepoint A_SELECT", Savepoint_Result);
    Status := Savepoint_Result.Result_Status;
    Savepoint_Result.Clear;
    if Pgerror (Status) then
      Print_Errors ("Open_Cursor savepoint release A_SELECT", Status);
      raise Postgresql_Error;
    end if;
  end Open_Cursor;

  --------------------------------------------
  procedure Open_Cursor (Statement : in Statement_Type) is
  begin
    Open_Cursor (Statement.Private_Statement.all);
  end Open_Cursor;

  ------------------------------------------------------------

  procedure Fetch (Private_Statement  : in out Private_Statement_Type;
                   End_Of_Set         : out Boolean) is
    Dml_Status : Exec_Status_Type;
    Ntpl       : Natural := 0;
  begin
    Check_Is_Connected;
    Check_Transaction_In_Progress;
    Private_Statement.Check_Is_Prepared;
    if not Private_Statement.Is_Open then
      raise Sql_Error with "Fetch: Cursor is not open";
    end if;

    if Private_Statement.Current_Row = Private_Statement.Number_Actually_Fetched then
      Private_Statement.Result.Clear; --leakfinder --clear old result if any
      -- Ok first time, or we have already fetched
      --  Private_Statement.Number_Actually_Fetched rows.
      -- we need to get another Private_Statement.Number_To_Fetch rows into
      -- our result set
      declare
        Fetch_String : String := "fetch forward" & Private_Statement.Number_To_Fetch'Img & " in " & Private_Statement.Cursor_Name;
      begin
        -- Log(Me, "Fetch: " & Fetch_String);
        Global_Connection.Exec (Fetch_String, Private_Statement.Result);
      end;
      Dml_Status := Result_Status (Private_Statement.Result);
      -- Log(Me, "Fetched from db");

      if Pgerror (Dml_Status) then
        Print_Errors ("Fetch", Dml_Status);
        raise Postgresql_Error;
      end if;

      begin
        --Text_Io.Put_Line("Fetch, Rows_Affected (1)" & Ntpl'Img);
        Ntpl := Rows_Affected (Private_Statement.Result);
        --Text_Io.Put_Line("Fetch, Rows_Affected (2)" & Ntpl'Img);
      exception
        when Constraint_Error => Ntpl := 0;
      end;
      End_Of_Set := (Ntpl = 0);
      Private_Statement.Current_Row := 1;
      Private_Statement.Number_Actually_Fetched := Ntpl;
      -- Log(Me, "Number_Actually_Fetched" & Private_Statement.Number_Actually_Fetched'Img);
    else
      -- just point to the next row in the cached resultset
      Private_Statement.Current_Row := Private_Statement.Current_Row + 1;
      End_Of_Set := False;
      -- Log(Me, "Fetched from cached cursor");
    end if;
    -- Log(Me, "current row is now" &Private_Statement.Current_Row'Img);
  end Fetch;

  procedure Fetch (Statement  : in Statement_Type;
                   End_Of_Set : out Boolean) is
  begin
    Fetch (Statement.Private_Statement.all, End_Of_Set);
  end Fetch;

  ------------------------------------------------------------

  procedure Close_Cursor (Private_Statement : in out Private_Statement_Type) is
    Dml_Status  : Exec_Status_Type;
    Dml_Result  : Result_Type;
  begin
    -- remove cursor and association?
    Check_Is_Connected;
    Check_Transaction_In_Progress;
    if not Private_Statement.Is_Open then
      raise Sql_Error with "Close_Cursor : Cursor is not open";
    end if;
    Private_Statement.Is_Open := False;
    Private_Statement.Result.Clear; --clear old result
    declare
      Close_String : String := "close " & Private_Statement.Cursor_Name;
    begin
      Global_Connection.Exec (Close_String, Dml_Result);
      -- Log(Me, "Close_This_Cursor -> " & Close_String);
    end;
    Dml_Status := Result_Status (Dml_Result);
    Dml_Result.Clear;
    if Pgerror (Dml_Status) then
      Print_Errors ("Close_This_Cursor", Dml_Status);
      raise Postgresql_Error;
    end if;
    Private_Statement.Current_Row := 0;
    Private_Statement.Number_Actually_Fetched := 0;
    -- Log(Me, "Close_cursor " & "Marked OK to Close " & Private_Statement.Cursor_Name);
  end Close_Cursor;

  procedure Close_Cursor (Statement : in Statement_Type) is
  begin
    Close_Cursor (Statement.Private_Statement.all);
  end Close_Cursor;

  -------------------------------------------------------------

  procedure Execute (Private_Statement           : in out Private_Statement_Type;
                     No_Of_Affected_Rows         : out Natural) is
    Status : Exec_Status_Type;
    type Savepoint_Handling_Type is (Insert, Remove, Rollback_To);

    -------------------------------------------------------------------------

    -------------------------------------------------------------------------
    procedure Handle_Savepoint (How             : in Savepoint_Handling_Type;
                                P_Stm           : in out Private_Statement_Type) is
      Dml_Status  : Exec_Status_Type;
      Dml_Result  : Result_Type;
    begin
      case How is
        when Insert      => Global_Connection.Exec ("savepoint " & P_Stm.Type_Of_Statement'Img, Dml_Result);
        when Remove      => Global_Connection.Exec ("release savepoint " & P_Stm.Type_Of_Statement'Img, Dml_Result);
        when Rollback_To => Global_Connection.Exec ("rollback to savepoint " & P_Stm.Type_Of_Statement'Img, Dml_Result);
      end case;

      Dml_Status := Dml_Result.Result_Status;
      Dml_Result.Clear;

      if Pgerror (Dml_Status) then
        case How is
          when Insert      => Print_Errors ("savepoint " & P_Stm.Type_Of_Statement'Img, Dml_Status);
          when Remove      => Print_Errors ("release savepoint " & P_Stm.Type_Of_Statement'Img, Dml_Status);
          when Rollback_To => Print_Errors ("rollback to savepoint " & P_Stm.Type_Of_Statement'Img, Dml_Status);
        end case;
        raise Postgresql_Error;
      end if;
    end Handle_Savepoint;
    -------------------------------------------------------------------------
    procedure Handle_Error (P_Stm : in out Private_Statement_Type; Clear_Statement : in Boolean) is
      Errors      : Error_Array_Type  := Determine_Errors (P_Stm);
    begin
      if Clear_Statement then
        P_Stm.Result.Clear;
      end if;
      -- rollback to the savepoint
      Handle_Savepoint (How => Rollback_To, P_Stm => P_Stm);

      -- remove the savepoint
      Handle_Savepoint (How => Remove, P_Stm => P_Stm);

      if    Errors (Error_Duplicate_Index) then
        raise Duplicate_Index;
      elsif Errors (Error_No_Such_Object) then
        raise No_Such_Object;
      elsif Errors (Error_No_Such_Column) then
        raise No_Such_Column;
      else
        Log (Me, "");
        Log (Me, "---------------------------------------------------------");
        Log (Me, "see http://www.postgresql.org/docs/9.3/interactive/errcodes-appendix.html");
        Log (Me, "---------------------------------------------------------");
        Print_Errors (To_String (P_Stm.Prepared_Statement), Status);
        Log (Me, "sql ->: '" & To_String (P_Stm.Prepared_Statement) & "'");
        Log (Me, "---------------------------------------------------------");
        Log (Me, "");
        raise Postgresql_Error;
      end if;
    end Handle_Error;
    ----------------------------------------------------------------------------
  begin
    -- check for open database and prepared Statement too!!
    -- declare/open the cursor and execute the Statement
    -- Log(Me, "Execute start");
    Check_Is_Connected;
    Check_Transaction_In_Progress;

    if Transaction_Status /= Read_Write then
      Log (Me, "Exceute: current transaction type is: " & Transaction_Status'Img);
      raise Sequence_Error;
    end if;

    -- extract cursor.Query to its Bindvarables here!
    -- raise sequence error if not all are bound!
    Private_Statement.Fill_Data_In_Prepared_Statement;

    -- Log(Me, "Original_Statement    '" & To_String (Private_Statement.Original_Statement));
    -- Log(Me, "PG_Prepared_Statement '" & To_String (Private_Statement.Pg_Prepared_Statement));
    -- Log(Me, "Prepared_Statement    '" & To_String (Private_Statement.Prepared_Statement));

    -- Log(Me, "Execute will run      '" & To_String (Private_Statement.Prepared_Statement) & "'");
    -- Log(Me, "Escaped string is     '" & Escape (Global_Connection, To_String (Private_Statement.Prepared_Statement) & "'"));

    case Private_Statement.Type_Of_Statement is
      when A_Select  => raise Sequence_Error;
      when An_Insert =>
        --  Log(Me, "Execute.Insert start");
        No_Of_Affected_Rows := 1;

        -- text_io.put_line(To_String (Private_Statement.Prepared_Statement));

        Handle_Savepoint (How => Insert, P_Stm => Private_Statement);
        Global_Connection.Exec (To_String (Private_Statement.Prepared_Statement),
                                Private_Statement.Result);
        Status := Private_Statement.Result.Result_Status;

        if Pgerror (Status) then
          Handle_Error (P_Stm => Private_Statement, Clear_Statement => False);
        end if;
        Private_Statement.Result.Clear;
        Handle_Savepoint (How => Remove, P_Stm => Private_Statement);

        -- Log(Me, "Execute.Insert end");

      when A_Delete  =>
        -- Log(Me, "Execute.Delete start");
        No_Of_Affected_Rows := Natural'Last;

        Handle_Savepoint (How => Insert, P_Stm => Private_Statement);

        Global_Connection.Exec (To_String (Private_Statement.Prepared_Statement),
                                Private_Statement.Result);
        Status := Private_Statement.Result.Result_Status;

        if Pgerror (Status) then
          Handle_Error (P_Stm => Private_Statement, Clear_Statement => False);
        end if;

        No_Of_Affected_Rows := Rows_Affected (Private_Statement.Result);
        Private_Statement.Result.Clear;

        Handle_Savepoint (How => Remove, P_Stm => Private_Statement);

        -- Log(Me, "Execute.Delete end");

      when An_Update =>
        -- Log(Me, "Execute.Update start");
        No_Of_Affected_Rows := Natural'Last;
        Handle_Savepoint (How => Insert, P_Stm => Private_Statement);

        Global_Connection.Exec (To_String (Private_Statement.Prepared_Statement),
                                Private_Statement.Result);
        Status := Private_Statement.Result.Result_Status;

        if Pgerror (Status) then
          Handle_Error (P_Stm => Private_Statement, Clear_Statement => False);
        end if;

        No_Of_Affected_Rows := Rows_Affected (Private_Statement.Result);
        Private_Statement.Result.Clear;

        Handle_Savepoint (How => Remove, P_Stm => Private_Statement);
        -- Log(Me, "Execute.Update end");

      when A_Ddl     =>
        -- Log(Me, "Execute.DDL start");
        Handle_Savepoint (How => Insert, P_Stm => Private_Statement);
        No_Of_Affected_Rows := Natural'Last;
        Global_Connection.Exec (To_String (Private_Statement.Prepared_Statement),
                                Private_Statement.Result);
        Status := Private_Statement.Result.Result_Status;
        if Pgerror (Status) then
          Handle_Error (P_Stm => Private_Statement, Clear_Statement => False);
        end if;
        Private_Statement.Result.Clear;
        Handle_Savepoint (How => Remove, P_Stm => Private_Statement);
        -- Log(Me, "Execute.DDL end");
    end case;
    -- Log(Me, "Execute end");
  end Execute;
  -----------------------------------------------------------
  procedure Execute (Statement           : in Statement_Type;
                     No_Of_Affected_Rows : out Natural) is
  begin
    Execute (Statement.Private_Statement.all, No_Of_Affected_Rows);
  end Execute;
  ------------------------------------------------------------

  procedure Execute (Statement : in  Statement_Type) is
    Rows : Natural := 0;
  begin
    Execute (Statement.Private_Statement.all, Rows);
    if Rows = 0 then
      raise No_Such_Row;
    end if;
  end Execute;
  ------------------------------------------------------------

  function Is_Null (Statement : Statement_Type;
                    Parameter : Positive) return Boolean is
  begin
    return Pgada.Database.Is_Null (Statement.Private_Statement.Result,
                                   Tuple_Index_Type (Statement.Private_Statement.Current_Row),
                                   Field_Index_Type (Parameter));
  end Is_Null;

  ------------------------------------------------------------

  function Is_Null (Statement : Statement_Type;
                    Parameter : String) return Boolean is
  begin
    return Is_Null (Statement, Positive (Field_Index (Statement.Private_Statement.Result, Parameter)));
  end Is_Null;

  --------------------------------------------------------------
  -- end cursor handling procs
  ------------------------------------------------------------

  --------------------------------------------------------------
  -- start Set handling procs
  ------------------------------------------------------------

  procedure Set (Statement : in out Statement_Type;
                 Parameter : in String;
                 Value     : in String) is
    Local_Value : constant String := Utils.Trim (Escape (Global_Connection, Value));
  begin
    if Local_Value (Local_Value'First) = ''' and then
      Local_Value (Local_Value'Last) = ''' then
      Statement.Private_Statement.Update_Map (Parameter,
                                              "'" &  Utils.Trim (Local_Value (Local_Value'First + 1 .. Local_Value'Last - 1)) & "'",
                                              A_String);
    else
      Statement.Private_Statement.Update_Map (Parameter, Local_Value, A_String);
    end if;
  end Set;

  ------------------------------------------------------------

  procedure Set (Statement : in out Statement_Type;
                 Parameter : in String;
                 Value     : in Integer_4) is
  begin
    Statement.Private_Statement.Update_Map (Parameter, Utils.Trim (Value'Img), An_Integer);
  end Set;


  ------------------------------------------------------------
  procedure Set (Statement : in out Statement_Type;
                 Parameter : in String;
                 Value     : in Integer_8) is
  begin
    Statement.Private_Statement.Update_Map (Parameter, Utils.Trim (Value'Img), An_Integer);
  end Set;


  ------------------------------------------------------------

  procedure Set (Statement : in out Statement_Type;
                 Parameter : in String;
                 Value     : in Fixed_Type) is
  begin
    Statement.Private_Statement.Update_Map (Parameter, Utils.Trim (Value'Img), A_Float);
  end Set;


  ---------------------------------------------------------
  procedure Set (Statement : in out Statement_Type;
                 Parameter : in String;
                 Value     : in Character) is
    Local_Value : String (1 .. 1) := (others => ' ');
  begin
    Local_Value (1) := Value;
    Statement.Private_Statement.Update_Map (Parameter, Local_Value, A_Character);
  end Set;

  -------------------------------------------------------------

  procedure Set (Statement : in out Statement_Type;
                 Parameter : in String;
                 Value     : in Boolean) is
  begin
    Statement.Private_Statement.Update_Map (Parameter, Utils.Lower_Case (Value'Img), A_String);
  end Set;

  -------------------------------------------------------------

  procedure Set_Date (Statement : in out Statement_Type;
                      Parameter : in String;
                      Value     : in Calendar2.Time_Type) is
  begin
    Statement.Private_Statement.Update_Map (Parameter, Calendar2.String_Date (Value), A_Date);
  end Set_Date;
  ------------------------------------------------------------

  procedure Set_Time (Statement : in out Statement_Type;
                      Parameter : in String;
                      Value     : in Calendar2.Time_Type) is
    Local_Time_1 : constant String := Calendar2.String_Time (Value);
    Local_Time_2 : String (1 .. 6) := (others => ' ');
  begin
    Local_Time_2 (1 .. 2) := Local_Time_1 (1 .. 2);  -- remove ':' from time
    Local_Time_2 (3 .. 4) := Local_Time_1 (4 .. 5);
    Local_Time_2 (5 .. 6) := Local_Time_1 (7 .. 8);

    Statement.Private_Statement.Update_Map (Parameter, Local_Time_2, A_Time);

  end Set_Time;
  ------------------------------------------------------------
  procedure Set_Timestamp (Statement : in out Statement_Type;
                           Parameter : in String;
                           Value     : in Calendar2.Time_Type) is
    Local_Time_1 : constant String := Calendar2.String_Date_Time_ISO (Date => Value, T => " ", TZ => "");

    --    Local_Time_2 : String(1..6) := (others => ' ');
  begin -- '2002-01-06 11:22:32.123'
    --    Local_Time_2(1..2) := Local_Time_1(1..2);  -- remove ':' from time
    --    Local_Time_2(3..4) := Local_Time_1(4..5);
    --    Local_Time_2(5..6) := Local_Time_1(7..8);

    --    Statement.Private_Statement.Update_Map(Parameter, Local_Time_2, A_Time);

    -- Log(Me, "Set_Timestamp: '" & Local_Time_1 & "'");
    Statement.Private_Statement.Update_Map (Parameter, Local_Time_1, A_Timestamp);
  end Set_Timestamp;
  ------------------------------------------------------------

  procedure Set (Statement : in out Statement_Type;
                 Parameter : in String;
                 Value     : in  Calendar2.Time_Type) is
  begin
    Statement.Set_Timestamp (Parameter, Value);
  end Set;

  -----------------------------------------------------------
  procedure Set (Statement : in out Statement_Type;
                 Parameter : in String;
                 Value     : in  Ada.Calendar.Time) is
  begin
    Statement.Set_Timestamp (Parameter, Calendar2.To_Time (Value));
  end Set;

  -----------------------------------------------------------

  procedure Set_Null (Statement : in out Statement_Type;
                      Parameter : String) is
  begin
    Statement.Private_Statement.Update_Map (Parameter, "NULL", Null_Type);
  end Set_Null;

  ------------------------------------------------------------

  procedure Set_Null_Date (Statement : in out Statement_Type;
                           Parameter : String) is
  begin
    Statement.Private_Statement.Update_Map (Parameter, "NULL", Null_Type);
  end Set_Null_Date;

  --------------------------------------------------------------
  -- end Set handling procs
  ------------------------------------------------------------

  --------------------------------------------------------------
  -- start Get handling procs
  ------------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in Positive;
                 Value     : out Integer_4) is
  begin
    declare
      Local_String : constant String := Get_Value (Statement.Private_Statement.Result,
                                                   Tuple_Index_Type (Statement.Private_Statement.Current_Row),
                                                   Field_Index_Type (Parameter));
    begin
      -- Log(Me, "local_String: '" & Local_String & "'");
      if Local_String'Length = 0 then
        Value := 0;
      else
        Value := Integer_4'Value (Local_String);
      end if;
    end;
  exception
    when Constraint_Error =>
      raise No_Such_Column with "No such column: " & Parameter'Img ;
  end Get;


  ------------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in String;
                 Value     : out Integer_4) is
    Field_Number : Field_Index_Type := Field_Index (Statement.Private_Statement.Result, Parameter);
  begin
    Get (Statement, Positive (Field_Number), Value);
  end Get;


  ------------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in Positive;
                 Value     : out Integer_8) is
  begin
    declare
      Local_String : constant String := Get_Value (Statement.Private_Statement.Result,
                                                   Tuple_Index_Type (Statement.Private_Statement.Current_Row),
                                                   Field_Index_Type (Parameter));
    begin
      -- Log(Me, "local_String: '" & Local_String & "'");
      if Local_String'Length = 0 then
        Value := 0;
      else
        Value := Integer_8'Value (Local_String);
      end if;
    end;
  exception
    when Constraint_Error =>
      raise No_Such_Column with "No such column: " & Parameter'Img ;
  end Get;


  ------------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in String;
                 Value     : out Integer_8) is
    Field_Number : Field_Index_Type := Field_Index (Statement.Private_Statement.Result, Parameter);
  begin
    Get (Statement, Positive (Field_Number), Value);
  end Get;


  ------------------------------------------------------------


  procedure Get (Statement : in Statement_Type;
                 Parameter : in Positive;
                 Value     : out String) is
  begin
    declare
      Local_String : constant String :=
                       Get_Value (Statement.Private_Statement.Result,
                                  Tuple_Index_Type (Statement.Private_Statement.Current_Row),
                                  Field_Index_Type (Parameter));
    begin
      -- Log(Me, "Parameter:" & Parameter'Img & " - value '" & Local_String & "'");
      Value := (others => ' ');
      if Local_String'Length > 0 then
        Value (1 .. Local_String'Length) := Local_String;
      end if;
    exception
      when Constraint_Error =>
        raise No_Such_Column with "No such column: " & Parameter'Img ;
    end;
  end Get;


  -----------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in String;
                 Value     : out String) is
    Field_Number : Field_Index_Type := Field_Index (Statement.Private_Statement.Result, Parameter);
  begin
    -- Log(Me, "Parameter: '" & Parameter & "'");
    Get (Statement, Positive (Field_Number), Value);
  end Get;


  ----------------------------------------------------------


  ------------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in Positive;
                 Value     : out Character) is
  begin
    declare
      Local_String : constant String :=
                       Get_Value (Statement.Private_Statement.Result,
                                  Tuple_Index_Type (Statement.Private_Statement.Current_Row),
                                  Field_Index_Type (Parameter));
    begin
      if Local_String'Length = 0 then
        Value := ' ';
      else
        Value := Local_String (1);
      end if;
    end;
  exception
    when Constraint_Error =>
      raise No_Such_Column with "No such column: " & Parameter'Img ;
  end Get;


  ----------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in String;
                 Value     : out Character) is
    Field_Number : Field_Index_Type := Field_Index (Statement.Private_Statement.Result, Parameter);
  begin
    Get (Statement, Positive (Field_Number), Value);
  end Get;


  ----------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in Positive;
                 Value     : out Fixed_Type) is
  begin
    declare
      Local_String : constant String :=
                       Get_Value (Statement.Private_Statement.Result,
                                  Tuple_Index_Type (Statement.Private_Statement.Current_Row),
                                  Field_Index_Type (Parameter));
    begin
      if Local_String'Length = 0 then
        Value := 0.0;
      else
       -- Value := Fixed_Type (Float'Value (Local_String));
        Value := Fixed_Type'Value (Local_String);
      end if;
    end;
  exception
    when Constraint_Error =>
      raise No_Such_Column with "No such column: " & Parameter'Img ;
  end Get;


  ------------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in String;
                 Value     : out Fixed_Type) is
    Field_Number : Field_Index_Type := Field_Index (Statement.Private_Statement.Result, Parameter);
  begin
    Get (Statement, Positive (Field_Number), Value);
  end Get;


  ------------------------------------------------------------
  procedure Get (Statement : in Statement_Type;
                 Parameter : in Positive;
                 Value     : out Boolean) is
  begin
    declare
      Local_String : constant String :=
                       Get_Value (Statement.Private_Statement.Result,
                                  Tuple_Index_Type (Statement.Private_Statement.Current_Row),
                                  Field_Index_Type (Parameter));
    begin
      if Local_String'Length = 0 then
        Value := False;
      else
        if Local_String = "f" then
          Value := False;
        elsif Local_String = "t" then
          Value := True;
        else
          raise Conversion_Error with "Not handled boolean value: '" & Local_String & "'" ;
        end if;
      end if;
    end;
  exception
    when Constraint_Error =>
      raise No_Such_Column with "No such column: " & Parameter'Img ;
  end Get;

  ------------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in String;
                 Value     : out Boolean) is
    Field_Number : Field_Index_Type := Field_Index (Statement.Private_Statement.Result, Parameter);
  begin
    Get (Statement, Positive (Field_Number), Value);
  end Get;

  ------------------------------------------------------------

  procedure Get_Date (Statement : in Statement_Type;
                      Parameter : in Positive;
                      Value     : out Calendar2.Time_Type) is
  begin
    declare
      Local_String : constant String :=
                       Get_Value (Statement.Private_Statement.Result,
                                  Tuple_Index_Type (Statement.Private_Statement.Current_Row),
                                  Field_Index_Type (Parameter));
    begin
      if Local_String'Length = 0 then
        Value := Calendar2.Time_Type_First;
      else
        Value := Convert_To_Date (Local_String);
      end if;
    end;
  exception
    when Constraint_Error =>
      raise No_Such_Column with "No such column: " & Parameter'Img ;
  end Get_Date;
  ------------------------------------------------------------
  procedure Get_Date (Statement : in Statement_Type;
                      Parameter : in String;
                      Value     : out Calendar2.Time_Type) is
    Field_Number : Field_Index_Type := Field_Index (Statement.Private_Statement.Result, Parameter);
  begin
    Get_Date (Statement, Positive (Field_Number), Value);
  end Get_Date;

  ------------------------------------------------------------

  procedure Get_Time (Statement : in Statement_Type;
                      Parameter : in Positive;
                      Value     : out Calendar2.Time_Type) is
  begin
    declare
      Local_String : constant String :=
                       Get_Value (Statement.Private_Statement.Result,
                                  Tuple_Index_Type (Statement.Private_Statement.Current_Row),
                                  Field_Index_Type (Parameter));
    begin
      if Local_String'Length = 0 then
        Value := Calendar2.Time_Type_First;
      else
        Value := Convert_To_Time (Local_String);
      end if;
    end;
  exception
    when Constraint_Error =>
      raise No_Such_Column with "No such column: " & Parameter'Img ;
  end Get_Time;

  --------------------------------------------------------------

  procedure Get_Time (Statement : in Statement_Type;
                      Parameter : in String;
                      Value     : out Calendar2.Time_Type) is

    Field_Number : Field_Index_Type := Field_Index (Statement.Private_Statement.Result, Parameter);
  begin
    Get_Time (Statement, Positive (Field_Number), Value);
  end Get_Time;

  ---------------------------------------------------------------
  procedure Get_Timestamp (Statement : in Statement_Type;
                           Parameter : in Positive;
                           Value     : out Calendar2.Time_Type) is
  begin
    declare
      Local_String : constant String :=
                       Get_Value (Statement.Private_Statement.Result,
                                  Tuple_Index_Type (Statement.Private_Statement.Current_Row),
                                  Field_Index_Type (Parameter));
    begin
      if Local_String'Length = 0 then
        Value := Calendar2.Time_Type_First;
      else
        Value := Convert_To_Timestamp (Local_String);
      end if;
    end;
  exception
    when Constraint_Error =>
      raise No_Such_Column with "No such column: " & Parameter'Img ;
  end Get_Timestamp;

  --------------------------------------------------------------

  procedure Get_Timestamp (Statement : in Statement_Type;
                           Parameter : in String;
                           Value     : out Calendar2.Time_Type) is

    Field_Number : Field_Index_Type := Field_Index (Statement.Private_Statement.Result, Parameter);
  begin
    Get_Timestamp (Statement, Positive (Field_Number), Value);
  end Get_Timestamp;

  ------------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in Positive;
                 Value     : out Ada.Calendar.Time) is
    Tmp : Calendar2.Time_Type;
  begin
    Statement.Get_Timestamp (Parameter, Tmp);
    Value := Calendar2.To_Calendar_Time (Tmp);
  end Get;

  ------------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in String;
                 Value     : out Ada.Calendar.Time) is
    Tmp : Calendar2.Time_Type;
  begin
    Statement.Get_Timestamp (Parameter, Tmp);
    Value := Calendar2.To_Calendar_Time (Tmp);
  end Get;

  ------------------------------------------------------------
  procedure Get (Statement : in Statement_Type;
                 Parameter : in Positive;
                 Value     : out Calendar2.Time_Type) is
  begin
    Statement.Get_Timestamp (Parameter, Value);
  end Get;

  ------------------------------------------------------------

  procedure Get (Statement : in Statement_Type;
                 Parameter : in String;
                 Value     : out Calendar2.Time_Type) is
  begin
    Statement.Get_Timestamp (Parameter, Value);
  end Get;

  ------------------------------------------------------------

  function Get_Prepared_Statement (Statement : Statement_Type) return String is
  begin
    return To_String (Statement.Private_Statement.Prepared_Statement);
  end Get_Prepared_Statement;

  ------------------------------------------------------------
  -- end Get handling procs
  ------------------------------------------------------------

  procedure Get_Column_Info
    (Statement   : Statement_Type;
     Parameter   : Positive;
     Name        : out String;
     Namelen     : out Integer_4;
     Datatype    : out Integer_4;
     Datatypelen : out Integer_4) is
  begin
    null;
  end Get_Column_Info;

  function Get_Nbr_Columns (Statement : Statement_Type) return Integer is
  begin
    return Nbr_Fields (Statement.Private_Statement.Result);
  end Get_Nbr_Columns;

  ------------------------------------------------------------
end Sql;
