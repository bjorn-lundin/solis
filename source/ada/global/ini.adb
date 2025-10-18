--------------------------------------------------------------------------------
--
--	COPYRIGHT	Consafe Logistics AB, Lund
--
--	RESPONSIBLE	Björn Lundin
--
--	DESCRIPTION	Rewrite of package Settings. Uses String instead
--              of String and on unix, it works even if inifile is in dos format
--
--------------------------------------------------------------------------------
-- 6.7      21-AUG-1996  Henrik Dannberg
--                       Original version (Settings)
-- 9.4-6643 03-Sep-2004  SNE
--                       New procedure LOAD with filename as input.
--------------------------------------------------------------------------------
-- 9.8-17902    30-Oct-2009  New name+use String+work on unix when dos-style inifile
--------------------------------------------------------------------------------

with Text_IO;
with Unchecked_Deallocation;
--with System_Services;

--with Sequential_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Directories;
with Utils;
--with Ada.Command_Line;

package body Ini is

  Global_Is_Loaded : Boolean := False;
  type String_Pointer_Type is access String;

  type Variable_Type;

  type Variable_Pointer_Type is access Variable_Type;

  type Variable_Type is
    record
      Name  : String_Pointer_Type;
      Value : String_Pointer_Type;
      Next  : Variable_Pointer_Type;
    end record;

  type Section_Type;

  type Section_Pointer_Type is access Section_Type;

  type Section_Type is
    record
      Name         : String_Pointer_Type;
      Count        : Natural;
      Variable_List: Variable_Pointer_Type;
      Next         : Section_Pointer_Type;
    end record;

  type Data_Type is
    record
      Count       : Natural := 0;
      Section_List: Section_Pointer_Type := null;
    end record;

   Data: Data_Type;

  Current_Ini_File : Unbounded_String := Null_Unbounded_String; -- 9.8-17902

  procedure Free is new Unchecked_Deallocation (String, String_Pointer_Type);


  function Is_Match (Left, Right: String) return Boolean is
  begin -- 9.8-17902
    return Utils.Upper_Case(Left) = Utils.Upper_Case(Right);
  end Is_Match;


  function Collapse (Line: String) return String is
    R: String(Line'Range);
    J: Integer := R'First;
    Quote : Boolean := False;
  begin
    for I in Line'Range loop
      if (Quote) then
        if (Line(I) = '"') then
          Quote := False;
        else
          R(J) := Line(I); J := J + 1;
        end if;
      elsif (Line(I) = '"') then
        Quote := True;
      elsif (Line(I) = ';') then
        return R(1..J-1);
      elsif ((Line(I) /= ' ') and (Line(I) /= Ascii.HT)) then
        R(J) := Line(I); J := J + 1;
      end if;
    end loop;
    return R(1..J-1);
  end Collapse;


  function Find_Section (No: Positive) return Section_Pointer_Type is
    S    : Section_Pointer_Type := Data.Section_List;
    Count: Integer := 0;
  begin
    if (S = null) then
      return null;
    elsif (No > Data.Count) then
      return null;
    else
      while (S /= null) loop
        Count := Count + 1;
        if (No = Count) then
          return S;
        end if;
        S := S.Next;
      end loop;
    end if;
    return null;
  end Find_Section;


  function Find_Section (Name: String) return Section_Pointer_Type is
    S: Section_Pointer_Type := Data.Section_List;
  begin
    if (S = null) then
      return null;
    else
      while (S /= null) loop
        if (Is_Match(Name,S.Name.all)) then
          return S;
        end if;
        S := S.Next;
      end loop;
    end if;
    return null;
  end Find_Section;


  function Find_Variable (S : Section_Pointer_Type;
                          No: Positive) return Variable_Pointer_Type is
    V    : Variable_Pointer_Type;
    Count: Integer := 0;
  begin
    if (S = null) then
      return null;
    elsif (No > S.Count) then
      return null;
    else
      V := S.Variable_List;
      while (V /= null) loop
        Count := Count + 1;
        if (No = Count) then
          return V;
        end if;
        V := V.Next;
      end loop;
    end if;
    return null;
  end Find_Variable;


  function Find_Variable (S   : Section_Pointer_Type;
                          Name: String) return Variable_Pointer_Type is
    V : Variable_Pointer_Type;
  begin
    if (S = null) then
      return null;
    else
      V := S.Variable_List;
      while (V /= null) loop
        if (IS_Match(Name,V.Name.all)) then
          return V;
        end if;
        V := V.Next;
      end loop;
    end if;
    return null;
  end Find_Variable;


  procedure Define_Section (S: in out Section_Pointer_Type; Name: String) is
    P: Section_Pointer_Type;
  begin
    if (Find_Section(Name) = null) then
      S := new Section_Type'(new String'(Name), 0, null, null);
      if (Data.Section_List = null) then
        Data.Section_List := S;
      else
        P := Data.Section_List;
        while (P.Next /= null) loop
          P := P.Next;
        end loop;
        P.Next := S;
      end if;
      Data.Count := Data.Count + 1;
    end if;
  end Define_Section;


  procedure Define_Variable (S       : in out Section_Pointer_Type;
                             Variable: String;
                             Value   : String := "") is
    pragma Warnings(Off,S); -- 9.8-17902
    V: Variable_Pointer_Type := Find_Variable(S,Variable);
    P: Variable_Pointer_Type;
  begin
    if (V = null) then
      if (S /= null) then
        V := new Variable_Type'(new String'(Variable),
                                new String'(Value),
                                null);
        if (S.Variable_List = null) then
          S.Variable_List := V;
        else
          P := S.Variable_List;
          while (P.Next /= null) loop
            P := P.Next;
          end loop;
          P.Next := V;
        end if;
        S.Count := S.Count + 1;
      end if;
    else
      Free(V.Value);
      V.Value := new String'(Value);
    end if;
  end Define_Variable;


  procedure Parse (Current_Section: in out Section_Pointer_Type;
                   S              :        String) is
  begin
    for I in S'Range loop
      if (S(I) = '=') then
        if (I = S'Last) then
          Define_Variable (Current_Section, S(S'First..I-1));
          return;
        else
          Define_Variable (Current_Section, S(S'First..I-1), S(I+1..S'Last));
          return;
        end if;
      elsif (S(I) = '[') then
        if (I < S'Last) then
          Define_Section (Current_Section, S(I+1..S'Last-1));
          return;
        end if;
      end if;
    end loop;
  end Parse;

  -- V9.4-6643 Functionality moved to LOAD(FILE_NAME)
--  procedure Load is    -- 9.8-17902 rewritten
--    Base_Name : String := Ada.Directories.Base_Name(Ada.Command_Line.Command_Name);
--    Target_Path : String := Ada.Directories.Containing_Directory(Ada.Command_Line.Command_Name);
--    Ini_Name : String := Base_Name & ".ini";
--    Target_Ini_Name : String := System_Services.Expand_File_Path(Target_Path & "/" & Ini_Name);
--    Config_Ini_Name : String := System_Services.Expand_File_Path("$SATTMATE_CONFIG/processes/" & Ini_Name);
--  begin
----    Text_Io.Put_Line("Target_Ini_Name: '" & Target_Ini_Name & "'");
----    Text_Io.Put_Line("Config_Ini_Name: '" & Config_Ini_Name & "'");
--    if Ada.Directories.Exists(Target_Ini_Name) then -- first look where exe is
--      Load(Target_Ini_Name);
--    elsif Ada.Directories.Exists(Config_Ini_Name) then -- then look in config dir
--      Load(Config_Ini_Name);
--    else
--      raise No_Ini_File_Found with "Ini-file not found: '" & Target_Ini_Name & "'";
--    end if;
--  end Load;

  -- V9.4-6643 New procedure   -- 9.8-17902 rewritten
  procedure Load (File_Name : in String) is
    File           : Text_Io.File_Type with Warnings => Off;
    Line           : String(1..256);
    Last           : Integer;
    Current_Section: Section_Pointer_Type := null;
    Comment        : Boolean := True;
--    Washed_File_Name : Unbounded_String := Null_Unbounded_String;
--    procedure Convert_Ini_File_To_Unix_Format(Name     : in  String;
--                                              New_Name : out Unbounded_String) is
--      type Byte is range 0..255;
--      for Byte'Size use 8;
--      package My_Byte_IO is new Sequential_IO(Byte);
--      The_Byte : Byte;
--      Input_File  : My_Byte_IO.File_Type;
--      Output_File : My_Byte_IO.File_Type;
--      use System_Services;
--    begin
--      case Operating_System is
--        when Unix =>
--          My_Byte_Io.Open (File => Input_File,
--                           Mode => My_Byte_Io.In_File,
--                           Name => Name);
--          New_Name := To_Unbounded_String(Utils.Skip_All_Blanks(Name & "." & Integer'Image(Integer(Pid_Of_This_Process))));
--          My_Byte_Io.Create(File => Output_File,
--                            Mode => My_Byte_Io.Out_File,
--                            Name => To_String(New_Name));
--          begin
--            loop
--              My_Byte_Io.Read(File => Input_File, Item => The_Byte);
--              case The_Byte is
--                when 16#0D# => null; -- disregard Carriage Return
--                when others => My_Byte_Io.Write(File => Output_File, Item => The_Byte);
--              end case;
--            end loop;
--          exception
--            when My_Byte_Io.End_Error => null;
--          end;
--          My_Byte_Io.Close(File => Input_File);
--          My_Byte_Io.Close(File => Output_File);
--        when Win32 | Vax_Vms =>
--          New_Name := To_Unbounded_String(Name);
--      end case;
--    end Convert_Ini_File_To_Unix_Format;

  begin
    Current_Ini_File := To_Unbounded_String(File_Name); --bnl 10.2
--    Text_Io.Put_Line("Current_Ini_File: '" & To_String(Current_Ini_File) & "'");
--    Convert_Ini_File_To_Unix_Format(File_Name,Washed_File_Name);
--    Text_Io.Put_Line("new name '" & To_String(Washed_File_Name) & "'");
--    Text_Io.Put_Line("old name '" & File_Name & "'");
--    Text_Io.Open (File, Text_Io.In_File, To_String(Washed_File_Name));
    Text_Io.Open (File, Text_Io.In_File, To_String(Current_Ini_File));
    begin
      loop
        Text_Io.Get_Line (File, Line, Last);
--        Text_Io.Put_Line("'" & Line(1..Last) & "' " & Last'Img );
        Comment := False;
        if Last >= 1 then
          if Line(1) = '#' then
            Comment := True;
          elsif Line(1) = ';' then
            Comment := True;
          end if;
        end if;
        if not Comment then
          Parse (Current_Section, Collapse(Line(1..Last)));
        end if;
      end loop;
    exception
      when Text_Io.End_Error => null;
    end;
    Text_Io.Close(File);  -- V8.2a
     Global_Is_Loaded := True;
--    if To_String(Washed_File_Name) /= File_Name then
--      Ada.Directories.Delete_File(To_String(Washed_File_Name));
--    end if;
  end Load;

  function Is_Loaded return Boolean is
  begin
    return Global_Is_Loaded;
  end Is_Loaded;

  procedure Save is
    S   : Section_Pointer_Type := Data.Section_List;
    V   : Variable_Pointer_Type;
    File: Text_Io.File_Type with Warnings => Off;
  begin
--    Text_Io.Put_Line ("saved to file : '" & To_String(Current_Ini_File) & "'");
    Text_Io.Create (File, Text_Io.Out_File, To_String(Current_Ini_File)); -- 9.8-17902
    while (S /= null) loop
      Text_Io.Put_Line (File, "[" & S.Name.all & "]");
      V := S.Variable_List;
      while (V /= null) loop
        Text_Io.Put_Line (File, V.Name.all & "=" & V.Value.all);
        V := V.Next;
      end loop;
      Text_Io.New_Line(File);
      S := S.Next;
    end loop;
    Text_Io.Close(File);
  end Save;


  function Get_Section_Count return Natural is
  begin
    return Data.Count;
  end Get_Section_Count;


  function Get_Section_Name (No: Positive) return String is
    S: Section_Pointer_Type := Find_Section(No);
  begin
    if (S = null) then
      return "";
    else
      return S.Name.all;
    end if;
  end Get_Section_Name;


  function Get_Variable_Count (Section: String) return Natural is
    S: Section_Pointer_Type := Find_Section(Section);
  begin
    if (S = null) then
      return 0;
    else
      return S.Count;
    end if;
  end Get_Variable_Count;


  function Get_Variable_Name (Section: String; No: Positive) return String is
    S: Section_Pointer_Type  := Find_Section(Section);
    V: Variable_Pointer_Type := Find_Variable (S, No);
  begin
    if (V = null) then
      return "";
    else
      return V.Name.all;
    end if;
  end Get_Variable_Name;


  function Get_Value (Section : String;
                      Variable: String;
                      Default : String) return String is
    S: Section_Pointer_Type  := Find_Section (Section);
    V: Variable_Pointer_Type := Find_Variable (S, Variable);
  begin
    if (V = null) then
      return Default;
    else
      return V.Value.all;
    end if;
  end Get_Value;


  function Get_Value (Section : String;
                      Variable: String;
                      Default : Boolean) return Boolean is
    S: Section_Pointer_Type  := Find_Section (Section);
    V: Variable_Pointer_Type := Find_Variable (S, Variable);
  begin
    if (V = null) then
      return Default;
    else
      return Boolean'Value(V.Value.all);
    end if;
  exception
    when others => return Default;
  end Get_Value;


  function Get_Value (Section : String;
                      Variable: String;
                      Default : Integer) return Integer is
    S: Section_Pointer_Type  := Find_Section (Section);
    V: Variable_Pointer_Type := Find_Variable (S, Variable);
  begin
    if (V = null) then
      return Default;
    else
      return Integer'Value(V.Value.all);
    end if;
  exception
    when others => return Default;
  end Get_Value;


  function Get_Enumeration_Value
             (Section : String;
              Variable: String;
              Default : Enumeration_Type) return Enumeration_Type is
    S: Section_Pointer_Type  := Find_Section (Section);
    V: Variable_Pointer_Type := Find_Variable (S, Variable);
  begin
    if (V = null) then
      return Default;
    else
      for I in Enumeration_Type loop
        if (Is_Match(V.Value.all,Enumeration_Type'Image(I))) then
          return I;
        end if;
      end loop;
      return Default;
    end if;
  exception
    when others => return Default;
  end Get_Enumeration_Value;


  procedure Set_Value (Section: String; Variable: String; Value: String) is
    S: Section_Pointer_Type := Find_Section (Section) with Warnings => Off;
  begin
    if (S = null) then
      Define_Section (S, Section);
    end if;
    Define_Variable (S, Variable, Value);
  end Set_Value;


  procedure Set_Value (Section: String; Variable: String; Value: Boolean) is
    S: Section_Pointer_Type := Find_Section (Section) with Warnings => Off;
  begin
    if (S = null) then
      Define_Section (S, Section);
    end if;
    Define_Variable (S, Variable, Boolean'Image(Value));
  end Set_Value;


  procedure Set_Value (Section: String; Variable: String; Value: Integer) is
    S: Section_Pointer_Type := Find_Section (Section) with Warnings => Off;
  begin
    if (S = null) then
      Define_Section (S, Section);
    end if;
    Define_Variable (S, Variable, Utils.Skip_All_Blanks(Integer'Image(Value)));
  end SET_VALUE;


  procedure Set_Enumeration_Value (Section : String;
                                   Variable: String;
                                   Value   : Enumeration_Type) is
    S: Section_Pointer_Type := Find_Section (Section) with Warnings => Off;
  begin
    if (S = null) then
      Define_Section (S, Section);
    end if;
    Define_Variable (S, Variable, Enumeration_Type'Image(Value));
  end Set_Enumeration_Value;

end Ini;
