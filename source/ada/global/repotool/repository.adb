
with Ada;
with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Io_Exceptions;
with Gnat.String_Split;
with Ada.Characters.Latin_1 ;

with Unicode.Encodings;

with Sax;
with Sax.Readers;        use Sax.Readers;
with Sax.Attributes;
with Input_Sources.File; use Input_Sources.File;
with Schema;
with Schema.Readers;
with Schema.Validators;
with Schema.Schema_Readers;


with Utils;
with Stacktrace;
--with Types;     use Types;
with Text_Io;
with Repository.Table;
with Repository.Column;
with Repository.View;
with Sql;


package body Repository is

  Do_Feedback        : Boolean := True;
  Global_Debug_Level : Integer_4 := 0;

  Tag_Tables      : constant String := "tables";
  Tag_Views       : constant String := "views";
  Tag_Clreqs      : constant String := "clreqs";
  Tag_Terms       : constant String := "terms";
  Tag_Codes       : constant String := "codes";
  Tag_Labels      : constant String := "labels";
  Tag_Directory   : constant String := "directory";
  Tag_Pattern     : constant String := "pattern";
  Tag_Prefix      : constant String := "prefix";



---------------------------------------------------------------

  Empty_Item_Type : constant Item_Type := (Directory      => Empty_String_Object,
                                           Pattern        => Empty_String_Object,
                                           Prefix         => Empty_String_Object,
                                           Is_Initialized => True);

  type Reader is new Sax.Readers.Reader with record
    Config : Config_Type;
--    OK               : Boolean := True;
--    Action           : Action_Type;
    Current_Tag      : Unbounded_String := Null_Unbounded_String;
    State            : Config_Type_Type := Config_Type_Type'First;
    State_Is_Valid   : Boolean          := False; -- until we get a wanted tag
  end record;

  overriding procedure Start_Element(Handler       : in out Reader;
                                     Namespace_URI : Unicode.CES.Byte_Sequence := "";
                                     Local_Name    : Unicode.CES.Byte_Sequence := "";
                                     Qname         : Unicode.CES.Byte_Sequence := "";
                                     Atts          : Sax.Attributes.Attributes'Class) ;

  overriding procedure End_Element(Handler         : in out Reader;
                                   Namespace_URI   : Unicode.CES.Byte_Sequence := "";
                                   Local_Name      : Unicode.CES.Byte_Sequence := "";
                                   Qname           : Unicode.CES.Byte_Sequence := "") ;

  overriding procedure Characters(Handler          : in out Reader;
                                  Ch               : Unicode.CES.Byte_Sequence := "");


  -- reads sattmate.xml and returns paths

  procedure Create (Self : in out Config_Type) is
    My_Reader    : Reader;
    Input        : File_Input;
    package EV renames Ada.Environment_Variables;
  begin
    if EV.Exists(Name => "BOT_CONFIG") then
      declare
        Filename : String := EV.Value(Name => "BOT_CONFIG") & "/repository/betbot.xml";
      begin
        --open sattmate.xml
        Open(Filename, Input);
        My_Reader.Set_Feature(Validation_Feature,False);
        My_Reader.Parse(Input);
        Close(Input);
      end;
    else
      raise Configuration_Error with "BOT_CONFIG is NOT defined!";
    end if;
    Self := My_Reader.Config;
    Self.Is_Initialized := True;
  end Create;

  -------------------------------------------------------------------------------------

  overriding function To_String(Self : Config_Type) return String is
    Tmp : Unbounded_String := Null_Unbounded_String;
  begin
    if Self.Is_Initialized then
      for i in Config_Type_Type'range loop
        Append(Tmp, i'Img & " -> ");
        Append(Tmp, Self.Item(i).To_String);
        Append(Tmp, Ascii.Lf);
      end loop;
      return To_String(Tmp);
    else
      raise Sequence_Error with "Object not initilized. call Create.";
    end if;
  end To_String;
  -------------------------------------------------------------------------------------

  overriding function To_String(Self : Item_Type) return String is
  begin
    return "Directory:" & Self.Directory.Fix_String &
           " Pattern:"  & Self.Pattern.Fix_String &
           " Prefix:"   & Self.Prefix.Fix_String;
  end To_String;

  -------------------------------------------------------------------------------------
  overriding procedure Start_Element(Handler       : in out Reader;
                                     Namespace_Uri : Unicode.Ces.Byte_Sequence := "";
                                     Local_Name    : Unicode.Ces.Byte_Sequence := "";
                                     Qname         : Unicode.Ces.Byte_Sequence := "";
                                     Atts          : Sax.Attributes.Attributes'Class) is
    pragma Unreferenced(Namespace_Uri);
    pragma Unreferenced(Qname);
    pragma Unreferenced(Atts);
    The_Tag : constant String := Local_Name;
  begin
    Handler.Current_Tag := To_Unbounded_String(The_Tag);
    -- The subsystem
--    Feedback("Start_Element " & The_Tag );

    if The_Tag = Tag_Tables then
      Handler.State := Tables;
      Handler.State_Is_Valid := True;
      Handler.Config.Item(Handler.State) := Empty_Item_Type;

    elsif The_Tag = Tag_Views then
      Handler.State := Views;
      Handler.State_Is_Valid := True;
      Handler.Config.Item(Handler.State) := Empty_Item_Type;

    elsif The_Tag = Tag_Clreqs then
      Handler.State := Clreqs;
      Handler.State_Is_Valid := True;
      Handler.Config.Item(Handler.State) := Empty_Item_Type;

    elsif The_Tag = Tag_Terms then
      Handler.State := Terms;
      Handler.State_Is_Valid := True;
      Handler.Config.Item(Handler.State) := Empty_Item_Type;

    elsif The_Tag = Tag_Codes then
      Handler.State := Codes;
      Handler.State_Is_Valid := True;
      Handler.Config.Item(Handler.State) := Empty_Item_Type;

    elsif The_Tag = Tag_Labels then
      Handler.State := Labels;
      Handler.State_Is_Valid := True;
      Handler.Config.Item(Handler.State) := Empty_Item_Type;

    end if;
  exception
    when Ada.Strings.Length_Error =>
        Feedback("Ada.Strings.Length_Error on Tag '" & The_Tag & "'");
  end Start_Element;
  --++--++--++--++--++--++--++--++--++--++--++--++--++--
  overriding procedure End_Element(Handler       : in out Reader;
                                   Namespace_Uri : Unicode.Ces.Byte_Sequence := "";
                                   Local_Name    : Unicode.Ces.Byte_Sequence := "";
                                   Qname         : Unicode.Ces.Byte_Sequence := "") is
    pragma Unreferenced(Namespace_Uri);
    pragma Unreferenced(Qname);
    pragma Unreferenced(Handler);
    The_Tag : constant String := Local_Name;
  begin
    if The_Tag = Tag_Tables then
      Handler.State_Is_Valid := False;

    elsif The_Tag = Tag_Views then
      Handler.State_Is_Valid := False;

    elsif The_Tag = Tag_Clreqs then
      Handler.State_Is_Valid := False;

    elsif The_Tag = Tag_Terms then
      Handler.State_Is_Valid := False;

    elsif The_Tag = Tag_Codes then
      Handler.State_Is_Valid := False;

    elsif The_Tag = Tag_Labels then
      Handler.State_Is_Valid := False;

    end if;
  exception
    when Ada.Strings.Length_Error =>
        Feedback("Ada.Strings.Length_Error on Tag '" & The_Tag & "'");
  end End_Element;
  --++--++--++--++--++--++--++--++--++--++--++--++--++--

  overriding procedure Characters(Handler          : in out Reader;
                                  Ch               : Unicode.Ces.Byte_Sequence := "") is
    The_Tag   : constant String := To_String(Handler.Current_Tag);
    The_Value : constant String := Utils.Expand_File_Path(To_Iso_Latin_15(Ch));
  begin
--    Feedback("Characters " & The_Tag & " -> '" & The_Value & "'");

    if Handler.State_Is_Valid then
      if The_Tag = Tag_Directory then
        Handler.Config.Item(Handler.State).Directory.Append(The_Value);
      elsif The_Tag = Tag_Pattern then
        Handler.Config.Item(Handler.State).Pattern.Append(The_Value);
      elsif The_Tag = Tag_Prefix then
        Handler.Config.Item(Handler.State).Prefix.Append(The_Value);
      end if;
    end if;

  exception
    when Ada.Strings.Length_Error =>
        Feedback("Ada.Strings.Length_Error on Tag '" & The_Tag & "' with data '" & The_Value & "'");
  end Characters;

  ----------------------------------------------------------------------

  function To_Language(L : String) return Language_Type is
  begin
    return Language_Type'Value(L);
  exception
    when Constraint_Error =>
      raise Configuration_Error with "Bad Language '" & L & "'";
  end To_Language;
  -------------------------------------------------------------------------

  function Validate(Xml, Xsd : in String) return Boolean is
    use Schema;
    use Schema.Readers;
    use Schema.Validators;
    use Schema.Schema_Readers;
    use Ada.Exceptions;
    Read      : File_Input;
    Schema    : Schema_Reader;
    Grammar   : XML_Grammar := No_Grammar;
    My_Reader : Validating_Reader;
  begin
    if Xsd'length > 0 then
      if Ada.Directories.Exists(Xsd) then
        begin
          Open (Xsd, Read);
          Parse (Schema, Read);
          Close (Read);
          Grammar := Get_Grammar(Schema);
        exception
          when XML_Validation_Error =>
          Feedback("Error reading xsd: " & Xsd & "'");
          Feedback("Exception XML_Validation_Error: " & Get_Error_Message(Schema));
          Close (Read);
          raise;
        end;
      else
        Feedback("File '" & Xsd & "' does not exist");
        return False;
      end if;
    end if;

    if Ada.Directories.Exists(Xml) then
      Set_Grammar(My_Reader, Grammar);
      Set_Feature(My_Reader, Schema_Validation_Feature, True);
      Open(Xml, Read);
      Parse(My_Reader, Read);
      Close(Read);
      -- Feedback("File '" & Xml & "' is valid");
      return True;
    else
      Feedback("File '" & Xml & "' does not exist");
      return False;
    end if;
  exception
    when E : XML_Validation_Error | XML_Fatal_Error =>
        Feedback("Exception " & Exception_Name (E) & ": " & Get_Error_Message(My_Reader));
        Feedback("xsd: '" & Xsd & "'");
        Feedback("xml: '" & Xml & "' is NOT valid");
        Close(Read);
        return False;
    when Ada.Io_Exceptions.Name_Error =>
        Feedback("File '" & Xml & "' is not a valid filename");
        return False;
    when E: others =>
      Stacktrace.Tracebackinfo(E);
        return False;
  end Validate;
  ----------------------------------------
  procedure Debug(S : String) is
    use Text_Io;
  begin
    if Global_Debug_Level > 0 then
      Put_Line(Standard_Error, S);
    end if;
  end Debug;

  procedure Code_Debug(S : String) is
    use Text_Io;
  begin
    if Global_Debug_Level > 0 then
      Put_Line(Standard_Output, S);
    end if;
  end Code_Debug;

  ----------------------------------------
  function To_SQL_Type(Data : Data_Type_Type ; Database : Database_Type_Type) return String is
  begin
    case Database is
      when Oracle =>
        case Data is
          when A_Char        => return "VARCHAR2";
          when A_Int         => return "NUMBER(9)";
          when A_Double      => return "NUMBER";
          when A_Boolean     => return "NUMBER(9)";
          when A_Short_Code  => return "NUMBER(9)";
          when A_Date        => return "DATE";
          when A_Time        => return "DATE";
          when A_Timestamp   => return "TIMESTAMP(3)";
          when A_Clob        => return "CLOB";
          when A_Nclob       => return "NCLOB";
          when A_Blob        => return "BLOB";
          when others        => raise Configuration_Error with "Datatype " & Data'Img & " is not supported";
        end case;

      when Postgresql =>
        case Data is
          when A_Char        => return "varchar";
          when A_Int         => return "integer";
          when A_Big_Int     => return "bigint";
          when A_Double      => return "numeric(15,2)"; --"float";
          when A_Boolean     => return "boolean";
          when A_Short_Code  => return "integer";
          when A_Date        => return "date";
          when A_Time        => return "time without time zone";
          when A_Timestamp   => return "timestamp(3) without time zone";
          when A_Clob        => return "varchar";
          when A_Nclob       => return "varchar";
          when A_Blob        => return "bytea";
          when others        => raise Configuration_Error with "Datatype " & Data'Img & " is not supported";
        end case;

      when Sql_Server =>
        case Data is
          when A_Char        => return "varchar";
          when A_Int         => return "integer";
          when A_Double      => return "float";
          when A_Boolean     => return "integer";
          when A_Short_Code  => return "integer";
          when A_Date        => return "datetime2(3)";
          when A_Time        => return "datetime2(3)";
          when A_Timestamp   => return "datetime2(3)";
          when A_Clob        => return "text";
          when A_Nclob       => return "ntext";
          when A_Blob        => return "blob";
          when others        => raise Configuration_Error with "Unsupported data type: " & Data'Img;
        end case;
    end case;
  end To_SQL_Type;
  ---------------------------------------------------------------------
  function To_Ada_Type(Data   : Data_Type_Type ; Size_Of: Size_Type) return String is
  begin
    case Data is
      when A_Char        =>
        if Size_Of = 1 then
          return "Character";
        else
          return "String";
        end if;
      when A_Int         => return "Integer_4";
      when A_Big_Int     => return "Integer_8";
      when A_Double      => return "Fixed_Type";
      when A_Boolean     => return "Boolean";
      when A_Short_Code  => return "Integer_4";
      when A_Date        => return "Time_Type";
      when A_Time        => return "Time_Type";
      when A_Timestamp   => return "Time_Type";
      when A_Clob        => return "Clob";
      when A_Nclob       => return "Nclob";
      when A_Blob        => return "Blob";
      when others        => raise Configuration_Error with "Unsupported data type: " & Data'Img;
    end case;
  end To_Ada_Type;
  ---------------------------------------------------------------------


  function Default_Value(Data : Data_Type_Type ; Database: Database_Type_Type) return String is
  begin
    case Database is
      when Oracle | Postgresql | Sql_Server =>
        case Data is
          when A_Char        => return "default ' '";
          when A_Double      => return "default 0.0";
          when A_Int     |
               A_Big_Int |
               A_Long    |
               A_Short_Code  => return "default 1";
          when A_Boolean     => return "default False";
          when others        => return "";
        end case;
    end case;
  end Default_Value;
  ---------------------------------------------------------------------------------------------
  function Null_Value(Data : Data_Type_Type ; Size_Of: Size_Type) return String is
  begin
    case Data is
      when A_Char        =>
        if Size_Of = 1 then
          return "' '";
        else
          return "(others => ' ')";
        end if;
      when A_Double      => return "0.0";
      when A_Int     |
           A_Big_Int |
           A_Long    |
           A_Short_Code  => return "0";
      when A_Boolean => return "False"; --  -- boolean is not number in db 0/1

      when A_Time    |
           A_Date    |
           A_Timestamp   => return "Time_Type_First";
      when A_Clob        => return "Null_Unbounded_String";
      when A_Nclob   |
           A_Blob        => return "(others => ' ')";
      when others        => raise Configuration_Error with "Unsupported data type: " & Data'Img;
    end case;
  end Null_Value;
  ---------------------------------------------------------------------------------------------
  function Null_Value_For_Type_At_Comparison( Data : Data_Type_Type ; Size_Of: Size_Type; Var_Name : String) return String is
  begin
    case Data is
      when A_Char        =>
        if Size_Of = 1 then
          return "' '";
        else
          return "(" & Var_Name & "'range => ' ')";
        end if;

      when A_Clob  |
           A_Nclob |
           A_Blob  => return "Null_Unbounded_String";

      when others=> return Null_Value (Data,Size_Of);
    end case;
  end Null_Value_For_Type_At_Comparison;


  ---------------------------------------------------------------------------------------------
  procedure Print_DDL_Create_Table_For_All(Self : in out Config_Type ; Database : Database_Type_Type) is
    All_Tables : String_Object := String_Object(Self.All_Entities_Defined_Names(Tables));
    use Gnat;
    Subs : String_Split.Slice_Set;
    Seps : constant String := " ";
    Tbl  : Repository.Table.Table_Type ;
  begin
    String_Split.Create (S          => Subs,
                         From       => All_Tables.Fix_String,
                         Separators => Seps,
                         Mode       => String_Split.Multiple);

    -- for each table name in All_Tables loop
    for j in 1 .. String_Split.Slice_Count(Subs) loop
      declare
        Table_Name : String  := String_Split.Slice(Subs, j);
      begin
        Tbl.Create(Table_Name, Self);
        case Database is
          when Oracle     => Tbl.Print_Oracle_Create_DDL;
          when SQl_Server => Tbl.Print_Sql_Server_Create_DDL;
          when Postgresql => Tbl.Print_Postgresql_Create_DDL;
        end case;
        Tbl.Reset;
      end;
    end loop;
  end Print_DDL_Create_Table_For_All;
  ---------------------------------------------------------------------------------------------
  procedure Print_DDL_Drop_Table_For_All(Self : in out Config_Type ; Database : Database_Type_Type) is
    All_Tables : String_Object := String_Object(Self.All_Entities_Defined_Names(Tables));
    use Gnat;
    Subs : String_Split.Slice_Set;
    Seps : constant String := " ";
    Tbl  : Repository.Table.Table_Type ;
  begin
    String_Split.Create (S          => Subs,
                         From       => All_Tables.Fix_String,
                         Separators => Seps,
                         Mode       => String_Split.Multiple);

    -- for each table name in All_Tables loop
    for j in 1 .. String_Split.Slice_Count(Subs) loop
      declare
        Table_Name : String  := String_Split.Slice(Subs, j);
      begin
        Tbl.Create(Table_Name, Self);
        case Database is
          when Oracle     => Tbl.Print_Oracle_Drop_DDL;
          when SQl_Server => Tbl.Print_Sql_Server_Drop_DDL;
          when Postgresql => Tbl.Print_Postgresql_Drop_DDL;
        end case;
        Tbl.Reset;
      end;
    end loop;
  end Print_DDL_Drop_Table_For_All;

  ---------------------------------------------------------------------------------------------
  procedure Print_DDL_Create_View_For_All(Self : in out Config_Type ; Database : Database_Type_Type) is
    All_Views : String_Object := String_Object(Self.All_Entities_Defined_Names(Views));
    use Gnat;
    Subs : String_Split.Slice_Set;
    Seps : constant String := " ";
    Vw   : Repository.View.View_Type ;
  begin
    String_Split.Create (S          => Subs,
                         From       => All_Views.Fix_String,
                         Separators => Seps,
                         Mode       => String_Split.Multiple);

    -- for each table name in All_Tables loop
    for j in 1 .. String_Split.Slice_Count(Subs) loop
      declare
        View_Name : String  := String_Split.Slice(Subs, j);
      begin
        Vw.Create(View_Name, Self);
        case Database is
          when Oracle     => Vw.Print_Oracle_Create_DDL;
          when SQl_Server => Vw.Print_Sql_Server_Create_DDL;
          when Postgresql => Vw.Print_Postgresql_Create_DDL;
        end case;
        Vw.Reset;
      end;
    end loop;
  end Print_DDL_Create_View_For_All;
  ---------------------------------------------------------------------------------------------
  procedure Print_DDL_Drop_View_For_All(Self : in out Config_Type ; Database : Database_Type_Type) is
    All_Views : String_Object := String_Object(Self.All_Entities_Defined_Names(Views));
    use Gnat;
    Subs : String_Split.Slice_Set;
    Seps : constant String := " ";
    Vw   : Repository.View.View_Type ;
  begin
    String_Split.Create (S          => Subs,
                         From       => All_Views.Fix_String,
                         Separators => Seps,
                         Mode       => String_Split.Multiple);

    -- for each table name in All_Views loop
    for j in 1 .. String_Split.Slice_Count(Subs) loop
      declare
        View_Name : String  := String_Split.Slice(Subs, j);
      begin
        Vw.Create(View_Name, Self);
        case Database is
          when Oracle     => Vw.Print_Oracle_Drop_DDL;
          when SQl_Server => Vw.Print_Sql_Server_Drop_DDL;
          when Postgresql => Vw.Print_Postgresql_Drop_DDL;
        end case;
        Vw.Reset;
      end;
    end loop;
  end Print_DDL_Drop_View_For_All;
  ---------------------------------------------------------------------------------------------

  function All_Entities_Defined_Full_Path(Self : in out Config_Type; Entity : Config_Type_Type )  return String_Object'class is
    Tmp   : String_Object;
    Fname : String_Object;

    package Directory_List_Package is new Ada.Containers.Doubly_Linked_Lists(String_Object);
    package A_Sorter is new Directory_List_Package.Generic_Sorting;
    File_List : Directory_List_Package.List;

    use Ada.Directories;
    Dir_Ent : Directory_Entry_Type;
    Search  : Search_Type;
  begin

    Start_Search(Search    => Search,
                 Directory => Self.Item(Entity).Directory.Fix_String,
                 Pattern   => Self.Item(Entity).Pattern.Fix_String);
    loop
      exit when not More_Entries(Search => Search);
        Get_Next_Entry(Search          => Search,
                       Directory_Entry => Dir_Ent);
        Fname.Set(Full_Name(Dir_Ent));
        File_List.Append(Fname);
    end loop;
    End_Search (Search => Search);

    A_Sorter.Sort(File_List);

    for F of File_List loop
      Tmp.Append(F.Fix_String & " ");
    end loop;
    if Tmp.Fix_String /= "" then
      Tmp.Delete_Last_Char;
    end if;
    return Tmp;
  end All_Entities_Defined_Full_Path;
  ---------------------------------------------------------------------------------------------

  function All_Entities_Defined_Names(Self : in out Config_Type; Entity : Config_Type_Type) return String_Object'class is
    Tmp   : String_Object;
    All_Entities : String_Object := String_Object(Self.All_Entities_Defined_Full_Path(Entity));
    use Gnat;
    Subs : String_Split.Slice_Set;
    Seps : constant String := " ";
    package AD renames Ada.Directories;
  begin
    if All_Entities.Fix_String /= "" then
      String_Split.Create (S          => Subs,
                           From       => All_Entities.Fix_String,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each field in idx.columns loop
      for j in 1 .. String_Split.Slice_Count(Subs) loop
        declare
          Full_File_Name : String  := String_Split.Slice(Subs, j);
          File_Name      : String  := Ad.Base_Name(Full_File_Name); --no path nor extension left
          Prefix         : String  := Self.Item(Entity).Prefix.Fix_String;
          Len            : Integer := Prefix'Length;
          Entity_Name    : String(1 .. File_Name'Last - Len -1 ) := File_Name(Len +2.. File_Name'Last);
        begin
          Tmp.Append(Entity_Name & " ");
        end;
      end loop;
    end if;
    return Tmp;
  end All_Entities_Defined_Names;

  ---------------------------------------------------------------------------------------------

  ---------------------------------------------------------------------------------------------

  function Header return String_Object is
    Tmp : String_Object;
  begin
    Tmp.Append("-----------------------------------------------------" & Ascii.Lf);
    Tmp.Append("-- This file is AUTOGENERATED by                     " & Ascii.Lf);
    Tmp.Append("-- repo                                              " & Ascii.Lf);
    Tmp.Append("--                                                   " & Ascii.Lf);
    Tmp.Append("----CHANGES HERE WILL BE LOST NEXT GENERATE!!!!----- " & Ascii.Lf);
    Tmp.Append("-----------DO NOT EDIT THIS FILE!!!!---------------- " & Ascii.Lf);
    Tmp.Append("-----------------------------------------------------" & Ascii.Lf & Ascii.Lf);
    return Tmp;
  end Header;
  -------------------------------------------------------------------------------------------------
  function To_Iso_Latin_15(Str : Unicode.CES.Byte_Sequence) return String is
    use Unicode.Encodings;
  begin
    return  Convert(Str  => Str,
                    From => Get_By_Name("utf-8"),
                    To   => Get_By_Name("iso-8859-15"));

  end To_Iso_Latin_15;
  -----------------------------------------------------

  procedure Feedback(What : in String) is
  begin
    if Do_Feedback then
      Text_io.Put_Line(Text_io.Standard_Error, What);
    end if;
  end Feedback;
  -------------------------------------
  procedure Set_Silent is
  begin
    Do_Feedback := False;
  end Set_Silent;
  -------------------------------------

  procedure Create_Table_Makefile(Self : in out Config_Type) is
    use Gnat; use Text_Io;
    use type String_Split.Slice_Number;
    Subs : String_Split.Slice_Set;
    Seps : constant String := " ";
    package AD renames Ada.Directories;

    All_Tables : String_Object := String_Object(Self.All_Entities_Defined_Names(Tables));
    All_Clreqs : String_Object := String_Object(Self.All_Entities_Defined_Names(Clreqs));

    Engine : String := Ada.Command_Line.Command_Name;
    Receipt_Prefix   : Character := Ada.Characters.Latin_1.HT; -- tab

    --------------------------------------------------------------------------------
    procedure Print_Entity(Entity_Type : Entity_Type_Type; Entities : String_Object) is
      Num_Slices : String_Split.Slice_Number;
      Entity     : String_Object;
    begin
      Entity.Set("table"); -- yes really  for both ud4 and table ...
      case Entity_Type is
        when Db  => Put_Line("tables: \");
        when Ud4 => Put_Line("clreqs: \");
      end case;
      if Entities.Fix_String = "" then
        return ;
      end if;
      String_Split.Create (S          => Subs,
                           From       => Entities.Fix_String,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);
      Num_Slices := String_Split.Slice_Count(Subs);
      for j in 1 .. Num_Slices loop
        declare
          Full_File_Name : String  := String_Split.Slice(Subs, j);
          File_Name      : String  := Ad.Simple_Name(Full_File_Name); --no path left
        begin
          if j < String_Split.Slice_Count(Subs) then
            Put_Line("    " & Entity.Lower_Case & "_" & File_Name & ".adb \");
          else
            Put_Line("    " & Entity.Lower_Case & "_" & File_Name & ".adb");
          end if;
        end;
      end loop;
    end Print_Entity;
    --------------------------------------------------------------------------------
    procedure Print_Targets(Entity_Type : Entity_Type_Type; Entities : String_Object) is
      Num_Slices       : String_Split.Slice_Number;
      DB_Type          : Sql.Database_Type := Sql.Database;
      Entity, Database : String_Object;
    begin
      if Entities.Fix_String = "" then
        return ;
      end if;
      Database.Set(DB_Type'Img);
      case Entity_Type is
        when Db  => Entity.Set("table");
        when Ud4 => Entity.Set("clreq");
      end case;

      String_Split.Create (S          => Subs,
                           From       => Entities.Fix_String,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);
      Num_Slices := String_Split.Slice_Count(Subs);
      for j in 1 .. Num_Slices loop
        declare
          Full_File_Name : String  := String_Split.Slice(Subs, j);
          File_Name      : String  := Ad.Simple_Name(Full_File_Name); --no path left
        begin
            Put_Line("");
            Put_Line("########## start " & File_Name & " ###################");
            Put_Line("table_" & File_Name & ".adb : $(REPO_TABLES_PATH)/" & Entity.Lower_Case & "_" & File_Name & ".xml \");

            case Entity_Type is
              when Db  =>
                declare
                  Tbl : Repository.Table.Table_Type;
                begin
                  Tbl.Create(File_Name,Self);
                  for Col of Tbl.Columns loop
                    Put_Line("                   $(REPO_TERMS_PATH)/trm_" & Col.Name.Lower_Case & ".xml \");
                  end loop;
                  Tbl.Reset;
                end;
                Put_Line("                   $(REPO_ENGINE)");
                Put_Line(Receipt_Prefix & " $(REPO_ENGINE) --"& Database.Lower_Case& "=" & File_Name & " > " & File_Name & ".sql");
                Put_Line(Receipt_Prefix & " $(REPO_ENGINE) --table=" & File_Name & " > table_" & File_Name & ".ada");
              when Ud4 =>
                declare
                  Tbl : Repository.Table.Table_Type;
                begin
                  Tbl.Create_Ud4(File_Name,Self);
                  for Col of Tbl.Columns loop
                    Put_Line("                   $(REPO_TERMS_PATH)/trm_" & Col.Name.Lower_Case & ".xml \");
                  end loop;
                  Tbl.Reset;
                end;
                Put_Line("                   $(REPO_ENGINE)");
                Put_Line(Receipt_Prefix & " $(REPO_ENGINE) --clreq=" & File_Name & " > table_" & File_Name & ".ada");
            end case;

            Put_Line(Receipt_Prefix & " gnatchop -gnat12 -w table_" & File_Name & ".ada");
            Put_Line(Receipt_Prefix & " rm -f table_" & File_Name & ".ada");
            Put_Line("########## stop " & File_Name & " ###################");
        end;
      end loop;
    end Print_Targets;
    --------------------------------------------------------------------------------
  begin
    Put_Line("# Autogenerated makefile for tables");
    Put_Line("# via " & Engine & " --table_makefile");
    Put_Line("");
    Put_Line("REPO_TABLES_PATH :=" & Self.Item(Tables).Directory.Fix_String);
    Put_Line("REPO_TERMS_PATH :=" & Self.Item(Terms).Directory.Fix_String);
    Put_Line("");
    Put_Line(".PHONY: all tables clreqs clean");
    Put_Line("");
    Put_Line("all: tables clreqs");
    Put_Line("");
    Print_Entity(Db, All_Tables);
    Put_Line("");
    Print_Entity(Ud4, All_Clreqs);
    Put_Line("");
    Print_Targets(Db, All_Tables);
    Put_Line("");
    Print_Targets(Ud4, All_Clreqs);
    Put_Line("");
    Put_Line("# delete all tables");
    Put_Line("clean:");
    Put_Line(Receipt_Prefix & " rm -f table_*.ad[bs]");
    Put_Line(Receipt_Prefix & " rm -f *.sql");
    Put_Line("");
  end Create_Table_Makefile;
  --------------------------------------------------------------------------------

  function List_Coded_Values(Self : in out Config_Type; Listing_Type : in Listing_Type_Type) return String_Object'class is
    Tmp   : String_Object;
    All_Entities : String_Object := String_Object(Self.All_Entities_Defined_Full_Path(Codes));
    use Gnat;
    Subs : String_Split.Slice_Set;
    Seps : constant String := " ";
    package AD renames Ada.Directories;
    Col : Repository.Column.Column_Type ;
  begin
    String_Split.Create (S          => Subs,
                         From       => All_Entities.Fix_String,
                         Separators => Seps,
                         Mode       => String_Split.Multiple);

    -- for each field in idx.columns loop
    for j in 1 .. String_Split.Slice_Count(Subs) loop
      declare
        Full_File_Name : String  := String_Split.Slice(Subs, j);
        File_Name      : String  := Ad.Base_Name(Full_File_Name); --no path nor extension left
        Prefix         : String  := Self.Item(Codes).Prefix.Fix_String;
        Len            : Integer := Prefix'Length;
        Col_Name       : String(1 .. File_Name'Last - Len -1 ) := File_Name(Len +2.. File_Name'Last);
      begin
        Col.Create(Col_Name, Self);
        if Data_Type(Col.Type_Of) = A_Short_Code then
          case Listing_Type is
            when Full => Tmp.Append( Full_File_Name & " ");
            when Name => Tmp.Append( Col_Name & " ");
          end case;
        end if;
        Col.Reset;
      end;
    end loop;

    return Tmp;
  end List_Coded_Values;

  -------------------------------------------------------------------------------------------

  -------------------------------------------------------------------------------------------
  procedure Make_Coded_Values(Self : in out Config_Type) is
    All_Values : String_Object := String_Object(Self.All_Entities_Defined_Full_Path(Codes));
    use Gnat; use Text_Io;
    Subs : String_Split.Slice_Set;
    Seps : constant String := " ";
    package AD renames Ada.Directories;
    Col : Repository.Column.Column_Type ;
    Column_List : Repository.Column.Columns_Type ;

    -----------------------------------------------------------------
    procedure Print_Header_Spec(Column : Repository.Column.Column_Type) is
      pragma Unreferenced(Column);
    begin
      Code_Debug("  -- start Print_Header_Spec ");
      Put_Line(Header.Fix_String);
      Put_Line("");
      Code_Debug("  -- stop  Print_Header_Spec ");
    end Print_Header_Spec;

    -----------------------------------------------------------------
    procedure Print_Withs_Spec(Column : Repository.Column.Column_Type) is
      pragma Unreferenced(Column);
    begin
      Code_Debug("  -- start Print_Withs_Spec ");
      Put_Line("with Sattmate_Types; use Sattmate_Types;");

      Code_Debug("  -- stop  Print_Withs_Spec ");
    end Print_Withs_Spec;
    -----------------------------------------------------------------
    procedure Print_Package_Start_Spec(Column : Repository.Column.Column_Type) is
      pragma Unreferenced(Column);
    begin
      Code_Debug("  -- start Print_Package_Start_Spec ");
      Put_Line("package Coded_Values is");
      Code_Debug("  -- stop  Print_Package_Start_Spec ");
    end Print_Package_Start_Spec;
    -----------------------------------------------------------------
    procedure Print_Private(Column : Repository.Column.Column_Type) is
      pragma Unreferenced(Column);
    begin
      Put_Line("");
      Code_Debug("  -- start Print_Private ");
      Put_Line("private");
      Put_Line("");
      Code_Debug("  -- stop  Print_Private ");
    end Print_Private;
    -----------------------------------------------------------------
    procedure Print_Functions_Spec(Column : Repository.Column.Column_Type) is
      Cnt : Integer_4 := 0;
      Num_In_List : Integer_4 := Integer_4(Column.Code_List.Length);
    begin
      Code_Debug("  -- start Print_Functions_Spec ");
      Put_Line("  --  <<---   " & Column.Name.Camel_Case & "  --- ");
      Put_Line("  type " & Column.Define.Camel_Case & "_Type is (");

      for CVI of Column.Code_List loop
        Cnt := Cnt +1;
        if Cnt < Num_In_List then
          Put_Line("    " & CVI.Define.Camel_Case & ",");
        else
          Put_Line("    " & CVI.Define.Camel_Case & ");");
        end if;
      end loop;
      Put_Line("");

      Put_Line("  for " & Column.Define.Camel_Case & "_Type'Size use Integer_4'Size;");
      Put_Line("  for " & Column.Define.Camel_Case & "_Type use (");
      Cnt := 0;
      for CVI of Column.Code_List loop
        Cnt := Cnt +1;
        if Cnt < Num_In_List then
          Put_Line("    " & CVI.Define.Camel_Case & " => " & CVI.Code'Img & ",");
        else
          Put_Line("    " & CVI.Define.Camel_Case & " => " & CVI.Code'Img &  ");");
        end if;
      end loop;
      Put_Line("");

      Put_Line("  function " & Column.Define.Camel_Case & "(X: " & Column.Define.Camel_Case & "_Type) return Integer_4;");
      Put_Line("  function " & Column.Define.Camel_Case & "(X: Integer_4) return " & Column.Define.Camel_Case & "_Type;");
      Put_Line("  --  ----- " & Column.Name.Camel_Case & " -->>");
      Put_Line("");

      Code_Debug("  -- stop  Print_Functions_Spec ");
    end Print_Functions_Spec;
    -----------------------------------------------------------------
    procedure Print_Private_Spec(Column : Repository.Column.Column_Type) is
    begin
      Code_Debug("  -- start Print_Private_Spec ");
      Put_Line("");

      Put_Line("  -- start " & Column.Name.Camel_Case & "");
      Put_Line("  function To_Integer_4_Checked(X: " & Column.Define.Camel_Case & "_Type) return Integer_4;");
      Put_Line("  function " & Column.Define.Camel_Case & " (X: " & Column.Define.Camel_Case & "_Type) return Integer_4 renames To_Integer_4_Checked;");
      Put_Line("  function From_Integer_4_Checked(X: Integer_4) return " & Column.Define.Camel_Case & "_Type;");
      Put_Line("  function " & Column.Define.Camel_Case & " (X: Integer_4) return " & Column.Define.Camel_Case & "_Type renames From_Integer_4_Checked;");
      Put_Line("  -- stop " & Column.Name.Camel_Case & "");
      Put_Line("");

      Code_Debug("  -- stop  Print_Private_Spec ");
    end Print_Private_Spec;
    -----------------------------------------------------------------
    procedure Print_Package_End_Spec(Column : Repository.Column.Column_Type) is
      pragma Unreferenced(Column);
    begin
      Code_Debug("  -- start Print_Package_End_Spec ");
      Put_Line("end Coded_Values;");
      Put_Line("");
      Code_Debug("  -- stop  Print_Package_End_Spec ");
    end Print_Package_End_Spec;

    -----------------------------------------------------------------
    procedure Print_Header_Body(Column : Repository.Column.Column_Type) is
      pragma Unreferenced(Column);
    begin
      Code_Debug("  -- start Print_Body.Print_Header_Body ");
      Put_Line(Header.Fix_String);
      Put_Line("");
      Code_Debug("  -- stop  Print_Body.Print_Header_Body ");
    end Print_Header_Body;

    -----------------------------------------------------------------
    procedure Print_Withs_Body(Column : Repository.Column.Column_Type) is
      pragma Unreferenced(Column);
    begin
      Code_Debug("  -- start Print_Withs_Body ");
      Put_Line("with Unchecked_Conversion;");
      Code_Debug("  -- stop  Print_Withs_Body ");
    end Print_Withs_Body;
    -----------------------------------------------------------------
    procedure Print_Package_Start_Body(Column : Repository.Column.Column_Type) is
      pragma Unreferenced(Column);
    begin
      Code_Debug("  -- start Print_Package_Start_Body ");
      Put_Line("package body Coded_Values is");
      Code_Debug("  -- stop  Print_Package_Start_Body ");
    end Print_Package_Start_Body;
    -----------------------------------------------------------------
    procedure Print_Functions_Body(Column : Repository.Column.Column_Type) is
    begin
      Code_Debug("  -- start Print_Functions_Body ");
      Put_Line("  --  <<---   " & Column.Name.Camel_Case & "  --- ");
      Put_Line("  function To_Integer_4_Checked(X: " & Column.Define.Camel_Case & "_Type) return Integer_4 is");
      Put_Line("    function To_Integer_4 is new Unchecked_Conversion(" & Column.Define.Camel_Case & "_Type, Integer_4);");
      Put_Line("    Return_Value : Integer_4 := To_Integer_4(X);");
      Put_Line("  begin");
      Put_Line("    if not Return_Value'Valid then");
      Put_Line("      raise Constraint_Error with " & Quote("Value out of range: ") & " & X'Img;");
      Put_Line("    else");
      Put_Line("      return Return_Value;");
      Put_Line("    end if;");
      Put_Line("  end To_Integer_4_Checked;");
      Put_Line("");
      Put_Line("  function From_Integer_4_Checked(X: Integer_4) return " & Column.Define.Camel_Case & "_Type is");
      Put_Line("    function From_Integer_4 is new Unchecked_Conversion(Integer_4, " & Column.Define.Camel_Case & "_Type);");
      Put_Line("    Return_Value : " & Column.Define.Camel_Case & "_Type := From_Integer_4(X);");
      Put_Line("  begin");
      Put_Line("    if not Return_Value'Valid then");
      Put_Line("      raise Constraint_Error with " & Quote("Value out of range: ") & " & X'Img;");
      Put_Line("    else");
      Put_Line("      return Return_Value;");
      Put_Line("    end if;");
      Put_Line("  end From_Integer_4_Checked;");
      Put_Line("  --  ----- " & Column.Name.Camel_Case & " -->>");
      Code_Debug("  -- stop  Print_Functions_Body ");
    end Print_Functions_Body;
    -----------------------------------------------------------------
--    procedure Print_Private_Body(Column : Repository.Column.Column_Type) is
--    begin
--      Code_Debug("  -- start Print_Private_Body ");
--      Code_Debug("  -- stop  Print_Private_Body ");
--    end Print_Private_Body;
    -----------------------------------------------------------------
    procedure Print_Package_End_Body(Column : Repository.Column.Column_Type) is
      pragma Unreferenced(Column);
    begin
      Code_Debug("  -- start Print_Package_End_Body ");
      Put_Line("end Coded_Values;");
      Code_Debug("  -- stop  Print_Package_End_Body ");
    end Print_Package_End_Body;

    -----------------------------------------------------------------

    Dummy : Repository.Column.Column_Type;

  begin
    String_Split.Create (S          => Subs,
                         From       => All_Values.Fix_String,
                         Separators => Seps,
                         Mode       => String_Split.Multiple);

    Print_Header_Spec(Dummy);
    Print_Withs_Spec(Dummy);
    Print_Package_Start_Spec(Dummy);
    -- for each cv
    for j in 1 .. String_Split.Slice_Count(Subs) loop
      declare
        Full_File_Name : String  := String_Split.Slice(Subs, j);
        File_Name      : String  := Ad.Base_Name(Full_File_Name); --no path nor extension left
        Prefix         : String  := Self.Item(Codes).Prefix.Fix_String;
        Len            : Integer := Prefix'Length;
        Col_Name       : String(1 .. File_Name'Last - Len -1 ) := File_Name(Len +2.. File_Name'Last);
      begin
        Col.Create(Col_Name, Self);
        if Data_Type(Col.Type_Of) = A_Short_Code and then
          Col.Define.UBString /= Null_Unbounded_String then
          Print_Functions_Spec(Col);
          Column_List.Append(Col);
        end if;
        Col.Reset;
      end;
    end loop;

    Print_Private(Dummy);
    for C of Column_List loop
      Print_Private_Spec(C);
    end loop;
    Print_Package_End_Spec(Dummy);

    Print_Header_Body(Dummy);
    Print_Withs_Body(Dummy);
    Print_Package_Start_Body(Dummy);
    for C of Column_List loop
      Print_Functions_Body(C);
    end loop;
    Column_List.Clear;
    Print_Package_End_Body(Dummy);

  end Make_Coded_Values;
  -------------------------------------------------------------------------------------------





  procedure Make_Extract_Package(Self : in out Config_Type) is
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Print_Extract_Header_Spec is
      use Text_Io;
    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Extract_Header_Spec ");
      Put_Line(Header.Fix_String);
      Put_Line("");
      Code_Debug("  -- stop  Make_Extract_Package.Print_Extract_Header_Spec ");
    end Print_Extract_Header_Spec;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Print_Extract_Withs_Spec(Table_Names,Clreq_Names : String_Object) is
      use Gnat; use Text_Io;
      Subs1,Subs2 : String_Split.Slice_Set;
      Seps : constant String := " ";
    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Extract_Withs_Spec ");
      Put_Line("with Sattmate_Xml;");
      Put_Line("");
      Put_Line("-- db-tables");
      Put_Line("");

      String_Split.Create (S          => Subs1,
                           From       => Table_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each table
      for j in 1 .. String_Split.Slice_Count(Subs1) loop
        declare
          Name : String  := String_Split.Slice(Subs1, j);
          Tmp  : String_Object;
        begin
          Tmp.Set(Name);
          Put_Line("with Table_" & Tmp.Camel_Case & ";");
        end;
      end loop;

      Put_Line("");
      Put_Line("-- Clreqs-tables");
      Put_Line("");

      String_Split.Create (S          => Subs2,
                           From       => Clreq_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each clreq
      for j in 1 .. String_Split.Slice_Count(Subs2) loop
        declare
          Name : String  := String_Split.Slice(Subs2, j);
          Tmp  : String_Object;
        begin
          Tmp.Set(Name);
          Put_Line("with Table_" & Tmp.Camel_Case & ";");
        end;
      end loop;

      Put_Line("");

      Code_Debug("  -- stop  Make_Extract_Package.Print_Extract_Withs_Spec ");
    end Print_Extract_Withs_Spec;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Print_Extract_Package_Start_Spec is
      use Text_Io;
    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Extract_Package_Start_Spec ");
      Put_Line("package M2_Convert_Xml is");
      Put_Line("");
      Put_Line("  Unknown_Column : exception;");
      Put_Line("");
      Code_Debug("  -- stop  Make_Extract_Package.Print_Extract_Package_Start_Spec ");
    end Print_Extract_Package_Start_Spec;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Print_Extract_XXX_Spec(Table_Names,Clreq_Names : String_Object) is
      use Gnat; use Text_Io;
      Subs1,Subs2 : String_Split.Slice_Set;
      Seps : constant String := " ";
      ----++++----++++----++++----++++----++++----++++----++++----++++
      procedure Do_Print(Tbl: String_Object) is
      begin
        Put_Line("-----------------------------------------------------");
        Put_Line("---- " & Tbl.Camel_Case & " start---------");
        Put_Line("");
        Put_Line("  procedure Extract     (Request : in     Sattmate_Xml.Document;");
        Put_Line("                         Tag     : in     String;");
        Put_Line("                         Data    : in out Table_" & Tbl.Camel_Case & ".Data_Type);");
        Put_Line("-----------------------------------------------------");
        Put_Line("");
        Put_Line("  procedure Extract_Keys(Request : in     Sattmate_Xml.Document;");
        Put_Line("                         Tag     : in     String;");
        Put_Line("                         Data    : in out Table_" & Tbl.Camel_Case & ".Data_Type);");
        Put_Line("-----------------------------------------------------");
        Put_Line("");
        Put_Line("  procedure Extract_Data(Request : in     Sattmate_Xml.Document;");
        Put_Line("                         Tag     : in     String;");
        Put_Line("                         Data    : in out Table_" & Tbl.Camel_Case & ".Data_Type);");
        Put_Line("-----------------------------------------------------");
        Put_Line("");
        Put_Line("  function Extract_All  (Request : in     Sattmate_Xml.Document;");
        Put_Line("                         Tag     : in     String) return Table_" & Tbl.Camel_Case & ".Data_Type;");
        Put_Line("");
        Put_Line("---- " & Tbl.Camel_Case & " stop-------");
        Put_Line("-----------------------------------------------------");
        Put_Line("");
      end Do_Print;
      ----++++----++++----++++----++++----++++----++++----++++----++++

    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Extract_XXX_Spec ");
      Put_Line("");

      String_Split.Create (S          => Subs1,
                           From       => Table_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each table
      for j in 1 .. String_Split.Slice_Count(Subs1) loop
        declare
          Name : String  := String_Split.Slice(Subs1, j);
          Tbl  : String_Object;
        begin
          Tbl.Set(Name);
          Do_Print(Tbl);
        end;
      end loop;

      String_Split.Create (S          => Subs2,
                           From       => Clreq_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each clreq
      for j in 1 .. String_Split.Slice_Count(Subs2) loop
        declare
          Name : String  := String_Split.Slice(Subs2, j);
          Tbl  : String_Object;
        begin
          Tbl.Set(Name);
          Do_Print(Tbl);
        end;
      end loop;

      Put_Line("");

      Code_Debug("  -- stop  Make_Extract_Package.Print_Extract_XXX_Spec ");
    end Print_Extract_XXX_Spec;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Print_Extract_Package_End_Spec is
      use Text_Io;
    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Extract_Package_End_Spec ");
      Put_Line("-----------------------------------------------------");
      Put_Line("end M2_Convert_Xml;");
      Put_Line("");
      Code_Debug("  -- stop  Make_Extract_Package.Print_Extract_Package_End_Spec ");
    end Print_Extract_Package_End_Spec;

    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Print_Extract_Header_Body is
      use Text_Io;
    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Extract_Header_Body ");
      Put_Line(Header.Fix_String);
      Put_Line("");
      Code_Debug("  -- stop  Make_Extract_Package.Print_Extract_Header_Body ");
    end Print_Extract_Header_Body;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Print_Extract_Withs_Body is
      use Text_Io;
    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Extract_Withs_Body ");
      Put_Line("with Sattmate_Types; use Sattmate_Types;");
      Put_Line("with M2_Utils;");
      Put_Line("with Sattmate_Calendar;");
--      Put_Line("with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;");
      Put_Line("");
      Code_Debug("  -- stop  Make_Extract_Package.Print_Extract_Withs_Body ");
    end Print_Extract_Withs_Body;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Print_Extract_Package_Start_Body is
      use Text_Io;
    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Extract_Package_Start_Body ");
      Put_Line("package body M2_Convert_Xml is");
      Put_Line("");

      Code_Debug("  -- stop  Make_Extract_Package.Print_Extract_Package_Start_Body ");
    end Print_Extract_Package_Start_Body;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Print_Extract_XXX_Body(Table_Names,Clreq_Names : String_Object) is
      use Gnat; use Text_Io;
      Subs1,Subs2 : String_Split.Slice_Set;
      Seps : constant String := " ";
      --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
      procedure Do_Print_Start(Tbl: String_Object) is
      begin
        Put_Line("--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++-++--++");
        Put_Line("  procedure Extract_Keys(Request : in     Sattmate_Xml.Document;");
        Put_Line("                         Tag     : in     String;");
        Put_Line("                         Data    : in out Table_" & Tbl.Camel_Case & ".Data_Type) is");
        Put_Line("  begin");
        Put_Line("      Extract(Request, Tag & " & Quote("/Key") & ", Data);");
        Put_Line("  end Extract_Keys; ");
        Put_Line("  --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++-++--++");
        Put_Line("  procedure Extract_Data(Request : in     Sattmate_Xml.Document;");
        Put_Line("                         Tag     : in     String;");
        Put_Line("                         Data    : in out Table_" & Tbl.Camel_Case & ".Data_Type) is");
        Put_Line("  begin");
        Put_Line("      Extract(Request, Tag & " & Quote("/Columns") & ", Data);");
        Put_Line("  end Extract_Data;");
        Put_Line("  --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++-++--++");
        Put_Line("  function Extract_All(Request : Sattmate_Xml.Document;");
        Put_Line("                       Tag     : String) return Table_" & Tbl.Camel_Case & ".Data_Type is");
        Put_Line("    Data : Table_" & Tbl.Camel_Case & ".Data_Type;");
        Put_Line("  begin");
        Put_Line("      Extract_Keys(Request, Tag, Data);");
        Put_Line("      Extract_Data(Request, Tag, Data);");
        Put_Line("    return Data;");
        Put_Line("  end Extract_All;");
        Put_Line("  --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++-++--++");
        Put_Line("  procedure Extract(Request : in     Sattmate_Xml.Document;");
        Put_Line("                    Tag     : in     String;");
        Put_Line("                    Data    : in out Table_" & Tbl.Camel_Case & ".Data_Type) is");
        Put_Line("    use Sattmate_Xml;");
        Put_Line("    use M2_Utils;");
        Put_Line("    The_Node    : Node := First_Node(Expr => Tag, Doc => Request);");
        Put_Line("    Column_Node : Node;");
        Put_Line("    Columns     : Node_List;");
        Put_Line("  begin");
        Put_Line("    Columns := Child_Nodes(The_Node);");
        Put_Line("    for J in 0 .. Length(Columns) - 1 loop");
        Put_Line("      Column_Node := Sattmate_Xml.Item(Columns, J);");
        Put_Line("      declare");
        Put_Line("        The_Name  : String := Get_Attribute(Column_Node, Name);");
        Put_Line("        The_Value : String := Get_Attribute(Column_Node, Val);");
        Put_Line("      begin");
      end Do_Print_Start;
      --++--++--++--++--++--++--++--
      procedure Do_Print_Middle(Tbl : Repository.Table.Table_Type) is
        First_Time : Boolean := True;
      begin
        for Col of Tbl.Columns loop
          if First_Time then
            Put_Line("        if     Check(" & Quote(Col.Name.Upper_Case) & ", The_Name, The_Name'Length) then");
            First_Time := False;
          else
            Put_Line("        elsif  Check(" & Quote(Col.Name.Upper_Case) & ", The_Name, The_Name'Length) then");
          end if;
          case Data_Type(Col.Type_Of) is
            when A_Char =>
              if Col.Size_Of = 1 then
                Put_Line("          begin  -- Handle empty strings as well");
                Put_Line("            Data." & Col.Name.Camel_Case & " := Trim(The_Value)(1);");
                Put_Line("          exception");
                Put_Line("            when Constraint_Error =>");
                Put_Line("              Data." & Col.Name.Camel_Case & " := ' ';");
                Put_Line("          end;");
              else
                Put_Line("          Move(M2_Utils.To_Iso_Latin_15(The_Value), Data." & Col.Name.Camel_Case & ");");
              end if;
            when A_Int .. A_Long | A_Short_Code | A_Boolean =>
              Put_Line("          Data." & Col.Name.Camel_Case & " := Integer_4'Value(The_Value);");
            when A_Float .. A_Double =>
              Put_Line("          Data." & Col.Name.Camel_Case & " := Fixed_Type'Value(The_Value);");
            when A_Date =>
              Put_Line("          Data." & Col.Name.Camel_Case & " := Sattmate_Calendar.To_Time_Type(The_Value," & Quote("00:00:00.000") & ");");
            when A_Time =>
              Put_Line("          Data." & Col.Name.Camel_Case & "  := Sattmate_Calendar.To_Time_Type(" & Quote("01-JAN-1901") & ",The_Value);");
            when A_Timestamp =>
              Put_Line("          Data." & Col.Name.Camel_Case & " := Sattmate_Calendar.To_Time_Type(The_Value(1..11), The_Value(13..24));");
            when A_Clob .. A_Blob | A_Char_Code  =>
            Put_Line( "          null; --" & Quote(Col.Name.Camel_Case & " = not supported datatype for XML " & Data_Type(Col.Type_Of)'Img) & ";");
          end case;
        end loop;
        Put_Line("        end if;");
      end Do_Print_Middle;

      procedure Do_Print_End is
      begin
        Put_Line("      end;");
        Put_Line("    end loop;");
        Put_Line("  end Extract;");
      end Do_Print_End;
      --++--++--++--++--++--++--++--
    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Extract_XXX_Body ");

      String_Split.Create (S          => Subs1,
                           From       => Table_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each table
      for j in 1 .. String_Split.Slice_Count(Subs1) loop
        declare
          Name : String  := String_Split.Slice(Subs1, j);
          Tbl  : String_Object;
          T_O  : Repository.Table.Table_Type with Warnings => Off;
        begin
          Tbl.Set(Name);
          T_O.Create(Name,Self);
          Do_Print_Start(Tbl);
          Do_Print_Middle(T_O);
          Do_Print_End;
          T_O.Reset;
        end;
      end loop;

      String_Split.Create (S          => Subs2,
                           From       => Clreq_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each clreq
      for j in 1 .. String_Split.Slice_Count(Subs2) loop
        declare
          Name : String  := String_Split.Slice(Subs2, j);
          Tbl  : String_Object;
          T_O  : Repository.Table.Table_Type;
        begin
          Tbl.Set(Name);
          Do_Print_Start(Tbl);
          T_O.Create_Ud4(Name,Self);
          Do_Print_Middle(T_O);
          T_O.Reset;
          Do_Print_End;
        end;
      end loop;

      Put_Line("");
      Code_Debug("  -- stop  Make_Extract_Package.Print_Extract_XXX_Body ");
    end Print_Extract_XXX_Body;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++
    procedure Print_Extract_Package_End_Body is
      use Text_Io;
    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Extract_Package_End_Body ");
      Put_Line("end M2_Convert_Xml;");
      Put_Line("");
      Code_Debug("  -- stop  Make_Extract_Package.Print_Extract_Package_End_Body ");
    end Print_Extract_Package_End_Body;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--++--++

    All_Tables : String_Object := String_Object(Self.All_Entities_Defined_Names(Tables));
    All_Clreqs : String_Object := String_Object(Self.All_Entities_Defined_Names(Clreqs));
  begin

    Print_Extract_Header_Spec;
    Print_Extract_Withs_Spec(All_Tables,All_Clreqs);
    Print_Extract_Package_Start_Spec;
    Print_Extract_XXX_Spec(All_Tables,All_Clreqs);
    Print_Extract_Package_End_Spec;

    Print_Extract_Header_Body;
    Print_Extract_Withs_Body;
    Print_Extract_Package_Start_Body;
    Print_Extract_XXX_Body(All_Tables,All_Clreqs);
    Print_Extract_Package_End_Body;

  end Make_Extract_Package;

  -------------------------------------------------------------------------------------------

  procedure Make_Xml_To_Ud4(Self : in out Config_Type) is
    use Gnat; use Text_Io;

    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Header_Spec is
    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Header_Spec ");
      Put_Line(Header.Fix_String);
      Put_Line("");
      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Header_Spec ");
    end Print_Xml_Ud4_Header_Spec;
    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Withs_Spec(Clreq_Names : String_Object) is
      Subs : String_Split.Slice_Set;
      Seps : constant String := " ";
    begin
      Code_Debug("  -- start Make_Extract_Package.Print_Xml_Ud4_Withs_Spec ");
      Put_Line("with Sattmate_Types; use Sattmate_Types;");
      Put_Line("with Sattmate_Xml;");
      Put_Line("with Uniface_Request;");
      Put_Line("with Process_Io;");
      Put_Line("");
      Put_Line("");

      String_Split.Create (S          => Subs,
                           From       => Clreq_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each clreq
      for j in 1 .. String_Split.Slice_Count(Subs) loop
        declare
          Name : String  := String_Split.Slice(Subs, j);
          Tmp  : String_Object;
        begin
          Tmp.Set(Name);
          Put_Line("with Table_" & Tmp.Camel_Case & ";");
        end;
      end loop;

      Put_Line("");

      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Withs_Spec ");
    end Print_Xml_Ud4_Withs_Spec;
    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Package_Start_Spec is
    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Package_Start_Spec ");
      Put_Line("package M2_Xmlud4 is");
      Put_Line("");
      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Package_Start_Spec ");
    end Print_Xml_Ud4_Package_Start_Spec;
    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Single_Spec is
    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Single_Spec ");
      Put_Line("  procedure BuildXmlTree(Service     : in     String;");
      Put_Line("                         Table_name  : in     String;");
      Put_Line("                         Status      : in     Uniface_Request.Status_Type;");
      Put_Line("                         Operation   : in     Uniface_Request.Operation_Type;");
      Put_Line("                         Reply       : in out Sattmate_Xml.Document);");
      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Single_Spec ");
    end Print_Xml_Ud4_Single_Spec;
    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Spec(Clreq_Names : String_Object) is

      procedure Do_Print(Tbl_Name : String_Object) is
      begin

        Put_Line("---- " & Tbl_Name.Camel_Case & " start ---------");
        Put_Line("");
        Put_Line("  procedure BuildXmlTree(Data        : in     Table_" & Tbl_Name.Camel_Case & ".Data_Type;");
        Put_Line("                         Service     : in     String;");
        Put_Line("                         Status      : in     Uniface_Request.Status_Type;");
        Put_Line("                         Operation   : in     Uniface_Request.Operation_Type;");
        Put_Line("                         Reply       : in out Sattmate_Xml.Document);");
        Put_Line("");
        Put_Line("  procedure BuildXmlTree(DataList    : in     Table_" & Tbl_Name.Camel_Case & "." & Tbl_Name.Camel_Case & "_List_Pack.List_Type;");
        Put_Line("                         Service     : in     String;");
        Put_Line("                         Status      : in     Uniface_Request.Status_Type;");
        Put_Line("                         Operation   : in     Uniface_Request.Operation_Type;");
        Put_Line("                         Reply       : in out Sattmate_Xml.Document);");
        Put_Line("");
        Put_Line("  procedure SendToProcess(Data           : in out Table_" & Tbl_Name.Camel_Case & ".Data_Type;");
        Put_Line("                          Service        : in     String;");
        Put_Line("                          Operation      : in     Uniface_Request.Operation_Type;");
        Put_Line("                          Server_Process : in     Process_Io.Process_Type;");
        Put_Line("                          Timeout        : in     Integer_4;");
        Put_Line("                          Reply          : in out Sattmate_Xml.Document;");
        Put_Line("                          Status         : in out Uniface_Request.Status_Type);");
        Put_Line("");
        Put_Line("---- " & Tbl_Name.Camel_Case & " end ---------");
      end Do_Print;

      Subs : String_Split.Slice_Set;
      Seps : constant String := " ";

    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Spec ");
      Put_Line("");

      String_Split.Create (S          => Subs,
                           From       => Clreq_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each clreq
      for j in 1 .. String_Split.Slice_Count(Subs) loop
        declare
          Name : String  := String_Split.Slice(Subs, j);
          Tmp  : String_Object;
        begin
          Tmp.Set(Name);
          Do_Print(Tmp);
        end;
      end loop;

      Put_Line("");

      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Spec ");
    end Print_Xml_Ud4_Spec;
    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Package_End_Spec is
    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Package_End_Spec ");
      Put_Line("");
      Put_Line("end M2_Xmlud4;");
      Put_Line("");
      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Package_End_Spec ");
    end Print_Xml_Ud4_Package_End_Spec;
    --++--++--++--++--++--++--++--++--++--++--

    procedure Print_Xml_Ud4_Header_Body is
    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Header_Body ");
      Put_Line(Header.Fix_String);
      Put_Line("");
      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Header_Body ");
    end Print_Xml_Ud4_Header_Body;
    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Withs_Body is
    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Withs_Body ");
      Put_Line("");
      Put_Line("with M2_Utils;");
      Put_Line("with M2_Replies; use M2_Replies;");
      Put_Line("with Sattmate_Calendar; use Sattmate_Calendar;");
      Put_Line("with General_Routines;");
      Put_Line("");
      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Withs_Body ");
    end Print_Xml_Ud4_Withs_Body;
    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Package_Start_Body is
    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Package_Start_Body ");
      Put_Line("");
      Put_Line("package body M2_Xmlud4 is");
      Put_Line("  use Sattmate_Xml;");
      Put_Line("  use M2_Utils;");
      Put_Line("  use Uniface_Request;");
      Put_Line("");
      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Package_Start_Body ");
    end Print_Xml_Ud4_Package_Start_Body;
    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Single_Body is
    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Single_Body ");
      Put_Line("");
      Put_Line("  procedure BuildXmlTree(Service     : in     String;");
      Put_Line("                         Table_name  : in     String;");
      Put_Line("                         Status      : in     Uniface_Request.Status_Type;");
      Put_Line("                         Operation   : in     Uniface_Request.Operation_Type;");
      Put_Line("                         Reply       : in out Sattmate_Xml.Document) is");
      Put_Line("");
      Put_Line("    Elements : array (1..5) of Node;");
      Put_Line("    Last_Element : Node;");
      Put_Line("    pragma Unreferenced(Last_Element); --last element is only assigned...");
      Put_Line("    R : Sattmate_Xml.Document := Reply;");
      Put_Line("    pragma Warnings(Off, Reply);");
      Put_Line("  begin");
      Put_Line("    Elements(1) := Append_Child(Reply, Create_Element_NS(R, " & Quote("") & ", MaMustangRPC));");
      Put_Line("    Elements(2) := Append_Child(Elements(1),Create_Element(R, SR));");
      Put_Line("    Elements(3) := Append_Child(Elements(2),Create_Element(R, Service));");
      Put_Line("    Set_Attribute(Elements(3), " & Quote("Operation") & ", Uniface_Request.Operation_Type'Image(Operation));");
      Put_Line("    Set_Attribute(Elements(3), " & Quote("Status") & ",  Uniface_Request.Status_Type'Image(Status));");
      Put_Line("    Elements(4)  := Append_Child(Elements(3),Create_Text_Node(R, Table_name));");
      Put_Line("    Elements(5) := Append_Child(Elements(2),Create_Element(R,Cc));");
      Put_Line("    Last_Element := Append_Child(Elements(5),Create_Text_Node(R,Trim(Integer_4'Image(Error(Mg_Ok)))));");
      Put_Line("  end BuildXmlTree;");
      Put_Line("");
      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Single_Body ");
    end Print_Xml_Ud4_Single_Body;
    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Body(Clreq_Names : String_Object) is
      Subs: String_Split.Slice_Set;
      Seps : constant String := " ";
      --------------------------------------------------------------------------
      function Type_To_Value (Col : Table.Table_Column_Type) return String_Object is
        Tmp : String_Object;
      begin
        case Data_Type(Col.Type_Of) is
          when A_Char =>
            if Col.Size_Of = 1 then
              Tmp.Set("(1..1 => (Data." & Col.Name.Camel_Case& "))");
            else
               Tmp.Set("Trim(Data." & Col.Name.Camel_Case & ")");
            end if;
          when A_Int .. A_Long | A_Short_Code | A_Boolean =>
            Tmp.Set("Trim(Data." & Col.Name.Camel_Case & "'Img)");
          when A_Float .. A_Double =>
            Tmp.Set("General_Routines.F8_To_String(Data." & Col.Name.Camel_Case & ")");
          when A_Date =>
            Tmp.Set("Sattmate_Calendar.String_Date(Data." & Col.Name.Camel_Case & ")");
          when A_Time =>
            Tmp.Set("Sattmate_Calendar.String_Time(Data." & Col.Name.Camel_Case & ")");
          when A_Timestamp =>
            Tmp.Set("Sattmate_Calendar.String_Date_And_Time(Data." & Col.Name.Camel_Case & ", Milliseconds => True )");
          when A_Clob .. A_Blob | A_Char_Code =>
            Tmp.Set(Quote(Col.Name.Camel_Case & " = not supported datatype for Type_To_Value " & Data_Type(Col.Type_Of)'Img));
        end case;
        return Tmp;
      end Type_To_Value;
      --------------------------------------------------------------------------

    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Body ");
      Put_Line("");
      String_Split.Create (S          => Subs,
                           From       => Clreq_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each clreq
      for j in 1 .. String_Split.Slice_Count(Subs) loop
        declare
          Name : String  := String_Split.Slice(Subs, j);
          Clq  : Repository.Table.Table_Type with Warnings => Off;
          Elem_Count,
          Parent_Elem,
          Curr_Elem,
          Text_Node : Natural := 0;
        begin
          Clq.Create_Ud4(Name,Self);
          Elem_Count := 8 + 2*Integer(Clq.Num_Columns) - 1;

          Put_Line("-----------------------------------------------------");
          Put_Line("");
          Put_Line("---- start " & Clq.Name.Camel_Case & " ---------");
          Put_Line("");
          Put_Line("  procedure BuildXmlTree(Data        : in     Table_" & Clq.Name.Camel_Case & ".Data_Type;");
          Put_Line("                         Service     : in     String;");
          Put_Line("                         Status      : in     Uniface_Request.Status_Type;");
          Put_Line("                         Operation   : in     Uniface_Request.Operation_Type;");
          Put_Line("                         Reply       : in out Sattmate_Xml.Document) is");
          Put_Line("    use Table_" & Clq.Name.Camel_Case & ";");
          Put_Line("    Elements     : array (1.." & Elem_Count'Img & ") of Node;");
          Put_Line("    Last_Element : Node;");
          Put_Line("    pragma Unreferenced(Last_Element); --last element is only assigned...");
          Put_Line("    R : Sattmate_Xml.Document := Reply;");
          Put_Line("    pragma Warnings(Off, Reply);");
          Put_Line("  begin");
          Put_Line("    Elements(1) := Append_Child(Reply, Create_Element_NS(R, " & Quote("") & ", MaMustangRPC));");
          Put_Line("    Elements(2) := Append_Child(Elements(1),Create_Element(R, SR));");
          Put_Line("    Elements(3) := Append_Child(Elements(2),Create_Element(R, Service));");
          Put_Line("    Set_Attribute(Elements(3), " & Quote("Operation") & ", Uniface_Request.Operation_Type'Image(Operation));");
          Put_Line("    Set_Attribute(Elements(3), " & Quote("Status") & ",  Uniface_Request.Status_Type'Image(Status));   ");
          Put_Line("    Elements(4)  := Append_Child(Elements(3),Create_Text_Node(R, " & Quote(Clq.Name.Upper_Case) & "));");
          Put_Line("    Elements(5) := Append_Child(Elements(2),Create_Element(R, M2_Utils.List));");
          Put_Line("    Elements(6)  := Append_Child(Elements(5),Create_Element(R, M2_Utils.Item));");

          Parent_Elem := 6;
          Curr_Elem   := 7;
          Text_Node   := 8;

          for Col of Clq.Columns loop
            Put_Line("    Elements(" & Trim(Curr_Elem'Img,Left) & ") := Append_Child(Elements(" & Trim(Parent_Elem'Img,Left) & "),Create_Element(R, " & Col.Name.Camel_Case & "_Name));");
            Put_Line("    Elements(" & Trim(Text_Node'Img,Left) & ") := Append_Child(Elements(" & Trim(Curr_Elem'Img  ,Left) & "),Create_Text_Node(R, " & Type_To_Value(Col).Fix_String & "));");
            Curr_Elem := Curr_Elem + 2;
            Text_Node := Text_Node + 2;
          end loop;

          Put_Line("    Elements(" & Curr_Elem'Img & ") := Append_Child(Elements(2),Create_Element(R,Cc));");
          Put_Line("    Last_Element := Append_Child(Elements(" & Curr_Elem'Img & "),Create_Text_Node(R,Trim(Integer_4'Image(Error(Mg_Ok)))));");
          Put_Line("  end BuildXmlTree;");
          Put_Line("");
          Put_Line("  procedure BuildXmlTree(DataList    : in     Table_" & Clq.Name.Camel_Case & "." & Clq.Name.Camel_Case & "_List_Pack.List_Type;");
          Put_Line("                         Service     : in     String;");
          Put_Line("                         Status      : in     Uniface_Request.Status_Type;");
          Put_Line("                         Operation   : in     Uniface_Request.Operation_Type;");
          Put_Line("                         Reply       : in out Sattmate_Xml.Document) is");
          Put_Line("    use Table_" & Clq.Name.Camel_Case & ";");
          Put_Line("    Data : Table_" & Clq.Name.Camel_Case & ".Data_Type;");
          Put_Line("    Elements     : array (1.." & Elem_Count'Img & ") of Node;");
          Put_Line("    Last_Element : Node;");
          Put_Line("    pragma Unreferenced(Last_Element); --last element is only assigned...");
          Put_Line("    R : Sattmate_Xml.Document := Reply;");
          Put_Line("    pragma Warnings(Off, Reply);");
          Put_Line("  begin");
          Put_Line("    Elements(1) := Append_Child(Reply, Create_Element_NS(R, " & Quote("") & ", MaMustangRPC));");
          Put_Line("    Elements(2) := Append_Child(Elements(1),Create_Element(R, SR));");
          Put_Line("    Elements(3) := Append_Child(Elements(2),Create_Element(R, Service));");
          Put_Line("    Set_Attribute(Elements(3), " & Quote("Operation") & ", Uniface_Request.Operation_Type'Image(Operation));");
          Put_Line("    Set_Attribute(Elements(3), " & Quote("Status") & ",  Uniface_Request.Status_Type'Image(Status));   ");
          Put_Line("    Elements(4)  := Append_Child(Elements(3),Create_Text_Node(R, " & Quote(Clq.Name.Upper_Case) & "));");
          Put_Line("    Elements(5) := Append_Child(Elements(2),Create_Element(R, M2_Utils.List));");
          Put_Line("    while not Table_" & Clq.Name.Camel_Case & "." & Clq.Name.Camel_Case & "_List_Pack.Is_Empty(DataList) loop");
          Put_Line("      Table_" & Clq.Name.Camel_Case & "." & Clq.Name.Camel_Case & "_List_Pack.Remove_From_Head(DataList, Data);");
          Put_Line("      Elements(6)  := Append_Child(Elements(5),Create_Element(R, M2_Utils.Item));");

          Parent_Elem := 6;
          Curr_Elem   := 7;
          Text_Node   := 8;

          for Col of Clq.Columns loop
            Put_Line("      Elements(" & Trim(Curr_Elem'Img,Left) & ") := Append_Child(Elements(" & Trim(Parent_Elem'Img,Left) & "),Create_Element(R, " & Col.Name.Camel_Case & "_Name));");
            Put_Line("      Elements(" & Trim(Text_Node'Img,Left) & ") := Append_Child(Elements(" & Trim(Curr_Elem'Img  ,Left) & "),Create_Text_Node(R, " & Type_To_Value(Col).Fix_String & "));");
            Curr_Elem := Curr_Elem + 2;
            Text_Node := Text_Node + 2;
          end loop;

          Put_Line("    end loop;");
          Put_Line("    Elements(" & Curr_Elem'Img & ") := Append_Child(Elements(2),Create_Element(R,Cc));");
          Put_Line("    Last_Element := Append_Child(Elements(" & Curr_Elem'Img & "),Create_Text_Node(R,Trim(Integer_4'Image(Error(Mg_Ok)))));");
          Put_Line("  end BuildXmlTree;");
          Put_Line("");
          Put_Line("");
          Put_Line("  procedure SendToProcess(Data           : in out Table_" & Clq.Name.Camel_Case & ".Data_Type;");
          Put_Line("                          Service        : in     String;");
          Put_Line("                          Operation      : in     Uniface_Request.Operation_Type;");
          Put_Line("                          Server_Process : in     Process_Io.Process_Type;");
          Put_Line("                          Timeout        : in     Integer_4;");
          Put_Line("                          Reply          : in out Sattmate_Xml.Document;");
          Put_Line("                          Status         : in out Uniface_Request.Status_Type) is");
          Put_Line("    use Table_" & Clq.Name.Camel_Case & ";");
          Put_Line("    DataList  : " & Clq.Name.Camel_Case & "_List_Pack.List_Type := " & Clq.Name.Camel_Case & "_List_Pack.Create;");
          Put_Line("    U_Request : Uniface_Request.Request_Type;");
          Put_Line("    Clreq_Service : constant String := " & Quote("ClRec") & " & Trim(Service);");
          Put_Line("  begin");
          Put_Line("    Status := Uniface_Request.Failure;");
          Put_Line("    Empty_Mailbox;");
          Put_Line("    Table_" & Clq.Name.Camel_Case & ".Make_Ud4_Telegram(U_Request, Data, Operation);");
          Put_Line("    Uniface_Request.Set_Status(U_Request, Uniface_Request.Success);");
          Put_Line("    Uniface_Request.Set_Client(U_Request, Server_Process);");
          Put_Line("    Uniface_Request.Set_Operation(U_Request, Operation);");
          Put_Line("    Uniface_Request.Send_to_Client(U_Request);");
          Put_Line("    -- wait for answer from Server_Process");
          Put_Line("    declare");
          Put_Line("      Message : Process_Io.Message_Type;");
          Put_Line("      Request : Uniface_Request.Request_Type;");
          Put_Line("    begin");
          Put_Line("      loop");
          Put_Line("        begin");
          Put_Line("          Process_Io.Receive(Message, Duration(Timeout));");
          Put_Line("        exception");
          Put_Line("          when Process_Io.Timeout =>");
          Put_Line("            BuildXmlTree(DataList, Clreq_Service, Uniface_Request.Failure, Operation, Reply);");
          Put_Line("            exit;");
          Put_Line("        end;");
          Put_Line("        Request := Uniface_Request.Construct(Message);");
          Put_Line("        Status := Uniface_Request.Get_Status(Request);");
          Put_Line("        case Status is");
          Put_Line("          when Uniface_Request.Success      =>");
          Put_Line("            begin");
          Put_Line("              Get_Values(Request, Data);");
          Put_Line("              Table_" & Clq.Name.Camel_Case & "." & Clq.Name.Camel_Case & "_List_Pack.Insert_At_Tail(DataList,Data);");
          Put_Line("              if Operation /= Uniface_Request.Get_All_Records then");
          Put_Line("                BuildXmlTree(DataList, Clreq_Service, Uniface_Request.Success, Operation, Reply);");
          Put_Line("                exit;");
          Put_Line("              end if;");
          Put_Line("            exception");
          Put_Line("              when Uniface_Request.No_Such_Column =>");
          Put_Line("                  Error(Trim(Service), Reply, Parameter_Error);");
          Put_Line("                exit;");
          Put_Line("            end;");
          Put_Line("          when Uniface_Request.End_Of_Table =>");
          Put_Line("            Status := Uniface_Request.Success;");
          Put_Line("            BuildXmlTree(DataList, Clreq_Service, Status, Operation, Reply);");
          Put_Line("            exit;");
          Put_Line("          when Uniface_Request.Failure      =>");
          Put_Line("            BuildXmlTree(DataList, Clreq_Service, Status, Operation, Reply);");
          Put_Line("            exit;");
          Put_Line("          when others                       =>");
          Put_Line("            Unknown_Service(Service => Clreq_Service, Reply => Reply);");
          Put_Line("            exit;");
          Put_Line("        end case;");
          Put_Line("      end loop;");
          Put_Line("    end;");
          Put_Line("    Table_" & Clq.Name.Camel_Case & "." & Clq.Name.Camel_Case & "_List_Pack.Release(DataList);");
          Put_Line("  end SendToProcess;");
          Put_Line("");
          Put_Line("---- end " & Clq.Name.Camel_Case & " ---------");
          Put_Line("");

          Clq.Reset;

        end;
      end loop;

      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Body ");
    end Print_Xml_Ud4_Body;
    --++--++--++--++--++--++--++--++--++--++--
    procedure Print_Xml_Ud4_Package_End_Body is
    begin
      Code_Debug("  -- start Make_Xml_To_Ud4.Print_Xml_Ud4_Package_End_Body ");
      Put_Line("");
      Put_Line("end M2_Xmlud4;");
      Put_Line("");
      Code_Debug("  -- stop  Make_Xml_To_Ud4.Print_Xml_Ud4_Package_End_Body ");
    end Print_Xml_Ud4_Package_End_Body;

    -------------- START Make_Xml_To_Ud4 ---------------------------
    All_Clreqs : String_Object := String_Object(Self.All_Entities_Defined_Names(Clreqs));
  begin

    Code_Debug("  -- start Make_Xml_To_Ud4 ");
    Print_Xml_Ud4_Header_Spec;
    Print_Xml_Ud4_Withs_Spec(All_Clreqs);
    Print_Xml_Ud4_Package_Start_Spec;
    Print_Xml_Ud4_Single_Spec;
    Print_Xml_Ud4_Spec(All_Clreqs);
    Print_Xml_Ud4_Package_End_Spec;

    Print_Xml_Ud4_Header_Body;
    Print_Xml_Ud4_Withs_Body;
    Print_Xml_Ud4_Package_Start_Body;
    Print_Xml_Ud4_Single_Body;
    Print_Xml_Ud4_Body(All_Clreqs);
    Print_Xml_Ud4_Package_End_Body;
    Code_Debug("  -- stop  Make_Xml_To_Ud4 ");

  end Make_Xml_To_Ud4;
  ----------------------------------------------------------------------------------

  procedure Make_C_Sharp_Class(Self : in out Config_Type; Clreq_Name : String_Object) is
    Clq : Repository.Table.Table_Type with Warnings => Off;
    use Text_Io;
  begin
    Clq.Name.Set(Clreq_Name.Lower_Case);
    Clq.Create_Ud4(Self);

    Put_Line("using System;");
    Put_Line("using System.Data;");
    Put_Line("using ProSet;");
    Put_Line("using System.Xml;");
    Put_Line("using System.Reflection;");
    Put_Line("using System.Globalization;");
    Put_Line("");
    Put_Line("namespace ???");
    Put_Line("{");
    Put_Line("  public class " & Clreq_Name.Camel_Case & "Xml");
    Put_Line("  {");
    Put_Line("    public struct " & Clreq_Name.Camel_Case & "Str");
    Put_Line("    {");

    for Col of Clq.Columns loop
      Put_Line("      public " & CSharp_Type_Mapper(Data_Type(Col.Type_Of)).all & " " & Col.Name.Lower_Case & ";");
    end loop;

    Put_Line("");
    declare
      S1 : String_Object;
    begin
      S1.Set("      public " & Clreq_Name.Camel_Case & "Str(");
      for Col of Clq.Columns loop
        S1.Append(Ascii.Lf & "               " & CSharp_Type_Mapper(Data_Type(Col.Type_Of)).all & " " & Col.Name.Lower_Case & ",");
      end loop;
      --remove the last ','
      S1.Delete_Last_Char;
      S1.Append(")");
      Put_Line(S1.Fix_String);
    end;
    Put_Line("      {");

    for Col of Clq.Columns loop
      Put_Line("        " & Col.Name.Lower_Case & " = " & Col.Name.Camel_Case & ";");
    end loop;

    Put_Line("      }");
    Put_Line("    }");
    Put_Line("    private static " & Clreq_Name.Camel_Case & "Str " & Clreq_Name.Camel_Case & ";");
    Put_Line("    private static FieldInfo[] fields = " & Clreq_Name.Camel_Case & ".GetType().GetFields(BindingFlags.Instance | BindingFlags.Public);");
    Put_Line("");
    Put_Line("    public static bool SendXml(ProClass ps, string operation, " & Clreq_Name.Camel_Case & "Str " & Clreq_Name.Lower_Case & ", ref XmlNode serviceReplyNode)");
    Put_Line("    {");
    Put_Line("      XmlNode RootNode;");
    Put_Line("      string ServiceName = " & Quote("ClRec_" & Clreq_Name.Upper_Case) & ";");
    Put_Line("      string TableName = " & Quote(Clreq_Name.Upper_Case) & ";");
    Put_Line("      RootNode = ps.Rpc.XmlCreateDocument(ServiceName);");
    Put_Line("      ps.Rpc.XmlAddElementAttribute((XmlElement)RootNode, m2SattmateServices.Common.OperationNodeName, operation);");
    Put_Line("      XmlNode TableNode = ps.Rpc.XmlAddChildNode(RootNode, " & Quote("Table") & ");");
    Put_Line("      ps.Rpc.XmlAddElementAttribute((XmlElement)TableNode, " & Quote("Name") & ", TableName);");
    Put_Line("      XmlNode KeyNode = ps.Rpc.XmlAddChildNode(TableNode, " & Quote("Columns") & ");");
    Put_Line("      XmlNode ColumnNode;");
    Put_Line("      foreach (FieldInfo field in fields)");
    Put_Line("      {");
    Put_Line("        ColumnNode = ps.Rpc.XmlAddChildNode(KeyNode, " & Quote("Column") & ");");
    Put_Line("        try");
    Put_Line("        {");
    Put_Line("          Type t = field.FieldType;");
    Put_Line("          string val = field.GetValue(" & Clreq_Name.Lower_Case & ").ToString();");
    Put_Line("          ps.Rpc.XmlAddElementAttribute((XmlElement)ColumnNode, " & Quote("Name") & ", field.Name.ToLower());");
    Put_Line("          if (t.Equals(typeof(System.Double)))");
    Put_Line("            val = dbcServer.dbcServer.dbStr(Convert.ToDouble(val));");
    Put_Line("          ps.Rpc.XmlAddElementAttribute((XmlElement)ColumnNode, " & Quote("Val") & ", val);");
    Put_Line("        }");
    Put_Line("        catch { }");
    Put_Line("      }");
    Put_Line("      return ps.Rpc.MaRpcExec(RootNode, true, ref serviceReplyNode);");
    Put_Line("    }");
    Put_Line("");
    Put_Line("    public static bool SendXmlGetStatus(ProClass ps, string operation, " & Clreq_Name.Camel_Case & "Str " & Clreq_Name.Lower_Case & ", ref string status)");
    Put_Line("    {");
    Put_Line("      XmlNode serviceReplyNode = null;");
    Put_Line("      if (SendXml(ps, operation, " & Clreq_Name.Lower_Case & ", ref serviceReplyNode))");
    Put_Line("      {");
    Put_Line("        status = m2SattmateServices.Common.GetStatusAttribute(serviceReplyNode);");
    Put_Line("        return true;");
    Put_Line("      }");
    Put_Line("      return false;");
    Put_Line("    }");
    Put_Line("");
    Put_Line("    public static bool SendXmlGetReply(ProClass ps, string operation, " & Clreq_Name.Camel_Case & "Str " & Clreq_Name.Lower_Case & ", ref DataTable dt, ref string status)");
    Put_Line("    {");
    Put_Line("      XmlNode serviceReplyNode = null;");
    Put_Line("      if (SendXml(ps, operation, " & Clreq_Name.Lower_Case & ", ref serviceReplyNode))");
    Put_Line("      {");
    Put_Line("        GetXmlReply(ps, serviceReplyNode, ref dt, ref status);");
    Put_Line("        return true;");
    Put_Line("      }");
    Put_Line("      return false;");
    Put_Line("    }");
    Put_Line("");
    Put_Line("    public static bool SendXmlGetReply(ProClass ps, string operation, ref " & Clreq_Name.Camel_Case & "Str " & Clreq_Name.Lower_Case & ", ref string status)");
    Put_Line("    {");
    Put_Line("      XmlNode serviceReplyNode = null;");
    Put_Line("      if (SendXml(ps, operation, " & Clreq_Name.Lower_Case & ", ref serviceReplyNode))");
    Put_Line("      {");
    Put_Line("        DataTable dt = null;");
    Put_Line("        GetXmlReply(ps, serviceReplyNode, ref dt, ref status);");
    Put_Line("        if (dt.Rows.Count > 0)");
    Put_Line("        {");
    Put_Line("          DataRow row = dt.Rows[0];");

    declare
      Tmp : String_Object;
    begin
      for Col of Clq.Columns loop
        Tmp.Reset;
        Tmp.Set("          " & Clreq_Name.Lower_Case & "." & Col.Name.Lower_Case & " = ");
        case Data_Type(Col.Type_Of) is
          when A_Char        => Tmp.Append("Convert.ToString(row[" & Quote(Col.Name.Lower_Case) & "]);");
          when A_Int         => Tmp.Append("Convert.ToInt32(row[" & Quote(Col.Name.Lower_Case) & "]);");
          when A_Double      => Tmp.Append("Convert.ToDouble(row[" & Quote(Col.Name.Lower_Case) & "]);");
          when A_Boolean     => Tmp.Append("Convert.ToInt32(row[" & Quote(Col.Name.Lower_Case) & "]);");
          when A_Short_Code  => Tmp.Append("Convert.ToInt32(row[" & Quote(Col.Name.Lower_Case) & "]);");
          when A_Date        => Tmp.Append("Convert.ToDateTime(row[" & Quote(Col.Name.Lower_Case) & "]);");
          when A_Time        => Tmp.Append("Convert.ToDateTime(row[" & Quote(Col.Name.Lower_Case) & "]);");
          when A_Timestamp   => Tmp.Append("Convert.ToDateTime(row[" & Quote(Col.Name.Lower_Case) & "]);");
          when others        => Tmp.Append("bad datatype " &  Data_Type(Col.Type_Of)'Img & ";");
        end case;
        Put_Line(Tmp.Fix_String);
      end loop;
    end;

    Put_Line("        }");
    Put_Line("        return true;");
    Put_Line("      }");
    Put_Line("      return false;");
    Put_Line("    }");
    Put_Line("    public static void GetXmlReply(ProClass ps, XmlNode serviceReplyNode, ref DataTable dt, ref string status)");
    Put_Line("    {");
    Put_Line("      status = m2SattmateServices.Common.GetStatusAttribute(serviceReplyNode);");
    Put_Line("      XmlNode ListNode = serviceReplyNode[" & Quote("List") & "];");
    Put_Line("      if (ListNode == null) return;");
    Put_Line("      dt = new DataTable();");
    Put_Line("      NumberFormatInfo numberInfo = CultureInfo.InvariantCulture.NumberFormat;");
    Put_Line("      foreach (FieldInfo field in fields)");
    Put_Line("      {");
    Put_Line("        dt.Columns.Add(field.Name.ToLower(), field.FieldType);");
    Put_Line("      }");
    Put_Line("      // Get all children to <List>");
    Put_Line("      XmlNodeList ItemList = ListNode.ChildNodes;");
    Put_Line("      // Loop all children to <List>");
    Put_Line("      foreach (XmlNode ChildNode in ItemList)");
    Put_Line("      {");
    Put_Line("        //Handle only 'Item' elements");
    Put_Line("        if (ChildNode.Name == " & Quote("Item") & ")");
    Put_Line("        {");
    Put_Line("          DataRow r;");
    Put_Line("          r = dt.NewRow();");
    Put_Line("          int i = 0;");
    Put_Line("          foreach (FieldInfo field in fields)");
    Put_Line("          {");
    Put_Line("            Type t = field.FieldType;");
    Put_Line("            if (t.Equals(typeof(System.Double)))");
    Put_Line("              r[i] = Convert.ToDouble(ps.Rpc.GetChildElementValue(ChildNode, field.Name.ToUpper()), numberInfo);");
    Put_Line("            else");
    Put_Line("              r[i] = ps.Rpc.GetChildElementValue(ChildNode, field.Name.ToUpper());");
    Put_Line("            i++;");
    Put_Line("          }");
    Put_Line("          dt.Rows.Add(r);");
    Put_Line("        }");
    Put_Line("      }");
    Put_Line("    }");
    Put_Line("  }");
    Put_Line("}");

    Clq.Reset;

  end Make_C_Sharp_Class;
  -----------------------------------------------------------------


  procedure Make_M2_Setup_Separates(Self : in out Config_Type) is
    use Text_Io;
    All_Tables : String_Object := String_Object(Self.All_Entities_Defined_Names(Tables));
    -- All_Clreqs : String_Object := String_Object(Self.All_Entities_Defined_Names(Clreqs));

    ---------------------------------------------------------------------
    procedure Print_Header_Body is
    begin
      Code_Debug("  -- start Make_M2_Setup_Separates.Print_Header_Body ");

      Put_Line(Header.Fix_String);
      Put_Line("");

      Code_Debug("  -- stop  Make_M2_Setup_Separates.Print_Header_Body ");
    end Print_Header_Body;
    ---------------------------------------------------------------------
    procedure Print_Withs_Body(Table_Names : String_Object) is
      use Gnat;
      Subs : String_Split.Slice_Set;
      Seps : constant String := " ";
    begin
      Code_Debug("  -- start Make_M2_Setup_Separates.Print_Withs_Body ");

      Put_Line("with Sattmate_Calendar;");
      Put_Line("");

      String_Split.Create (S          => Subs,
                           From       => Table_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each clreq
      for j in 1 .. String_Split.Slice_Count(Subs) loop
        declare
          Name : String  := String_Split.Slice(Subs, j);
          Tmp  : String_Object;
        begin
          Tmp.Set(Name);
          Put_Line("with Table_" & Tmp.Camel_Case & ";");
        end;
      end loop;

      Put_Line("");

      Code_Debug("  -- stop  Make_M2_Setup_Separates.Print_Withs_Body ");
    end Print_Withs_Body;
    ---------------------------------------------------------------------
    procedure Print_Package_Start_Body is
    begin
      Code_Debug("  -- start Make_M2_Setup_Separates.Print_Package_Start_Body ");

      Put_Line("  separate (M2_Setup)");

      Code_Debug("  -- stop  Make_M2_Setup_Separates.Print_Package_Start_Body ");
    end Print_Package_Start_Body;
    ---------------------------------------------------------------------
    procedure Print_Add_Body(Table_Names : String_Object) is
      use Gnat;
      First_Time : Boolean := True;
      Subs : String_Split.Slice_Set;
      Seps : constant String := " ";
    begin
      Code_Debug("  -- start Make_M2_Setup_Separates.Print_Add_Body ");

      Put_Line("  procedure    Add_Record          (Request        : in     Sattmate_Xml.Document;");
      Put_Line("                                    Reply          : in out Sattmate_Xml.Document;");
      Put_Line("                                    Service        : in     String) is");
      Put_Line("    use Sattmate_Xml;");
      Put_Line("    use M2_Utils;");
      Put_Line("    Local_Base    : constant String := Global_Base & " & Quote("/") & " & Service;");
      Put_Line("    Local_Table   : constant String := Local_Base & " & Quote("/Table") & ";");
      Put_Line("    Table_Node    : Node := First_Node(Expr    => Local_Table ,");
      Put_Line("                                       Doc     => Request); ");
      Put_Line("    Table_Name    : String := Get_Attribute(Table_Node, Name);");
      Put_Line("    Result        : M2_Replies.Error_Type := M2_Replies.Mg_Bad;");
      Put_Line("    Transaction   : Sql.Transaction_Type;");
      Put_Line("  begin");
      Put_Line("    if not Is_Allowed(Request, Service) then");
      Put_Line("      M2_Replies.Error(Service, Reply, M2_Replies.Mg_Bad);");
      Put_Line("      return;");
      Put_Line("    end if;");
      Put_Line("");

      String_Split.Create (S          => Subs,
                           From       => Table_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each table
      for j in 1 .. String_Split.Slice_Count(Subs) loop
        declare
          Name : String  := String_Split.Slice(Subs, j);
          Tmp  : String_Object;
        begin
          Tmp.Set(Name);
          Put_Line(" -- " & Tmp.Camel_Case & " --");
          if First_Time then
            Put_Line("    if    Check(Table_" & Tmp.Camel_Case & ".Table_" & Tmp.Camel_Case & "_Name, Table_Name, Table_Name'Length) then");
            First_Time := False;
          else
            Put_Line("    elsif Check(Table_" & Tmp.Camel_Case & ".Table_" & Tmp.Camel_Case & "_Name, Table_Name, Table_Name'Length) then");
          end if;
          Put_Line("          declare");
          Put_Line("            Data : Table_" & Tmp.Camel_Case & ".Data_Type;");
          Put_Line("          begin");
          Put_Line("            Data := M2_Convert_Xml.Extract_All(Request, Local_Table);");
          Put_Line("            Transaction.Start;");
          Put_Line("            Data.Insert;");
          Put_Line("            Transaction.Commit;");
          Put_Line("            Result := M2_Replies.Mg_Ok;");
          Put_Line("          exception");
          Put_Line("            when Sql.Duplicate_Index =>");
          Put_Line("              Transaction.Rollback;");
          Put_Line("          end;");
        end;
      end loop;
      Put_Line("    else");
      Put_Line("      M2_Replies.Unknown_Service(Service & " & Quote("-") & " & Table_Name, Reply);");
      Put_Line("      return;");
      Put_Line("    end if;");
      Put_Line("    M2_Replies.Standard_Reply(Service, Reply, Result);");
      Put_Line("  end Add_Record;");

      Put_Line("");

      Code_Debug("  -- stop  Make_M2_Setup_Separates.Print_Add_Body ");
    end Print_Add_Body;
    ---------------------------------------------------------------------
    procedure Print_Delete_Body(Table_Names : String_Object) is
      use Gnat;
      First_Time : Boolean := True;
      Subs : String_Split.Slice_Set;
      Seps : constant String := " ";
    begin
      Code_Debug("  -- start Make_M2_Setup_Separates.Print_Delete_Body ");

      Put_Line("  procedure    Delete_Record      (Request        : in     Sattmate_Xml.Document;");
      Put_Line("                                   Reply          : in out Sattmate_Xml.Document;");
      Put_Line("                                   Service        : in     String) is");
      Put_Line("    use Sattmate_Xml;");
      Put_Line("    use M2_Utils;");
      Put_Line("    use type Sattmate_Calendar.Time_Type;");
      Put_Line("    Local_Base    : constant String := Global_Base & " & Quote("/") & " & Service;");
      Put_Line("    Local_Table   : constant String := Local_Base & " & Quote("/Table") & ";");
      Put_Line("    Table_Node    : Node := First_Node(Expr    => Local_Table ,");
      Put_Line("                                       Doc     => Request); ");
      Put_Line("    Table_Name    : String := Get_Attribute(Table_Node, Name);");
      Put_Line("    Result        : M2_Replies.Error_Type := M2_Replies.Mg_Bad;");
      Put_Line("    Loop_Counter  : Integer_4 := 0;");
      Put_Line("    Transaction   : Sql.Transaction_Type;");
      Put_Line("  begin");
      Put_Line("    if not Is_Allowed(Request, Service) then");
      Put_Line("      M2_Replies.Error(Service, Reply, M2_Replies.Mg_Bad);");
      Put_Line("      return;");
      Put_Line("    end if;");

      Put_Line("");
      String_Split.Create (S          => Subs,
                           From       => Table_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each table
      for j in 1 .. String_Split.Slice_Count(Subs) loop
        declare
          Name : String  := String_Split.Slice(Subs, j);
          Tbl  : Repository.Table.Table_Type with Warnings => Off;
        begin
          Tbl.Name.Set(Name);
          Tbl.Create(Self);
          Put_Line("");
          Put_Line("");
          Put_Line("    -- " & Tbl.Name.Camel_Case & " --");
          Put_Line("");
          if First_Time then
            Put_Line("    if    Check(Table_" & Tbl.Name.Camel_Case & ".Table_" & Tbl.Name.Camel_Case & "_Name, Table_Name, Table_Name'Length) then");
            First_Time := False;
          else
            Put_Line("    elsif Check(Table_" & Tbl.Name.Camel_Case & ".Table_" & Tbl.Name.Camel_Case & "_Name, Table_Name, Table_Name'Length) then");
          end if;
          Put_Line("      loop");
          Put_Line("        declare");
          Put_Line("          Data     : Table_" & Tbl.Name.Camel_Case & ".Data_Type;");
          Put_Line("          Eos      : Boolean := False;");
          Put_Line("        begin");
          Put_Line("          Data := M2_Convert_Xml.Extract_All(Request, Local_Table);");
          Put_Line("          Transaction.Start;");
          Put_Line("          Data.Read(Eos);");
          Put_Line("          if not Eos then");
          Put_Line("            M2_Convert_Xml.Extract_Data(Request, Local_Table, Data);");

          case Tbl.Ixx_Type is
            when Table.None      => Put_Line("            Data.Delete;  --No Delete_Withcheck in " & Tbl.Name.Camel_Case);
            when Table.Date_Time |
                 Table.Timestamp => Put_Line("            Data.Delete_Withcheck;");
          end case;

          Put_Line("            Result := M2_Replies.Mg_Ok;");
          Put_Line("          else");
          Put_Line("            --somebody else deleted this record, when we were fiddling with it!");
          Put_Line("            Result := M2_Replies.Mg_Ok;  -- for delete that is ok");
          Put_Line("          end if;");
          Put_Line("          Transaction.Commit;");
          Put_Line("          exit;");
          Put_Line("        exception");
          Put_Line("          when Sql.Transaction_Conflict | Sql.No_Such_Row =>");
          Put_Line("            Transaction.Rollback;");
          Put_Line("            Result := M2_Replies.Mg_Bad;");
          Put_Line("            Loop_Counter := Loop_Counter +1;");
          Put_Line("            exit when Loop_Counter >= M2_Utils.Trf_Conflict_Max_Loops;");
          Put_Line("            delay M2_Utils.Trf_Conflict_Delay;");
          Put_Line("        end;");
          Put_Line("      end loop;");
          Tbl.Reset;
        end;
      end loop;

      Put_Line("    else");
      Put_Line("      M2_Replies.Unknown_Service(Service & " & Quote("-") & " & Table_Name, Reply);");
      Put_Line("      return;");
      Put_Line("    end if;");
      Put_Line("    M2_Replies.Standard_Reply(Service, Reply, Result);");
      Put_Line("  end Delete_Record;");


      Code_Debug("  -- stop  Make_M2_Setup_Separates.Print_Delete_Body ");
    end Print_Delete_Body;
    ---------------------------------------------------------------------
    procedure Print_Update_Body(Table_Names : String_Object) is
      use Gnat;
      First_Time : Boolean := True;
      Subs : String_Split.Slice_Set;
      Seps : constant String := " ";
    begin
      Code_Debug("  -- start Make_M2_Setup_Separates.Print_Update_Body ");
      Put_Line("  procedure    Update_Record      (Request        : in     Sattmate_Xml.Document;");
      Put_Line("                                   Reply          : in out Sattmate_Xml.Document;");
      Put_Line("                                   Service        : in     String) is");
      Put_Line("    use Sattmate_Xml;");
      Put_Line("    use M2_Utils;");
      Put_Line("    use type Sattmate_Calendar.Time_Type;");
      Put_Line("    Local_Base    : constant String := Global_Base & " & Quote("/") & " & Service;");
      Put_Line("    Local_Table   : constant String := Local_Base & " & Quote("/Table") & ";");
      Put_Line("    Table_Node    : Node := First_Node(Expr    => Local_Table ,");
      Put_Line("                                       Doc     => Request); ");
      Put_Line("    Table_Name    : String := Get_Attribute(Table_Node, Name);");
      Put_Line("    Result        : M2_Replies.Error_Type := M2_Replies.Mg_Bad;");
      Put_Line("    Loop_Counter  : Integer_4 := 0;");
      Put_Line("    Transaction   : Sql.Transaction_Type;");
      Put_Line("  begin");
      Put_Line("    if not Is_Allowed(Request, Service) then");
      Put_Line("      M2_Replies.Error(Service, Reply, M2_Replies.Mg_Bad);");
      Put_Line("      return;");
      Put_Line("    end if;");

      Put_Line("");
      String_Split.Create (S          => Subs,
                           From       => Table_Names.Camel_Case,
                           Separators => Seps,
                           Mode       => String_Split.Multiple);

      -- for each table
      for j in 1 .. String_Split.Slice_Count(Subs) loop
        declare
          Name : String  := String_Split.Slice(Subs, j);
          Tbl  : Repository.Table.Table_Type with Warnings => Off;
        begin
          Tbl.Name.Set(Name);
          Tbl.Create(Self);
          Put_Line("");
          Put_Line("");
          Put_Line("    -- " & Tbl.Name.Camel_Case & " --");
          Put_Line("");
          if First_Time then
            Put_Line("    if    Check(Table_" & Tbl.Name.Camel_Case & ".Table_" & Tbl.Name.Camel_Case & "_Name, Table_Name, Table_Name'Length) then");
            First_Time := False;
          else
            Put_Line("    elsif Check(Table_" & Tbl.Name.Camel_Case & ".Table_" & Tbl.Name.Camel_Case & "_Name, Table_Name, Table_Name'Length) then");
          end if;

          if Tbl.All_Columns_Are_Primary then
            Put_Line("    --Since this table consist of just keys, no update is possible!");
            Put_Line("      Result := M2_Replies.Mg_Bad;");
          else
            Put_Line("      loop");
            Put_Line("        declare");
            Put_Line("          Data     : Table_" & Tbl.Name.Camel_Case & ".Data_Type;");
            Put_Line("          Eos      : Boolean := False;");
            Put_Line("        begin");
            Put_Line("          Data := M2_Convert_Xml.Extract_All(Request, Local_Table);");
            Put_Line("          Transaction.Start;");
            Put_Line("          Data.Read(Eos);");
            Put_Line("          if not Eos then");
            Put_Line("            M2_Convert_Xml.Extract_Data(Request, Local_Table, Data);");

            case Tbl.Ixx_Type is
              when Table.None      => Put_Line("            Data.Update;  --No Update_Withcheck in " & Tbl.Name.Camel_Case);
              when Table.Date_Time |
                   Table.Timestamp => Put_Line("            Data.Update_Withcheck;");
            end case;

            Put_Line("            Result := M2_Replies.Mg_Ok;");
            Put_Line("          else");
            Put_Line("            --somebody else deleted this record, when we were fiddling with it!");
            Put_Line("            Result := M2_Replies.Mg_Bad;");
            Put_Line("          end if;");
            Put_Line("          Transaction.Commit;");
            Put_Line("          exit;");
            Put_Line("        exception");
            Put_Line("          when Sql.Transaction_Conflict | Sql.No_Such_Row =>");
            Put_Line("            Transaction.Rollback;");
            Put_Line("            Result := M2_Replies.Mg_Bad;");
            Put_Line("            Loop_Counter := Loop_Counter +1;");
            Put_Line("            exit when Loop_Counter >= M2_Utils.Trf_Conflict_Max_Loops;");
            Put_Line("            delay M2_Utils.Trf_Conflict_Delay;");
            Put_Line("        end;");
            Put_Line("      end loop;");
          end if;
          Tbl.Reset;
        end;
      end loop;

      Put_Line("    else");
      Put_Line("      M2_Replies.Unknown_Service(Service & " & Quote("-") & " & Table_Name, Reply);");
      Put_Line("      return;");
      Put_Line("    end if;");
      Put_Line("    M2_Replies.Standard_Reply(Service, Reply, Result);");
      Put_Line("  end Update_Record;");

      Code_Debug("  -- stop  Make_M2_Setup_Separates.Print_Update_Body ");
    end Print_Update_Body;
    ---------------------------------------------------------------------

  begin
    Code_Debug("  -- start Make_M2_Setup_Separates ");

    Print_Header_Body;
    Print_Withs_Body(All_Tables);
    Print_Package_Start_Body;
    Print_Add_Body(All_Tables);

    Print_Header_Body;
    Print_Withs_Body(All_Tables);
    Print_Package_Start_Body;
    Print_Delete_Body(All_Tables);

    Print_Header_Body;
    Print_Withs_Body(All_Tables);
    Print_Package_Start_Body;
    Print_Update_Body(All_Tables);

    Code_Debug("  -- stop  Make_M2_Setup_Separates ");

  end Make_M2_Setup_Separates;

  -----------------------------------------------------------------



  function Quote(What : String) return String is
  begin
    return '"' & What & '"';
  end Quote;
  -----------------------------------------------------------------

  procedure Set_Debug_Level(Level : Integer_4) is
  begin
    Global_Debug_Level := Level;
  end Set_Debug_Level;

  -----------------------------------------------------------------

end Repository;
