with Ada.Environment_Variables;
with Ada.Directories;
--with Ada.Characters.Handling;
with Text_Io;

--with GNAT.String_Split;

with Sax;
with Sax.Readers;        use Sax.Readers;
with Sax.Attributes;

with Input_Sources.File; use Input_Sources.File;
with Unicode.CES;

with Utils;
with Sql;

with Repository.Table;



package body Repository.View is


  Tag_View           : constant String := "View";
  Attr_Name          : constant String := "Name";
  Attr_Type          : constant String := "Type";

  Tag_Description    : constant String := "Description";

--  Tag_Columns        : constant String := "Columns";
  Tag_Table          : constant String := "Table";

  Tag_Column         : constant String := "Column";
  Attr_As            : constant String := "As";
  Attr_As_Oracle      : constant String := "AsOracle";
  Attr_As_SqlServer   : constant String := "AsSqlServer";
  Attr_As_Postgresql  : constant String := "AsPostgresql";
  Attr_Key           : constant String := "Key";

  Tag_From           : constant String := "From";
  Tag_From_Oracle     : constant String := "FromOracle";
  Tag_From_SqlServer  : constant String := "FromSqlServer";
  Tag_From_Postgresql : constant String := "FromPostgresql";


  type Reader is new Sax.Readers.Reader with record
    View         : View_Type;
    Current_Tag  : Unbounded_String := Null_Unbounded_String;
    Config       : Config_Type;
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

  ------------------------------------------------

  function Column_List_String( List : Columns_Type) return String is
    Tmp : String_Object;
  begin
    if not List.Is_Empty then
      for Col of List loop
        Tmp.Set(Tmp.Fix_String & Col.To_String & " " & Ascii.Lf);
      end loop;
      return Ascii.Lf & Tmp.Fix_String;
    else
      return "";
    end if;
  end Column_List_String;
  ------------------------------------------------
  overriding
  function To_String(Self : View_Column_Type ) return String is
  begin
    return C.Column_Type(Self).To_String &
           Self.Primary'Img;
  end To_String;

  ------------------------------------------------
  overriding
  procedure Reset(Self : in out View_Column_Type ) is
  begin
    C.Column_Type(Self).Reset; -- cast to parenttype and use that reset
    Self.As.Reset;
    Self.As_Oracle.Reset;
    Self.As_SqlServer.Reset;
    Self.As_Postgresql.Reset;
  end Reset;


  ------------------------------------------------
  overriding
  function To_String(Self : View_Type) return String is
  begin
    return Self.Name.Fix_String & " " &
           Self.Type_Of'Img & " " &
           Self.Description.Fix_String & " " &
           Column_List_String(Self.Columns) & " " &
           Self.From.Fix_String & Ascii.Lf &
           Self.From_Oracle.Fix_String & Ascii.Lf &
           Self.From_SqlServer.Fix_String & Ascii.Lf &
           Self.From_Postgresql.Fix_String ;
  end To_String;
  ---------------------------------------------------------

  procedure Create(Self : in out View_Type ; Config : Repository.Config_Type'class) is
    My_Reader    : Reader;
    Input        : File_Input;
  begin
    if Self.Name.UBString = Null_Unbounded_String then
      raise Sequence_Error with "View name must be set before calling Create";
    end if;
    Debug("View create: '" & Self.Name.Fix_String & "'");

    My_Reader.View  := Self;
    My_Reader.Config := Repository.Config_Type(Config);

    declare
      Filename : String := Utils.Lower_Case(
                               Config.Item(Views).Directory.Fix_String & "/"  &
                               Config.Item(Views).Prefix.Fix_String & "_" & Self.Name.Fix_String & ".xml");
      Xsdname  : String := Ada.Environment_Variables.Value(Name => "BOT_CONFIG") & "/repository/view.xsd";
      Is_Valid : Boolean := False;
    begin
      -- check doc is valid
      Is_Valid := Validate(Xml => Filename, Xsd => Xsdname);
      if not Is_Valid then
        raise Configuration_Error with "not valid xml: '" & Filename & "'";
      end if;
      -- parse it and use it since it is ok
      Open(Filename, Input);
      My_Reader.Set_Feature(Validation_Feature,False);
      My_Reader.Parse(Input);
      Close(Input);
    end;
    Self := My_Reader.View;
    Self.Is_Initialized := True;
  end Create;

  -------------------------------------------------------------------------------------
  procedure Create(Self : in out View_Type ; Name : String; Config : Repository.Config_Type'class) is
  begin
    Self.Name.Set(Name);
    Self.Create(Config);
  end Create;

  -------------------------------------------------------------------------------------

  overriding
  procedure Reset(Self : in out View_Type) is
  begin
    Self.Name.Reset;
    Self.Type_Of := View_Type_Type'First;
    Self.Description.Reset;

    for Col of Self.Columns loop
      Col.Reset;
    end loop;
    Self.Columns.Clear;

    Self.From.Reset;
    Self.From_Oracle.Reset;
    Self.From_SqlServer.Reset;
    Self.From_Postgresql.Reset;
  end Reset;

  -------------------------------------------------------------------------------------
  overriding procedure Start_Element(Handler       : in out Reader;
                                     Namespace_Uri : Unicode.Ces.Byte_Sequence := "";
                                     Local_Name    : Unicode.Ces.Byte_Sequence := "";
                                     Qname         : Unicode.Ces.Byte_Sequence := "";
                                     Atts          : Sax.Attributes.Attributes'Class) is
    pragma Unreferenced(Namespace_Uri);
    pragma Unreferenced(Qname);
    The_Tag : constant String := Local_Name;
  begin
    Handler.Current_Tag := To_Unbounded_String(The_Tag);
--    Feedback("Start_Element " & The_Tag );

    if The_Tag = Tag_View then
      Handler.View.Name.Set(Atts.Get_Value(Attr_Name));
      Handler.View.Type_Of := View_Type_Type'Value(Atts.Get_Value(Attr_Type));

    elsif The_Tag = Tag_Description then
      -- get data in characters
       Handler.View.Description.Reset;

    elsif The_Tag = Tag_Table then

      declare
        Tbl          : Repository.Table.Table_Type;
        Is_Duplicate : Boolean := False;
        Dummy        : String_Object;
      begin
        Tbl.Name.Set(Atts.Get_Value(Attr_Name));
        Tbl.Create(Handler.Config);

        -- check that columns of the table is not added before
        for Table_Col of Tbl.Columns loop
          Is_Duplicate := False;
          for View_Col of Handler.View.Columns loop
            if Table_Col.Name.Lower_Case = View_Col.Name.Lower_Case then
              Is_Duplicate := True;
              exit;
            end if;
          end loop;
          if not Is_Duplicate then
            declare
              Tmp_Col : View_Column_Type;
            begin
              Tmp_Col.Name.Set(Table_Col.Name.Fix_String);
              Tmp_Col.Create(Handler.Config);
              Tmp_Col.Original_Table.Set(Tbl.Name.Camel_Case);
              Handler.View.Columns.Append(Tmp_Col);
              Tmp_Col.Reset;
            end;
          end if;
        end loop;
        Tbl.Reset;
      end;

    elsif The_Tag = Tag_Column then

      declare
        Col : View_Column_Type;
        Is_Duplicate : Boolean := False;
      begin
        Col.Name.Set(Atts.Get_Value(Attr_Name));
        Col.Create(Handler.Config);
        if Atts.Get_Index(Attr_Key) >-1 then   -- check for attribute's existence
          Col.Primary := Atts.Get_Value(Attr_Key) = "P";
        else
          Col.Primary := False;
        end if;

        if Atts.Get_Index(Attr_As) >-1 then   -- check for attribute's existence
          Col.As.Set(Atts.Get_Value(Attr_As));
        end if;

        if Atts.Get_Index(Attr_As_Oracle) >-1 then   -- check for attribute's existence
          Col.As_Oracle.Set(Atts.Get_Value(Attr_As_Oracle));
        end if;

        if Atts.Get_Index(Attr_As_SqlServer) >-1 then   -- check for attribute's existence
          Col.As_SqlServer.Set(Atts.Get_Value(Attr_As_SqlServer));
        end if;

        if Atts.Get_Index(Attr_As_Postgresql) >-1 then   -- check for attribute's existence
          Col.As_Postgresql.Set(Atts.Get_Value(Attr_As_Postgresql));
        end if;

        -- check that columns of the table is not added before
        for View_Col of Handler.View.Columns loop
          if Col.Name.Lower_Case = View_Col.Name.Lower_Case then
            Is_Duplicate := True;
            exit;
          end if;
        end loop;
        if not Is_Duplicate then
          Handler.View.Columns.Append(Col);
        end if;
      end;

    elsif The_Tag = Tag_From then
      -- get data in characters
       Handler.View.From.Reset;
    elsif The_Tag = Tag_From_Oracle then
      -- get data in characters
       Handler.View.From_Oracle.Reset;
    elsif The_Tag = Tag_From_SqlServer then
      -- get data in characters
       Handler.View.From_SqlServer.Reset;
    elsif The_Tag = Tag_From_Postgresql then
      -- get data in characters
       Handler.View.From_Postgresql.Reset;

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
    The_Tag : constant String := Local_Name;
  begin

    null;

  exception
    when Ada.Strings.Length_Error =>
    	Feedback("Ada.Strings.Length_Error on Tag '" & The_Tag & "'");
  end End_Element;
  --++--++--++--++--++--++--++--++--++--++--++--++--++--

  overriding procedure Characters(Handler : in out Reader;
                                  Ch      : Unicode.Ces.Byte_Sequence := "") is
    The_Tag   : constant String := To_String(Handler.Current_Tag);
    The_Value : constant String := Utils.Expand_File_Path(To_Iso_Latin_15(Ch));
  begin
--    Feedback("Characters " & The_Tag & " -> '" & The_Value & "'");
    if The_Tag = Tag_Description then
      Handler.View.Description.Append(The_Value);
    elsif The_Tag = Tag_From then
      Handler.View.From.Append(The_Value);
    elsif The_Tag = Tag_From_Oracle then
      Handler.View.From_Oracle.Append(The_Value);
    elsif The_Tag = Tag_From_SqlServer then
      Handler.View.From_SqlServer.Append(The_Value);
    elsif The_Tag = Tag_From_Postgresql then
      Handler.View.From_Postgresql.Append(The_Value);
    end if;

  exception
    when Ada.Strings.Length_Error =>
      Feedback("Ada.Strings.Length_Error on Tag '" & The_Tag & "' with data '" & The_Value & "'");
  end Characters;
  --------------------------------------------------------------------

  procedure Set_Name(Self : in out View_Type; Filename : String; Config : Repository.Config_Type'class) is
    use Ada.Directories;
    Basename : String  := Base_Name(Filename);
    Pos      : Integer := 0;
    Prefix   : String  := Config.Item(Views).Prefix.Fix_String;
  begin
   -- Feedback("basename: " & Basename);
    Pos := Utils.Position(Basename, Prefix );
   -- Feedback("Pos: " & Pos'Img);
   -- Feedback("Prefix'Last: " & Prefix'Last'Img);
    if Pos /= Basename'First -1 then
     -- Feedback("Filename '" & Basename(Prefix'Last + 2 .. Basename'Last) & "'");
      Self.Name.Set(Basename(Prefix'Last + 2 .. Basename'Last));
    else
      raise Configuration_Error with "cannot set name from file '" & Filename & "'";
    end if;
  end Set_Name;

  --------------------------------------------------------------------
  procedure Print_Oracle_Create_DDL(Self : in out View_Type) is
    use Text_Io;
    View_Def : String_Object;
    type As_Source_Type is (None, As, As_Oracle, As_SqlServer, As_Postgresql);
    As_Data_Source : As_Source_Type := As_Source_Type'First;

    type From_Source_Type is (From, From_Oracle, From_SqlServer, From_Postgresql);
    From_Source : From_Source_Type := From_Source_Type'First;

  begin
    Put_Line("");
    Put_Line("prompt 'creating view " & Self.Name.Fix_String & "';");
    Put_Line("create or replace view " & Self.Name.Fix_String & " ( " );

    for Col of Self.Columns loop
      View_Def.Append("  " & Col.Name.Upper_Case & "," & Ascii.Lf);
    end loop;
    -- kill last  "," & Ascii.Lf
    View_Def.Delete_Last_Char;
    View_Def.Delete_Last_Char;
    View_Def.Append(")");
    Put_Line(View_Def.Fix_String);

    View_Def.Reset;
    View_Def.Append("as select " & Ascii.Lf);

    for Col of Self.Columns loop
      As_Data_Source := None; -- start with assumption no extra info

      if Col.As.Fix_String /= "" then
        As_Data_Source := As;  -- general db info
      end if;
      case Sql.Database is
        when Sql.Postgresql =>
          if Col.As_Postgresql.Fix_String /= "" then
            As_Data_Source := As_Postgresql;  -- specialized db info
          end if;
      end case;

      case As_Data_Source is  --Self.Original_Table.Upper_Case is empty if not from 'Table' element
        when None          =>
          if Col.Original_Table.Upper_Case /= "" then
            View_Def.Append("  " & Col.Original_Table.Upper_Case & "." & Col.Name.Upper_Case);
          else
            View_Def.Append("  " & Col.Name.Upper_Case);
          end if;
        when As            => View_Def.Append("  " & Col.As.Upper_Case);
        when As_Oracle     => View_Def.Append("  " & Col.As_Oracle.Upper_Case);
        when As_SqlServer  => View_Def.Append("  " & Col.As_SqlServer.Upper_Case);
        when As_Postgresql => View_Def.Append("  " & Col.As_Postgresql.Upper_Case);
      end case;
      View_Def.Append("," & Ascii.Lf);
    end loop;
    -- kill last  "," & Ascii.Lf
    View_Def.Delete_Last_Char;
    View_Def.Delete_Last_Char;

    Put_Line(View_Def.Fix_String);
    View_Def.Reset;

    From_Source := From; -- start with assumption no extra info
    case Sql.Database is
      when Sql.Postgresql =>
        if Self.From_Postgresql.Fix_String /= "" then
          From_Source := From_Postgresql;  -- specialized db info
        end if;
    end case;

    View_Def.Set("from");
    case From_Source is
      when From            => View_Def.Append("  " & Self.From.Fix_String);
      when From_Oracle     => View_Def.Append("  " & Self.From_Oracle.Fix_String);
      when From_SqlServer  => View_Def.Append("  " & Self.From_SqlServer.Fix_String);
      when From_Postgresql => View_Def.Append("  " & Self.From_Postgresql.Fix_String);
    end case;

    Put_Line(View_Def.Fix_String);
    Put_Line("/");
    Put_Line("");

  end Print_Oracle_Create_DDL;
  --------------------------------------------------------------------
  procedure Print_Sql_Server_Create_DDL(Self : in out View_Type) is
    use Text_Io;
    View_Def : String_Object;
    type As_Source_Type is (None, As, As_Oracle, As_SqlServer, As_Postgresql);
    As_Data_Source : As_Source_Type := As_Source_Type'First;

    type From_Source_Type is (From, From_Oracle, From_SqlServer, From_Postgresql);
    From_Source : From_Source_Type := From_Source_Type'First;

  begin
    Put_Line("if exists (select TABLE_NAME from INFORMATION_SCHEMA.VIEWS");
    Put_Line("where TABLE_NAME = '" & Self.Name.Upper_Case & "') drop view " & Self.Name.Fix_String );
    Put_Line("go");
    Put_Line("");

    Put_Line("create view " & Self.Name.Fix_String & " ( " );

    for Col of Self.Columns loop
      View_Def.Append("  " & Col.Name.Upper_Case & "," & Ascii.Lf);
    end loop;
    -- kill last  "," & Ascii.Lf
    View_Def.Delete_Last_Char;
    View_Def.Delete_Last_Char;
    View_Def.Append(")");
    Put_Line(View_Def.Fix_String);

    View_Def.Reset;
    View_Def.Append("as select " & Ascii.Lf);

    for Col of Self.Columns loop
      As_Data_Source := None; -- start with assumption no extra info

      if Col.As.Fix_String /= "" then
        As_Data_Source := As;  -- general db info
      end if;
      case Sql.Database is
        when Sql.Postgresql =>
          if Col.As_Postgresql.Fix_String /= "" then
            As_Data_Source := As_Postgresql;  -- specialized db info
          end if;
      end case;

      case As_Data_Source is
        when None          =>
          if Col.Original_Table.Upper_Case /= "" then
            View_Def.Append("  " & Col.Original_Table.Upper_Case & "." & Col.Name.Upper_Case);
          else
            View_Def.Append("  " & Col.Name.Upper_Case);
          end if;
        when As            => View_Def.Append("  " & Col.As.Upper_Case);
        when As_Oracle     => View_Def.Append("  " & Col.As_Oracle.Upper_Case);
        when As_SqlServer  => View_Def.Append("  " & Col.As_SqlServer.Upper_Case);
        when As_Postgresql => View_Def.Append("  " & Col.As_Postgresql.Upper_Case);
      end case;
      View_Def.Append("," & Ascii.Lf);
    end loop;
    -- kill last  "," & Ascii.Lf
    View_Def.Delete_Last_Char;
    View_Def.Delete_Last_Char;

    Put_Line(View_Def.Fix_String);
    View_Def.Reset;

    From_Source := From; -- start with assumption no extra info
    case Sql.Database is
      when Sql.Postgresql =>
        if Self.From_Postgresql.Fix_String /= "" then
          From_Source := From_Postgresql;  -- specialized db info
        end if;
    end case;

    View_Def.Set("from");
    case From_Source is
      when From            => View_Def.Append("  " & Self.From.Fix_String);
      when From_Oracle     => View_Def.Append("  " & Self.From_Oracle.Fix_String);
      when From_SqlServer  => View_Def.Append("  " & Self.From_SqlServer.Fix_String);
      when From_Postgresql => View_Def.Append("  " & Self.From_Postgresql.Fix_String);
    end case;

    Put_Line(View_Def.Fix_String);
    Put_Line("go");
    Put_Line("");

  end Print_Sql_Server_Create_DDL;
  --------------------------------------------------------------------
  procedure Print_Postgresql_Create_DDL(Self : in out View_Type) is
    use Text_Io;
    View_Def : String_Object;
    type As_Source_Type is (None, As, As_Oracle, As_SqlServer, As_Postgresql);
    As_Data_Source : As_Source_Type := As_Source_Type'First;

    type From_Source_Type is (From, From_Oracle, From_SqlServer, From_Postgresql);
    From_Source : From_Source_Type := From_Source_Type'First;

  begin
    Put_Line("");
    Put_Line("begin;");
    Put_Line("drop view if exists " & Self.Name.Fix_String & ";" );
    Put_Line("commit;");

    Put_Line("begin;");
    Put_Line("create view " & Self.Name.Fix_String & " ( " );

    for Col of Self.Columns loop
      View_Def.Append("  " & Col.Name.Upper_Case & "," & Ascii.Lf);
    end loop;
    -- kill last  "," & Ascii.Lf
    View_Def.Delete_Last_Char;
    View_Def.Delete_Last_Char;
    View_Def.Append(")");
    Put_Line(View_Def.Fix_String);

    View_Def.Reset;
    View_Def.Append("as select " & Ascii.Lf);

    for Col of Self.Columns loop
      As_Data_Source := None; -- start with assumption no extra info

      if Col.As.Fix_String /= "" then
        As_Data_Source := As;  -- general db info
      end if;
      case Sql.Database is
        when Sql.Postgresql =>
          if Col.As_Postgresql.Fix_String /= "" then
            As_Data_Source := As_Postgresql;  -- specialized db info
          end if;
      end case;

      case As_Data_Source is  --Self.Original_Table.Upper_Case is empty if not from 'Table' element
        when None          =>
          if Col.Original_Table.Upper_Case /= "" then
            View_Def.Append("  " & Col.Original_Table.Upper_Case & "." & Col.Name.Upper_Case);
          else
            View_Def.Append("  " & Col.Name.Upper_Case);
          end if;
        when As            => View_Def.Append("  " & Col.As.Upper_Case);
        when As_Oracle     => View_Def.Append("  " & Col.As_Oracle.Upper_Case);
        when As_SqlServer  => View_Def.Append("  " & Col.As_SqlServer.Upper_Case);
        when As_Postgresql => View_Def.Append("  " & Col.As_Postgresql.Upper_Case);
      end case;
      View_Def.Append("," & Ascii.Lf);
    end loop;
    -- kill last  "," & Ascii.Lf
    View_Def.Delete_Last_Char;
    View_Def.Delete_Last_Char;

    Put_Line(View_Def.Fix_String);
    View_Def.Reset;

    From_Source := From; -- start with assumption no extra info
    case Sql.Database is
      when Sql.Postgresql =>
        if Self.From_Postgresql.Fix_String /= "" then
          From_Source := From_Postgresql;  -- specialized db info
        end if;
    end case;

    View_Def.Set("from");
    case From_Source is
      when From            => View_Def.Append("  " & Self.From.Fix_String);
      when From_Oracle     => View_Def.Append("  " & Self.From_Oracle.Fix_String);
      when From_SqlServer  => View_Def.Append("  " & Self.From_SqlServer.Fix_String);
      when From_Postgresql => View_Def.Append("  " & Self.From_Postgresql.Fix_String);
    end case;

    Put_Line(View_Def.Fix_String);
    Put_Line(";");
    Put_Line("commit;");
    Put_Line("");

  end Print_Postgresql_Create_DDL;
  --------------------------------------------------------------------
  procedure Print_Oracle_Drop_DDL(Self : in out View_Type) is
    use Text_Io;
  begin
    -- write like
    -- prompt 'dropping view XSYSTEM';
    -- drop view XSYSTEM
    -- /
    Put_Line("prompt 'dropping view " & Self.Name.Upper_Case & "';");
    Put_Line("drop view " & Self.Name.Upper_Case & ";");
  end Print_Oracle_Drop_DDL;
  --------------------------------------------------------------------
  procedure Print_Sql_Server_Drop_DDL(Self : in out View_Type) is
    use Text_Io;
  begin
    -- write like
    -- prompt 'dropping view XSYSTEM';
    -- drop view XSYSTEM
    -- /
    Put_Line("drop view " & Self.Name.Upper_Case);
    Put_Line("go");
  end Print_Sql_Server_Drop_DDL;
  --------------------------------------------------------------------
  procedure Print_Postgresql_Drop_DDL(Self : in out View_Type) is
    use Text_Io;
  begin
    Put_Line("begin;");
    Put_Line("drop view if exists " & Self.Name.Upper_Case & ";");
    Put_Line("commit;");
  end Print_Postgresql_Drop_DDL;
  --------------------------------------------------------------------


end Repository.View ;

