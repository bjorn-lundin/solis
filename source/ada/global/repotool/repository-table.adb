with Ada.Environment_Variables;
with Ada.Directories;
--with Ada.Characters.Handling;
with Text_Io;

with GNAT.String_Split;
with Sax;
with Sax.Readers;        use Sax.Readers;
with Sax.Attributes;
with Input_Sources.File; use Input_Sources.File;
with Unicode.CES;
with Utils;


package body Repository.Table is


  Tag_Table           : constant String := "Table";
  Attr_Name           : constant String := "Name";
--  Attr_Loadable       : constant String := "Loadable";
--  Attr_Wiz            : constant String := "Wiz";
  Attr_Desc           : constant String := "Desc";
  Attr_Tablespace     : constant String := "Tablespace";


  Tag_Column          : constant String := "Column";
  Attr_Description    : constant String := "Description";
  Attr_AllowNull      : constant String := "AllowNull";
--  Attr_Setup          : constant String := "Setup";

  Tag_Index           : constant String := "Index";
  Attr_Columns        : constant String := "Columns";
  Attr_Type           : constant String := "type";

  type Reader is new Sax.Readers.Reader with record
    Table             : Table_Type;
    Current_Tag       : Unbounded_String := Null_Unbounded_String;
    Config            : Config_Type;
    Index_Tags        : Integer_4 := 0;
--    Language          : Language_Type;
--    State            : Table_State_Type := Table_State_Type'First;
--    Language_Is_Valid : Boolean          := False; -- until we get a valid lang
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

  function Index_List_String( List : Index_Type) return String is
    Tmp : String_Object;
  begin
    if not List.Is_Empty then
      for Idx of List loop
        Tmp.Set(Tmp.Fix_String & Idx.To_String & " " & Ascii.Lf);
      end loop;
      return Ascii.Lf & Tmp.Fix_String;
    else
      return "";
    end if;
  end Index_List_String;
  ------------------------------------------------

  overriding
  function To_String(Self : Table_Column_Type ) return String is
  begin
    return C.Column_Type(Self).To_String &
           Self.Nullable'Img & " " &
           Self.Primary'Img & " " &
           Self.Foreign'Img & " " &
           Self.Indexed'Img & " " &
           Self.Unique'Img ;
  end To_String;

  ------------------------------------------------
  overriding
  function To_String(Self : Table_Type) return String is
  begin
    return Self.Name.Fix_String & " " &
           Self.Wiz'Img & " " &
           Self.Desc.Fix_String & " " &
           Self.Tablespace.Fix_String & " " &
           Column_List_String(Self.Columns) & " " &
           Index_List_String(Self.Indicies) & " " &
           Self.Ixx_Type'Img;
  end To_String;
  ---------------------------------------------------------
  procedure Create_Ud4(Self : in out Table_Type ; Config : Repository.Config_Type'class) is
    My_Reader    : Reader;
    Input        : File_Input;
  begin
    if Self.Name.UBString = Null_Unbounded_String then
      raise Sequence_Error with "Table name must be set before calling Create";
    end if;
    Debug("Table create: '" & Self.Name.Fix_String & "'");

    My_Reader.Table := Self;
    My_Reader.Config := Repository.Config_Type(Config);

    declare
      Filename : String := Utils.Lower_Case(
                               Config.Item(Clreqs).Directory.Fix_String & "/"  &
                               Config.Item(Clreqs).Prefix.Fix_String & "_" & Self.Name.Fix_String & ".xml");
      Xsdname  : String := Ada.Environment_Variables.Value(Name => "BOT_CONFIG") & "/repository/table.xsd";
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
    Self := My_Reader.Table;
    Self.Is_Initialized := True;
    Self.Entity_Type := Ud4;
  end Create_Ud4;

  -------------------------------------------------------------------------------------
  procedure Create_Ud4(Self : in out Table_Type ; Name : String; Config : Repository.Config_Type'class) is
  begin
    Self.Name.Set(Name);
    Self.Create_Ud4(Config);
  end Create_Ud4;

  -------------------------------------------------------------------------------------

  procedure Create(Self : in out Table_Type ; Config : Repository.Config_Type'class) is
    My_Reader    : Reader;
    Input        : File_Input;
  begin
    if Self.Name.UBString = Null_Unbounded_String then
      raise Sequence_Error with "Table name must be set before calling Create";
    end if;
    Debug("Table create: '" & Self.Name.Fix_String & "'");

    My_Reader.Table := Self;
    My_Reader.Config := Repository.Config_Type(Config);

    declare
      Filename : String := Utils.Lower_Case(
                               Config.Item(Tables).Directory.Fix_String & "/"  &
                               Config.Item(Tables).Prefix.Fix_String & "_" & Self.Name.Fix_String & ".xml");
      Xsdname  : String := Ada.Environment_Variables.Value(Name => "BOT_CONFIG") & "/repository/table.xsd";
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
    Self := My_Reader.Table;
    Self.Is_Initialized := True;
    Self.Entity_Type := Db;
  end Create;

  -------------------------------------------------------------------------------------
  procedure Create(Self : in out Table_Type ; Name : String; Config : Repository.Config_Type'class) is
  begin
    Self.Name.Set(Name);
    Self.Create(Config);
  end Create;

  -------------------------------------------------------------------------------------
  overriding
  procedure Reset(Self : in out Table_Type ) is
  begin
    Self.Name.Reset;
    Self.Wiz := Wiz_Type'First;
    Self.Desc.Reset;
    Self.Tablespace.Reset;

    for Col of Self.Columns loop
      Col.Reset;
    end loop;
    Self.Columns.Clear;
    for Idx of Self.Indicies loop
      Idx.Reset;
    end loop;
    Self.Indicies.Clear;
  end Reset;

  -------------------------------------------------------------------------------------
  overriding procedure Start_Element(Handler       : in out Reader;
                          Namespace_URI : Unicode.CES.Byte_Sequence := "";
                          Local_Name    : Unicode.CES.Byte_Sequence := "";
                          Qname         : Unicode.CES.Byte_Sequence := "";
                          Atts          : Sax.Attributes.Attributes'Class) is
    pragma Unreferenced(Namespace_URI);
    pragma Unreferenced(Qname);
    The_Tag : constant String := Local_Name;
  begin
    Handler.Current_Tag := To_Unbounded_String(The_Tag);
--    Feedback("Start_Element " & The_Tag );

    if The_Tag = Tag_Table then
      Handler.Table.Name.Set(Atts.Get_Value(Attr_Name));
      Handler.Table.Desc.Set(Atts.Get_Value(Attr_Desc));
      Handler.Table.Tablespace.Set(Atts.Get_Value(Attr_Tablespace));

    elsif The_Tag = Tag_Column then

      declare
        Col : Table_Column_Type;
      begin
        Col.Name.Set(Atts.Get_Value(Attr_Name));
        Col.Create(Handler.Config);
        Col.Nullable := Atts.Get_Value(Attr_AllowNull) = "1";
        Col.Description.Set(Atts.Get_Value(Attr_Description));

        Handler.Table.Columns.Append(Col);

        if Col.Name.Lower_Case = "ixxluda" then
          Handler.Table.Ixx_Type := Date_Time;
        elsif Col.Name.Lower_Case = "ixxluts" then
          Handler.Table.Ixx_Type := Timestamp;
        end if;
      end;

    elsif The_Tag = Tag_Index then
      declare
        Idx : I.Index_Type;
      begin
        Idx.Columns.Set(Atts.Get_Value(Attr_Columns));
        Idx.Type_Of := I.Index_Type_Type'Value(Atts.Get_Value(Attr_Type));
        Handler.Index_Tags := Handler.Index_Tags +1;
        Idx.Sequence_Number := Handler.Index_Tags;
        declare
          use Gnat;
          Subs : String_Split.Slice_Set;
          Seps : constant String := ",";
        begin
            --check for multi-fields
            String_Split.Create (S          => Subs,
                                 From       => Idx.Columns.Camel_Case,
                                 Separators => Seps,
                                 Mode       => String_Split.Multiple);

            -- for each field in idx.columns loop
            for j in 1 .. String_Split.Slice_Count(Subs) loop
              declare
                Column_Name : String := String_Split.Slice(Subs, j);
               -- use Types;
              begin
                --mark the correct column as apropiate
                for Col of Handler.Table.Columns loop
                  if Utils.Lower_Case(Column_Name) = Col.Name.Lower_Case then
                    Idx.Column_List.Append(C.Column_Type(Col));
                   -- Feedback("Added " & Col.Name.Fix_String & " to list for " & Idx.Columns.Fix_String);
                    exit; -- only 1 can match
                  end if;
                end loop;
              end;
            end loop;
        end;
        Handler.Table.Indicies.Append(Idx);
      end;

    end if;
  exception
    when Ada.Strings.Length_Error =>
      Feedback("Ada.Strings.Length_Error on Tag '" & The_Tag & "'");
  end Start_Element;
  --++--++--++--++--++--++--++--++--++--++--++--++--++--
  overriding procedure End_Element(Handler       : in out Reader;
                        Namespace_URI : Unicode.CES.Byte_Sequence := "";
                        Local_Name    : Unicode.CES.Byte_Sequence := "";
                        Qname         : Unicode.CES.Byte_Sequence := "") is
    pragma Unreferenced(Namespace_URI);
    pragma Unreferenced(Qname);
    The_Tag : constant String := Local_Name;
  begin
    if The_Tag = Tag_Table then
      --mark each columns as indexed where appropiate

      declare
        use Gnat;
        Subs : String_Split.Slice_Set;
        Seps : constant String := ",";
      begin -- loop all indices

        for Idx of Handler.Table.Indicies loop
          --check for multi-fields
          String_Split.Create (S          => Subs,
                               From       => Idx.Columns.Fix_String,
                               Separators => Seps,
                               Mode       => String_Split.Multiple);

          -- for each field in idx.columns loop
          for j in 1 .. String_Split.Slice_Count(Subs) loop
            declare
              Column_Name : String := String_Split.Slice(Subs, j);
             -- use Types;
            begin
              --mark the correct column as apropiate
              for Col of Handler.Table.Columns loop
                if Utils.Lower_Case(Column_Name) = Col.Name.Lower_Case  then

                  case Idx.Type_Of is
                    when I.Primary     => Col.Primary    := True;
                    when I.Index       => Col.Indexed    := True;
                    when I.Unique      => Col.Unique     := True;
                    when I.Functional  => Col.Functional := True;
                  end case;
                end if;
              end loop;
            end;
          end loop;
        end loop;

        -- check for all fields being part of primary key
        -- also put all pks in separate list
        Handler.Table.All_Columns_Are_Primary := True ; -- assume all primary until proven otherwise

        for Col of Handler.Table.Columns loop
          if not Col.Primary then
            Handler.Table.All_Columns_Are_Primary := False;
          else
            Handler.Table.Primary_Column_List.Append(Col);
            Handler.Table.Primary_Column_List_But_Last.Append(Col);
            Handler.Table.Num_Primary_Keys := Handler.Table.Num_Primary_Keys +1;
            Handler.Table.Last_Primary_Column := Col;
          end if;
          Handler.Table.Num_Columns := Handler.Table.Num_Columns +1;
        end loop;

        -- delete the last index. With that, we get the same as Delete/Is_Existing with pk only
        if not Handler.Table.Primary_Column_List_But_Last.Is_Empty then
          Handler.Table.Primary_Column_List_But_Last.Delete_Last;
        end if;

      end;
    end if;

  exception
    when Ada.Strings.Length_Error =>
    	Feedback("Ada.Strings.Length_Error on Tag '" & The_Tag & "'");
  end End_Element;
  --++--++--++--++--++--++--++--++--++--++--++--++--++--

  overriding  procedure Characters(Handler : in out Reader;
                       Ch      : Unicode.CES.Byte_Sequence := "") is
    The_Tag   : constant String := To_String(Handler.Current_Tag);
    The_Value : constant String := Utils.Expand_File_Path(To_Iso_Latin_15(Ch));
  begin
--    Feedback("Characters " & The_Tag & " -> '" & The_Value & "'");
    null;
  exception
    when Ada.Strings.Length_Error =>
      Feedback("Ada.Strings.Length_Error on Tag '" & The_Tag & "' with data '" & The_Value & "'");
  end Characters;
  --------------------------------------------------------------------

  procedure Set_Name(Self : in out Table_Type; Filename : String; Config : Repository.Config_Type'class) is
    use Ada.Directories;
    Basename : String  := Base_Name(Filename);
    Pos      : Integer := 0;
    Prefix   : String  := Config.Item(Tables).Prefix.Fix_String;
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
  procedure Print_Oracle_Create_DDL(Self : in out Table_Type) is
    use Text_Io;
    Index_Counter : Integer_4 := 0;
    DDL          : Unbounded_String := Null_Unbounded_String;
    String_Range : Unbounded_String := Null_Unbounded_String;
    Nullable     : Unbounded_String := Null_Unbounded_String;
    Comment      : Unbounded_String := Null_Unbounded_String;
  begin
    Put_Line("");
    Put_Line("prompt 'creating table " & Self.Name.Fix_String & "';");
    Put_Line("create table " & Self.Name.Fix_String & " ( " );

    -- loop over all columns and write like
    --  BPLOAID number(9) default 1 not null , -- Primary Key

    for Col of Self.Columns loop
      Feedback(Col.Name.Upper_Case & Col.Type_Of'Img);
      case Data_Type(Col.Type_Of) is
        when A_Char => String_Range := To_Unbounded_String("(" & Utils.Trim(Col.Size_Of'Img) & ")");
        when others => String_Range := Null_Unbounded_String;
      end case;

      if Col.Nullable then
        Nullable := To_Unbounded_String(Default_Value(Data_Type(Col.Type_Of), Oracle));
      else
        Nullable := To_Unbounded_String(Default_Value(Data_Type(Col.Type_Of), Oracle) & " not null");
      end if;

      if Col.Primary then
        Index_Counter := Index_Counter + 1;
        Comment := To_Unbounded_String("-- Primary Key");
      elsif Col.Unique then
        Index_Counter := Index_Counter + 1;
        Comment := To_Unbounded_String("-- unique index" & Index_Counter'Img);
      elsif Col.Indexed then
        Index_Counter := Index_Counter + 1;
        Comment := To_Unbounded_String("-- non unique index" & Index_Counter'Img);
      else
        Comment := Null_Unbounded_String;
      end if;
      Append(DDL, "  " &
                     Col.Name.Upper_Case & " " &
                     Utils.Lower_Case (To_Sql_Type(Data_Type(Col.Type_Of), Oracle)) &
                     To_String(String_Range) &
                     " " &
                     To_String(Nullable) &
                     " , " &
                     To_String(Comment) & Ascii.Lf);
    end loop;
    -- replace the last comma with Lf & ")", ie len -2 since it ends with ,_lf (comma, space, Lf)
    declare
      sDDL : String := To_String(DDL);
    begin
      for j in reverse sDDL'range loop
        case sDDL(j) is
          when Ascii.Lf => sDDL(j) := ' ';
          when ','      => sDDL(j) := ')'; exit;
          when others   => null;
        end case;
      end loop;
      Put_Line(sDDL);
    end;
    if Self.Tablespace.Fix_String'Length > 0 then
      Put_Line(" tablespace " & Self.Tablespace.Fix_String);
    end if;
    Put_Line("/");
    Put_Line("");

    -- now the primary key
    Index_Counter := 0;
    for Idx of Self.Indicies loop
      Index_Counter := Index_Counter + 1;
      case Idx.Type_Of is
        when I.Primary =>
          Put_Line("alter table " & Self.Name.Fix_String & " add constraint " & Self.Name.Fix_String & "P" & Utils.Trim(Index_Counter'Img) & " primary key (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          Put_Line("/");

        when I.Unique =>
          Put_Line("create unique index " & Self.Name.Fix_String & "I" & Utils.Trim(Index_Counter'Img) & " on "  & Self.Name.Fix_String & " (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          Put_Line("/");

        when I.Index =>
          Put_Line("create index " & Self.Name.Fix_String & "I" & Utils.Trim(Index_Counter'Img) & " on "  & Self.Name.Fix_String & " (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          Put_Line("/");

        when I.Functional =>
          Put_Line("create index " & Self.Name.Fix_String & "I" & Utils.Trim(Index_Counter'Img) & " on "  & Self.Name.Fix_String & " (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          Put_Line("/");
      end case;

      Put_Line("");
    end loop;

    Put_Line("prompt 'comment on table " & Self.Name.Fix_String & " is '" & Self.Desc.Fix_String & "' ';");
    Put_Line("comment on table " & Self.Name.Fix_String & " is '" & Self.Desc.Fix_String & "'");
    Put_Line("/");

    -- loop over all columns again and write like
    --  comment on column BPLOAD.bsortts is 'Sort timestamp'
    for Col of Self.Columns loop
      Put_Line("prompt 'comment on column " & Self.Name.Fix_String & "." & Col.Name.Fix_String & " is '" & Col.Description.Fix_String & "' ';");
      Put_Line("comment on column " & Self.Name.Fix_String & " is '" & Col.Description.Fix_String & "'");
      Put_Line("/");
    end loop;
  end Print_Oracle_Create_DDL;
  --------------------------------------------------------------------
  procedure Print_Sql_Server_Create_DDL(Self : in out Table_Type) is
    use Text_Io;
    Index_Counter : Integer_4 := 0;
    DDL          : Unbounded_String := Null_Unbounded_String;
    String_Range : Unbounded_String := Null_Unbounded_String;
    Nullable     : Unbounded_String := Null_Unbounded_String;
    Comment      : Unbounded_String := Null_Unbounded_String;
  begin
    Put_Line("");
    Put_Line("create table " & Self.Name.Fix_String & " ( " );

    -- loop over all columns and write like
    --  BPLOAID number(9) default 1 not null , -- Primary Key
    Put_Line("num cols :" & Self.Columns.Length'Img);
    for Col of Self.Columns loop
      Put_Line(Col.Name.Fix_String & ":" & Col.Type_Of'Img);
      case Data_Type(Col.Type_Of) is
        when A_Char => String_Range := To_Unbounded_String("(" & Utils.Trim(Col.Size_Of'Img) & ") COLLATE SQL_Latin1_General_CP1_CS_AS");
        when others => String_Range := Null_Unbounded_String;
      end case;

      if Col.Nullable then
        Nullable := To_Unbounded_String(Default_Value(Data_Type(Col.Type_Of), Sql_Server));
      else
        Nullable := To_Unbounded_String(Default_Value(Data_Type(Col.Type_Of), Sql_Server) & " not null");
      end if;

      if Col.Primary then
        Index_Counter := Index_Counter + 1;
        Comment := To_Unbounded_String("-- Primary Key");
      elsif Col.Unique then
        Index_Counter := Index_Counter + 1;
        Comment := To_Unbounded_String("-- unique index" & Index_Counter'Img);
      elsif Col.Indexed then
        Index_Counter := Index_Counter + 1;
        Comment := To_Unbounded_String("-- non unique index" & Index_Counter'Img);
      else
        Comment := Null_Unbounded_String;
      end if;
      Append(DDL, "  " &
                     Col.Name.Upper_Case & " " &
                     Utils.Lower_Case (To_Sql_Type(Data_Type(Col.Type_Of), Sql_Server)) &
                     To_String(String_Range) &
                     " " &
                     To_String(Nullable) &
                     " , " &
                     To_String(Comment) & Ascii.Lf);
    end loop;
    -- replace the last comma with Lf & ")", ie len -2 since it ends with ,_lf (comma, space, Lf)
    declare
      sDDL : String := To_String(DDL);
    begin
      for j in reverse sDDL'range loop
        case sDDL(j) is
          when Ascii.Lf => sDDL(j) := ' ';
          when ','      => sDDL(j) := ')'; exit;
          when others   => null;
        end case;
      end loop;
      Put_Line(sDDL);
    end;
    Put_Line("go");
    Put_Line("");

    -- now the primary key
    Index_Counter := 0;
    for Idx of Self.Indicies loop
      Index_Counter := Index_Counter + 1;
      case Idx.Type_Of is
        when I.Primary =>
          Put_Line("alter table " & Self.Name.Fix_String & " add constraint " & Self.Name.Fix_String & "P" & Utils.Trim(Index_Counter'Img) & " primary key (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          Put_Line("go");

        when I.Unique =>
          Put_Line("create unique index " & Self.Name.Fix_String & "I" & Utils.Trim(Index_Counter'Img) & " on " & Self.Name.Fix_String & " (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          Put_Line("go");

        when I.Index =>
          Put_Line("create index " & Self.Name.Fix_String & "I" & Utils.Trim(Index_Counter'Img) & " on " & Self.Name.Fix_String & " (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          Put_Line("go");

        when I.Functional =>
          Put_Line("create index " & Self.Name.Fix_String & "I" & Utils.Trim(Index_Counter'Img) & " on " & Self.Name.Fix_String & " (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          Put_Line("go");
      end case;

      Put_Line("");
    end loop;

  end Print_Sql_Server_Create_DDL;
  --------------------------------------------------------------------
  procedure Print_Postgresql_Create_DDL(Self : in out Table_Type) is
    use Text_Io;
    Index_Counter : Integer_4 := 0;
    DDL          : Unbounded_String := Null_Unbounded_String;
    String_Range : Unbounded_String := Null_Unbounded_String;
    Nullable     : Unbounded_String := Null_Unbounded_String;
    Comment      : Unbounded_String := Null_Unbounded_String;
  begin
    Put_Line("begin;");
    Put_Line("create table " & Self.Name.Fix_String & " ( " );

    -- loop over all columns and write like
    --  BPLOAID number(9) default 1 not null , -- Primary Key
    for Col of Self.Columns loop
      case Data_Type(Col.Type_Of) is
        when A_Char => String_Range := To_Unbounded_String("(" & Utils.Trim(Col.Size_Of'Img) & ")");
        when others => String_Range := Null_Unbounded_String;
      end case;

      if Col.Nullable then
        Nullable := To_Unbounded_String(Default_Value(Data_Type(Col.Type_Of), Postgresql));
      else
        Nullable := To_Unbounded_String(Default_Value(Data_Type(Col.Type_Of), Postgresql) & " not null");
      end if;

      if Col.Primary then
        Index_Counter := Index_Counter + 1;
        Comment := To_Unbounded_String("-- Primary Key");
      elsif Col.Unique then
        Index_Counter := Index_Counter + 1;
        Comment := To_Unbounded_String("-- unique index" & Index_Counter'Img);
      elsif Col.Indexed then
        Index_Counter := Index_Counter + 1;
        Comment := To_Unbounded_String("-- non unique index" & Index_Counter'Img);
      else
        Comment := Null_Unbounded_String;
      end if;
      Append(DDL, "  " &
                     Col.Name.Upper_Case & " " &
                     Utils.Lower_Case (To_Sql_Type(Data_Type(Col.Type_Of), Postgresql)) &
                     To_String(String_Range) &
                     " " &
                     To_String(Nullable) &
                     " , " &
                     To_String(Comment) & Ascii.Lf);
    end loop;
    -- replace the last comma with Lf & ")", ie len -2 since it ends with ,_lf (comma, space, Lf)
    declare
      sDDL : String := To_String(DDL);
    begin
      for j in reverse sDDL'range loop
        case sDDL(j) is
          when Ascii.Lf => sDDL(j) := ' ';
          when ','      => sDDL(j) := ')'; exit;
          when others   => null;
        end case;
      end loop;
      Put_Line(sDDL); --last part of sDDL might be a comment. Make sure the ';' is on new line
    end;

    if Self.Tablespace.Fix_String'Length > 0 then
      Put_Line(" tablespace " & Self.Tablespace.Fix_String);
    end if;

    Put_Line(";");
    Put_Line("");

    -- now the primary key
    Index_Counter := 0;
    for Idx of Self.Indicies loop
      Index_Counter := Index_Counter + 1;
      case Idx.Type_Of is
        when I.Primary =>
          Put_Line("alter table " & Self.Name.Fix_String & " add constraint " & Self.Name.Fix_String & "P" & Utils.Trim(Index_Counter'Img) & " primary key (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          Put_Line(";");

        when I.Unique =>
          Put_Line("create unique index " & Self.Name.Fix_String & "I" & Utils.Trim(Index_Counter'Img) & " on "  & Self.Name.Fix_String & " (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          if Self.Tablespace.Fix_String'Length > 0 then
            Put_Line(" tablespace " & Self.Tablespace.Fix_String);
          end if;
          Put_Line(";");

        when I.Index =>
          Put_Line("create index " & Self.Name.Fix_String & "I" & Utils.Trim(Index_Counter'Img) & " on "  & Self.Name.Fix_String & " (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          if Self.Tablespace.Fix_String'Length > 0 then
            Put_Line(" tablespace " & Self.Tablespace.Fix_String);
          end if;
          Put_Line(";");

        when I.Functional =>
          Put_Line("create index " & Self.Name.Fix_String & "I" & Utils.Trim(Index_Counter'Img) & " on "  & Self.Name.Fix_String & " (");
          Put_Line("  " & Idx.Columns.Fix_String);
          Put_Line(")");
          if Self.Tablespace.Fix_String'Length > 0 then
            Put_Line(" tablespace " & Self.Tablespace.Fix_String);
          end if;
          Put_Line(";");
      end case;

      Put_Line("");
    end loop;

    Put_Line("comment on table " & Self.Name.Fix_String & " is '" & Self.Desc.Fix_String & "';");

    -- loop over all columns again and write like
    --  comment on column BPLOAD.bsortts is 'Sort timestamp'
    for Col of Self.Columns loop
      Put_Line("comment on column " & Self.Name.Fix_String & "." & Col.Name.Fix_String & " is '" & Col.Description.Fix_String & "';");
    end loop;
    Put_Line("commit;");
    Put_Line("");

  end Print_Postgresql_Create_DDL;
  --------------------------------------------------------------------
  procedure Print_Oracle_Drop_DDL(Self : in out Table_Type) is
    use Text_Io;
  begin
    -- write like
    -- prompt 'dropping table XSYSTEM';
    -- drop table XSYSTEM
    -- /
    Put_Line("prompt 'dropping table " & Self.Name.Upper_Case & "';");
    Put_Line("drop table " & Self.Name.Upper_Case & ";");
  end Print_Oracle_Drop_DDL;
  --------------------------------------------------------------------
  procedure Print_Sql_Server_Drop_DDL(Self : in out Table_Type) is
    use Text_Io;
  begin
    -- write like
    -- prompt 'dropping table XSYSTEM';
    -- drop table XSYSTEM
    -- /
    Put_Line("drop table " & Self.Name.Upper_Case);
    Put_Line("go");
  end Print_Sql_Server_Drop_DDL;
  --------------------------------------------------------------------
  procedure Print_Postgresql_Drop_DDL(Self : in out Table_Type) is
    use Text_Io;
  begin
    Put_Line("begin;");
    Put_Line("drop table if exists " & Self.Name.Upper_Case & ";");
    Put_Line("commit;");
  end Print_Postgresql_Drop_DDL;
  --------------------------------------------------------------------


  --------------------------------------------------------------
  procedure Keyed_Sql_Statement(Self : in out Table_Type ;
                                Stm_Name : String;
                                First_Stm_Row : String;
                                Key : I.Index_Type_Type;
                                Generate_IXX : Boolean := False;
                                Turns : Integer_4 := 99999;
                                Order_By_PK : Boolean := False) is
    use Text_Io;
    Keyword   : String_Object;
    This_Turn : Integer_4:= 0;
  begin
    Code_Debug("  -- start Keyed_Sql_Statement" );
    Put_Line("    " & Stm_Name & ".Prepare(" & Quote(First_Stm_Row & " ") & " &" );

    Keyword.Set("where");

    for Col of Self.Columns loop
      This_Turn := This_Turn +1;
      exit when This_Turn > Turns;

      case Key is
        when I.Primary =>
          if Col.Primary then
            Put_Line("                " & Quote(Keyword.Lower_Case & " " & Col.Name.Upper_Case & " =:" & Col.Name.Upper_Case & " ")  & " &" );
            Keyword.Set("and");
          end if;

        when I.Unique =>
          if Col.Unique then
            Put_Line("                " & Quote(Keyword.Lower_Case & " " & Col.Name.Upper_Case & " =:" & Col.Name.Upper_Case & " ")  & " &" );
            Keyword.Set("and");
          end if;
        when I.Index | I.Functional => raise Configuration_Error with "Keyed_Sql_Statement: " & Key'Img & " " & Col.Name.Camel_Case;
      end case;
    end loop;

    if Generate_IXX then
      case Self.Ixx_Type is
        when None      => null;
        when Date_Time =>
          Put_Line("            " & Quote("and IXXLUPD = :IXXLUPD ") & " &");
          Put_Line("            " & Quote("and IXXLUDA = :IXXLUDA ") & " &");
          Put_Line("            " & Quote("and IXXLUTI = :IXXLUTI ") & " &");
        when Timestamp =>
          Put_Line("            " & Quote("and IXXLUPD = :IXXLUPD ") & " &");
          Put_Line("            " & Quote("and IXXLUTS = :IXXLUTS ") & " &");
      end case;
    end if;

    if Order_By_PK then
      Put_Line("            " & Quote("order by ") & " & ");
      for Col of Self.Primary_Column_List_But_Last loop
        Put_Line("            " & Quote(Col.Name.Upper_Case & ", ") & " & ");
      end loop;
      Put_Line("            " & Quote(Self.Last_Primary_Column.Name.Upper_Case) & ");");
    else
      Put_Line("            " & Quote("") & ");");
    end if;

    Code_Debug("  -- stop Keyed_Sql_Statement" );

  end Keyed_Sql_Statement;
-----------------------------------------------------------

  procedure Set_Keyed_Sql_Statement(Self         : in out Table_Type ;
                                    Stm_Name     : in     String;
                                    Key          : in     I.Index_Type_Type;
                                    Generate_IXX : in     Boolean := False;
                                    Turns        : in     Integer_4 := 99999) is
    use Text_Io;
    This_Turn : Integer_4:= 0;
  begin
    Code_Debug("  -- start Set_Keyed_Sql_Statement" );

    for Col of Self.Columns loop
      This_Turn := This_Turn +1;
      exit when This_Turn > Turns;

      case Key is
        when I.Primary =>
          if Col.Primary then
            case Data_Type(Col.Type_Of) is
              when A_Char .. A_Short_Code =>
                Put_Line("    " & Stm_Name & ".Set(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              when A_Date =>
                Put_Line("    " & Stm_Name & ".Set_Date(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              when A_Time =>
                Put_Line("    " & Stm_Name & ".Set_Time(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              when A_Timestamp =>
                Put_Line("    " & Stm_Name & ".Set_Timestamp(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              when A_Clob =>
                Put_Line("    " & Stm_Name & ".Set_Clob(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              when others =>
                raise Configuration_Error with "Not supported datatype: " &  Data_Type(Col.Type_Of)'Img;
            end case;
          end if;

        when I.Unique =>
          if Col.Unique then
            case Data_Type(Col.Type_Of) is
              when A_Char .. A_Short_Code =>
                Put_Line("    " & Stm_Name & ".Set(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              when A_Date =>
                Put_Line("    " & Stm_Name & ".Set_Date(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              when A_Time =>
                Put_Line("    " & Stm_Name & ".Set_Time(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              when A_Timestamp =>
                Put_Line("    " & Stm_Name & ".Set_Timestamp(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              when A_Clob =>
                Put_Line("    " & Stm_Name & ".Set_Clob(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              when others =>
                raise Configuration_Error with "Not supported datatype: " &  Data_Type(Col.Type_Of)'Img;
            end case;
          end if;
        when I.Index | I.Functional => raise Configuration_Error with "Keyed_Sql_Statement: " & Key'Img & " " & Col.Name.Camel_Case;
      end case;
    end loop;

    if Generate_IXX then
      case Self.Ixx_Type is
        when None      => null;
        when Date_Time =>
          Put_Line("    " & Stm_Name & ".Set(" & Quote("IXXLUPD") & ", Data.Ixxlupd);");
          Put_Line("    " & Stm_Name & ".Set_Date(" & Quote("IXXLUDA") & ", Data.Ixxluda);");
          Put_Line("    " & Stm_Name & ".Set_Time(" & Quote("IXXLUTI") & ", Data.Ixxluti);");
        when Timestamp =>
          Put_Line("    " & Stm_Name & ".Set(" & Quote("IXXLUPD") & ", Data.Ixxlupd);");
          Put_Line("    " & Stm_Name & ".Set_Timestamp(" & Quote("IXXLUTS") & ", Data.Ixxluts);");
      end case;
    end if;
    Code_Debug("  -- stop Set_Keyed_Sql_Statement" );
  end Set_Keyed_Sql_Statement;

  ---------------------------------------------------------------------------------------------

  procedure Prepare_All_Columns(Self          : in out Table_Type ;
                                Stm_Name      : in     String;
                                First_Stm_Row : in     String ;
                                Where_Keys    : in     Boolean;
                                Old_IXX       : in     Boolean;
                                Set_Primary   : in     Boolean) is
    Keyword   : String_Object;
    use Text_Io;

  begin
    Code_Debug(" -- Start Prepare_All_Columns");

    Put_Line("    " & Stm_Name &".Prepare(" & Quote(First_Stm_Row & " ") & " &");
    declare
      Tmp : String_Object;
    begin
      for Col of Self.Columns loop
        if not Set_Primary then
          if not Col.Primary then
            Tmp.Append("            " & Quote(Col.Name.Upper_Case & "=:" & Col.Name.Upper_Case & ", " ) & " &" & Ascii.Lf);
--            Put_Line("            " & Quote(Col.Name.Upper_Case & "=:" & Col.Name.Upper_Case & ", " ) & " &");
          end if;
        else
          Tmp.Append("            " & Quote(Col.Name.Upper_Case & "=:" & Col.Name.Upper_Case & ", " ) & " &" & Ascii.Lf);
--          Put_Line("            " & Quote(Col.Name.Upper_Case & "=:" & Col.Name.Upper_Case & ", " ) & " &");
        end if;
      end loop;
      for j in 1 .. 6 loop
        Tmp.Delete_Last_Char; -- kill last ', " &'
      end loop;
      Tmp.Append(" " & Ascii.Quotation & " &"); -- put back all but the ','
      Put_Line(Tmp.Fix_String);
    end;

    Keyword.Set("where");
    if Where_Keys then
      for Col of Self.Primary_Column_List loop
        Put_Line("            " & Quote(Keyword.Fix_String & " " & Col.Name.Upper_Case & "=:" & Col.Name.Upper_Case & " ") & " &");
        Keyword.Set("and");
      end loop;
    end if;

    if Old_IXX then
      case Self.Ixx_Type is
        when None      => null;
        when Date_Time =>
          Put_Line("    " & Quote("and IXXLUPD" & " = :OLD_IXXLUPD ") & " &");
          Put_Line("    " & Quote("and IXXLUTI" & " = :OLD_IXXLUTI ") & " &");
          Put_Line("    " & Quote("and IXXLUDA" & " = :OLD_IXXLUDA ") & " &");
        when Timestamp =>
          Put_Line("    " & Quote("and IXXLUPD" & " = :OLD_IXXLUPD ") & " &");
          Put_Line("    " & Quote("and IXXLUTS" & " = :OLD_IXXLUTS ") & " &");
      end case;
    end if;
    Put_Line("            " & Quote("") & ");");

    Code_Debug(" -- Stop Prepare_All_Columns");
  end Prepare_All_Columns;
  -- --------------------------------------------------------------------------------------------

  procedure Insert_All_Columns(Self          : in out Table_Type;
                               Stm_Name      : in     String;
                               First_Stm_Row : in     String) is
    use Text_Io;
    Cur_Column : Integer_4 := 0;
  begin
    Code_Debug(" -- Start Insert_All_Columns");
    Put_Line("    " & Stm_Name &".Prepare(" & Quote(First_Stm_Row) & " &");

    for Col of Self.Columns loop
      -- over all but the last col
      Cur_Column := Cur_Column +1 ;
      if Cur_Column < Self.Num_Columns then
        Put_Line("         " & Quote(":" & Col.Name.Upper_Case & ",") & " &");
      else
        -- now the last col
        Put_Line("         " & Quote(":" & Col.Name.Upper_Case & ")" ) & ");");
      end if;
    end loop;
    Code_Debug(" -- Stop Insert_All_Columns");

  end Insert_All_Columns;

  -- --------------------------------------------------------------------------------------------

  procedure Index_Sql_Statement(T            : in out Table_Type;
                                Stm_Name      : in     String;
                                First_Stm_Row : in     String;
                                Order_By_PK   : in     Boolean;
                                Field_List    : in     C.Columns_Type := C.Columns_Pkg.Empty_List ) is
    Keyword : String_Object;
    use Text_Io;
--    Treat_This_Column : Boolean := True;
  begin
    Code_Debug(" -- Start Index_Sql_Statement");
    Put_Line("    " & Stm_Name & ".Prepare(" & Quote(First_Stm_Row & " ") & " &" );

    Keyword.Set("where");

    for Idx_Col of Field_List loop
      Put_Line("                " & Quote(Keyword.Lower_Case & " " & Idx_Col.Name.Upper_Case & " =:" & Idx_Col.Name.Upper_Case & " ")  & " &" );
      Keyword.Set("and");
    end loop;

    if Order_By_PK then
      Put_Line("            " & Quote("order by ") & " & ");
      for Col of T.Primary_Column_List_But_Last loop
        Put_Line("            " & Quote(Col.Name.Upper_Case & ", ") & " & ");
      end loop;
      Put_Line("            " & Quote(T.Last_Primary_Column.Name.Upper_Case) & ");");
    else
      Put_Line("            " & Quote("") & ");");
    end if;

    Code_Debug(" -- stop Index_Sql_Statement");
  end Index_Sql_Statement;

  -----------------------------------------------------
  procedure Set_Index_Sql_Statement(T            : in out Table_Type ;
                                    Stm_Name     : in     String;
                                    Field_List   : in     C.Columns_Type := C.Columns_Pkg.Empty_List;
                                    Generate_IXX : in     Boolean := False) is
    use Text_Io;
--    Treat_This_Column : Boolean := True;
  begin
    Code_Debug(" -- Start  Set_Index_Sql_Statement");

    for Idx_Col of Field_List loop
      case Data_Type(Idx_Col.Type_Of) is
        when A_Char .. A_Short_Code =>
          Put_Line("    " & Stm_Name & ".Set(" & Quote(Idx_Col.Name.Upper_Case) & ", Data." & Idx_Col.Name.Camel_Case & ");");
        when A_Date =>
          Put_Line("    " & Stm_Name & ".Set_Date(" & Quote(Idx_Col.Name.Upper_Case) & ", Data." & Idx_Col.Name.Camel_Case & ");");
        when A_Time =>
          Put_Line("    " & Stm_Name & ".Set_Time(" & Quote(Idx_Col.Name.Upper_Case) & ", Data." & Idx_Col.Name.Camel_Case & ");");
        when A_Timestamp =>
          Put_Line("    " & Stm_Name & ".Set_Timestamp(" & Quote(Idx_Col.Name.Upper_Case) & ", Data." & Idx_Col.Name.Camel_Case & ");");
        when A_Clob =>
          Put_Line("    " & Stm_Name & ".Set_Clob(" & Quote(Idx_Col.Name.Upper_Case) & ", Data." & Idx_Col.Name.Camel_Case & ");");
        when others =>  raise Configuration_Error with "Not supported datatype: " &  Data_Type(Idx_Col.Type_Of)'Img;
      end case;
    end loop;

    if Generate_IXX then
      case T.Ixx_Type is
        when None      => null;
        when Date_Time =>
          Put_Line("    " & Stm_Name & ".Set(" & Quote("IXXLUPD") & ", Data.Ixxlupd);");
          Put_Line("    " & Stm_Name & ".Set_Date(" & Quote("IXXLUDA") & ", Data.Ixxluda);");
          Put_Line("    " & Stm_Name & ".Set_Time(" & Quote("IXXLUTI") & ", Data.Ixxluti);");
        when Timestamp =>
          Put_Line("    " & Stm_Name & ".Set(" & Quote("IXXLUPD") & ", Data.Ixxlupd);");
          Put_Line("    " & Stm_Name & ".Set_Timestamp(" & Quote("IXXLUTS") & ", Data.Ixxluts);");
      end case;
    end if;
    Code_Debug(" -- Stop  Set_Index_Sql_Statement");
  end Set_Index_Sql_Statement;

  ------------------------------------------------------

  procedure Set_All_Columns_But_Pk(Self          : in out Table_Type;
                            Stm_Name      : in     String;
                            Set_Old_IXX   : in     Boolean;
                            Set_Primary   : in     Boolean) is
    pragma Unreferenced(Set_Primary);
    use Text_Io;
    ------------------------------------------------------
    procedure Set_Null (Col : Table_Column_Type; Stm_Name : String) is
    begin
      Code_Debug(" -- Start Set_All_Columns_But_Pk.Set_Null");
      case Data_Type(Col.Type_Of) is
        when A_Char .. A_Short_Code | A_Clob =>
          Put_Line("      " & Stm_Name & ".Set_Null(" & Quote(Col.Name.Upper_Case) & ");");
        when A_Date .. A_Timestamp           =>
          Put_Line("      " & Stm_Name & ".Set_Null_Date(" & Quote(Col.Name.Upper_Case) & ");");
        when others =>  raise Configuration_Error with "Not supported datatype: " &  Data_Type(Col.Type_Of)'Img;
      end case;
      Code_Debug(" -- Stop  Set_All_Columns_But_Pk.Set_Null");
    end Set_Null;

    ------------------------------------------------------

    procedure Set_Non_Null (Col : Table_Column_Type; Stm_Name : String) is
    begin
      Code_Debug(" -- Start Set_All_Columns_But_Pk.Set_Non_Null");
      case Data_Type(Col.Type_Of) is
        when A_Char .. A_Short_Code => Put_Line("      " & Stm_Name & ".Set(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Clob                 => Put_Line("      " & Stm_Name & ".Set_Clob(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Time                 => Put_Line("      " & Stm_Name & ".Set_Time(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Date                 => Put_Line("      " & Stm_Name & ".Set_Date(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Timestamp            => Put_Line("      " & Stm_Name & ".Set_Timestamp(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when others =>  raise Configuration_Error with "Not supported datatype: " &  Data_Type(Col.Type_Of)'Img;
      end case;
      Code_Debug(" -- Stop  Set_All_Columns_But_Pk.Set_Non_Null");
    end Set_Non_Null;
    ------------------------------------------------------


    ----------------- Start Set_All_Columns_But_Pk -------------------------------------

    Non_Primary_Column_List : Columns_Type with Warnings => Off ;
  begin
    Code_Debug(" -- Start Set_All_Columns_But_Pk");

    if Set_Old_IXX then
      case Self.Ixx_Type is
        when None      => null;
        when Date_Time =>
          Put_Line("    " & Stm_Name & ".Set(" & Quote("OLD_IXXLUPD") & ", Data.Ixxlupd );");
          Put_Line("    " & Stm_Name & ".Set_Time(" & Quote("OLD_IXXLUTI") & ", Data.Ixxluti );");
          Put_Line("    " & Stm_Name & ".Set_Date(" & Quote("OLD_IXXLUDA") & ", Data.Ixxluda );");
        when Timestamp =>
          Put_Line("    " & Stm_Name & ".Set(" & Quote("OLD_IXXLUPD") & ", Data.Ixxlupd );");
          Put_Line("    " & Stm_Name & ".Set_Timestamp(" & Quote("OLD_IXXLUTS") & ", Data.Ixxluts );");
      end case;
    end if;
    Put_Line("");
    Put_Line("    if not Keep_Timestamp then");
    case Self.Ixx_Type is
      when None      =>
        Put_Line("      null; --for tables without Ixx*");
      when Date_Time =>
        Put_Line("      Data.Ixxluda := Now;");
        Put_Line("      Data.Ixxluti := Now;");
      when Timestamp =>
        Put_Line("      Data.Ixxluts := Now;");
    end case;
    Put_Line("    end if;");


    -- fill the Non_Primary_Column_List
    for Col of Self.Columns loop
      -- only indexed fields treated
      if not Col.Primary then
        Non_Primary_Column_List.Append(Col);
      end if;
    end loop;

    for Col of Non_Primary_Column_List loop
      if Col.Nullable then

        if Col.Name.Lower_Case = "ixxluda" or else
          Col.Name.Lower_Case = "ixxluda" then
          Put_Line("    if not Keep_Timestamp then");
          Put_Line("      null; --for tables without Ixxlud* ");
          Put_Line("      Data." & Col.Name.Camel_Case & " := Now;");
          Put_Line("    end if;");
          Set_Non_Null(Col,Stm_Name);
        else
          case Data_Type(Col.Type_Of) is
            when A_Boolean    =>
              -- call boolean cannot be null here
              Set_Non_Null(Col,Stm_Name);
            when others =>
              case Data_Type(Col.Type_Of) is
                when A_Char    =>
                  Put_Line("    if " & "Data." & Col.Name.Camel_Case & " = " &  Null_Value_For_Type_At_Comparison(Data_Type(Col.Type_Of), Col.Size_Of, "Data." & Col.Name.Fix_String) & " then");
                when others    => null;
                  Put_Line("    if " & "Data." & Col.Name.Camel_Case & " = " &  Null_Value_For_Type_At_Comparison(Data_Type(Col.Type_Of), Col.Size_Of, Col.Name.Fix_String) & " then");
              end case;
            Set_Null(Col,Stm_Name);
            Put_Line("    else");
            Set_Non_Null(Col,Stm_Name);
            Put_Line("    end if;");
          end case;
        end if;

      else --not nullable

        if Col.Name.Lower_Case = "ixxlupd" then
          Put_Line("    if not Keep_Timestamp then");
          Put_Line("      null; --for tables without Ixxlupd");
          Put_Line("      Data." & Col.Name.Camel_Case & " := Process.Name(1..15);");
          Put_Line("    end if;");
          Set_Non_Null(Col,Stm_Name);
        else
          Set_Non_Null(Col,Stm_Name);
        end if;
      end if;
    end loop;
    Non_Primary_Column_List.Clear;

    Code_Debug(" -- Stop  Set_All_Columns_But_Pk");
  end Set_All_Columns_But_Pk;


  ------------------------------------------------------

  procedure Set_All_Columns_Incl_Pk(Self          : in out Table_Type;
                            Stm_Name      : in     String;
                            Set_Old_IXX   : in     Boolean;
                            Set_Primary   : in     Boolean) is
    pragma Unreferenced(Set_Primary);
    use Text_Io;
    ------------------------------------------------------
    procedure Set_Null (Col : Table_Column_Type; Stm_Name : String) is
    begin
      Code_Debug(" -- Start Set_All_Columns_Incl_Pk.Set_Null");
      case Data_Type(Col.Type_Of) is
        when A_Char .. A_Double | A_Short_Code | A_Clob =>
          Put_Line("      " & Stm_Name & ".Set_Null(" & Quote(Col.Name.Upper_Case) & ");");
        when A_Date .. A_Timestamp           =>
          Put_Line("      " & Stm_Name & ".Set_Null_Date(" & Quote(Col.Name.Upper_Case) & ");");
        when others =>  raise Configuration_Error with "Not supported datatype: " &  Data_Type(Col.Type_Of)'Img;
      end case;
      Code_Debug(" -- Stop  Set_All_Columns_Incl_Pk.Set_Null");
    end Set_Null;

    ------------------------------------------------------

    procedure Set_Non_Null (Col : Table_Column_Type; Stm_Name : String) is
    begin
      Code_Debug(" -- Start Set_All_Columns_Incl_Pk.Set_Non_Null");
      case Data_Type(Col.Type_Of) is
        when A_Char .. A_Short_Code => Put_Line("      " & Stm_Name & ".Set(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Clob                 => Put_Line("      " & Stm_Name & ".Set_Clob(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Time                 => Put_Line("      " & Stm_Name & ".Set_Time(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Date                 => Put_Line("      " & Stm_Name & ".Set_Date(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Timestamp            => Put_Line("      " & Stm_Name & ".Set_Timestamp(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when others =>  raise Configuration_Error with "Not supported datatype: " &  Data_Type(Col.Type_Of)'Img;
      end case;
      Code_Debug(" -- Stop  Set_All_Columns_Incl_Pk.Set_Non_Null");
    end Set_Non_Null;
    ------------------------------------------------------


    ----------------- Start Set_All_Columns_Incl_Pk -------------------------------------

  begin
    Code_Debug(" -- Start Set_All_Columns_Incl_Pk");

    if Set_Old_IXX then
      case Self.Ixx_Type is
        when None      => null;
        when Date_Time =>
          Put_Line("    " & Stm_Name & ".Set(" & Quote("OLD_IXXLUPD") & ", Data.Ixxlupd );");
          Put_Line("    " & Stm_Name & ".Set_Time(" & Quote("OLD_IXXLUTI") & ", Data.Ixxluti );");
          Put_Line("    " & Stm_Name & ".Set_Date(" & Quote("OLD_IXXLUDA") & ", Data.Ixxluda );");
        when Timestamp =>
          Put_Line("    " & Stm_Name & ".Set(" & Quote("OLD_IXXLUPD") & ", Data.Ixxlupd );");
          Put_Line("    " & Stm_Name & ".Set_Timestamp(" & Quote("OLD_IXXLUTS") & ", Data.Ixxluts );");
      end case;
    end if;
    Put_Line("");
    Put_Line("    if not Keep_Timestamp then");
    case Self.Ixx_Type is
      when None      =>
        Put_Line("      null; --for tables without Ixx*");
      when Date_Time =>
        Put_Line("      Data.Ixxluda := Now;");
        Put_Line("      Data.Ixxluti := Now;");
      when Timestamp =>
        Put_Line("      Data.Ixxluts := Now;");
    end case;
    Put_Line("    end if;");


    for Col of Self.Columns  loop
      if Col.Nullable then

        if Col.Name.Lower_Case = "ixxluda" or else
          Col.Name.Lower_Case = "ixxluti" then
          Put_Line("    if not Keep_Timestamp then");
          Put_Line("      null; --for tables without Ixxlud* ");
          Put_Line("      Data." & Col.Name.Camel_Case & " := Now;");
          Put_Line("    end if;");
          Set_Non_Null(Col,Stm_Name);
        else
          case Data_Type(Col.Type_Of) is
            when A_Boolean    =>
              Put_Line("  -- call  Set_Non_Null start boolean");
              Set_Non_Null(Col,Stm_Name);
              Put_Line("  -- call  Set_Non_Null stop boolean");
            when others =>
              Put_Line("  -- call  not boolean");

              case Data_Type(Col.Type_Of) is
                when A_Char    =>
                  Put_Line("    if " & "Data." & Col.Name.Camel_Case & " = " &  Null_Value_For_Type_At_Comparison(Data_Type(Col.Type_Of), Col.Size_Of, "Data." & Col.Name.Fix_String) & " then");
                when others    => null;
                  Put_Line("    if " & "Data." & Col.Name.Camel_Case & " = " &  Null_Value_For_Type_At_Comparison(Data_Type(Col.Type_Of), Col.Size_Of, Col.Name.Fix_String) & " then");
              end case;
              Set_Null(Col,Stm_Name);
              Put_Line("    else");
              Set_Non_Null(Col,Stm_Name);
              Put_Line("    end if;");
          end case;
        end if;

      else --not nullable

        if Col.Name.Lower_Case = "ixxlupd" then
          Put_Line("    if not Keep_Timestamp then");
          Put_Line("      null; --for tables without Ixxlupd");
          Put_Line("      Data." & Col.Name.Camel_Case & " := Process.Name(1..15);");
          Put_Line("    end if;");
          Set_Non_Null(Col,Stm_Name);
        else
          Set_Non_Null(Col,Stm_Name);
        end if;
      end if;
    end loop;

    Code_Debug(" -- Stop  Set_All_Columns_Incl_Pk");
  end Set_All_Columns_Incl_Pk;



---------------------------------------------------------------------------------------------

  -- start auto generating Ada packages --
  -- This section contains
  -- Print_Ada -
  --     Print_Ada_Spec
  --         Print_Header_Spec
  --         Print_Withs_Spec
  --         Print_Package_Start_Spec
  --         Print_Def_Functions_Spec
  --         Print_Ud4_Functions_Spec
  --         Print_XML_Functions_Spec
  --         Print_Def_Functions_Lists_Spec
  --         Print_XML_Functions_Lists_Spec
  --         Print_Package_End_Spec
  --     Print_Ada_Body
  --         Print_Header_Body
  --         Print_Withs_Body
  --         Print_Package_Start_Body
  --         Print_Def_Functions_Body
  --         Print_Ud4_Functions_Body
  --         Print_XML_Functions_Body
  --         Print_Package_End_Body


  -- Start Headers  --

  procedure Print_Header_Spec(Self : in out Table_Type ) is
    pragma Unreferenced(Self);
    use Text_Io;
  begin
    Put_Line(Header.Fix_String);
    Put_Line("");
  end Print_Header_Spec;
  --------------------------------
  procedure Print_Withs_Spec(Self : in out Table_Type ) is
    pragma Unreferenced(Self);
    use Text_Io;
  begin
    Code_Debug(" -- start Print_Withs_Spec");
    --Put_Line("pragma Warnings(Off);");
    Put_Line("with Ada.Containers.Doubly_Linked_Lists;");
    Put_Line("with Ada.Containers.Hashed_Maps;");
    Put_Line("with Ada.Containers.Ordered_Maps;");
    Put_Line("with Ada.Strings;");
    Put_Line("with Ada.Strings.Hash;");
    Put_Line("with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;");
    Put_Line("with Gnatcoll.Json; use Gnatcoll.Json;");
    Put_Line("with Types, Calendar2, Sql;");
    Put_Line("with Table_Utils;  --All tables inherit from here");

    Put_Line("");
    Put_Line("pragma Elaborate_All(Calendar2);");
    Put_Line("");
    Code_Debug(" -- stop Print_Withs_Spec");
    Put_Line("");
  end Print_Withs_Spec;
  --------------------------------
  procedure Print_Package_Start_Spec(Self : in out Table_Type ) is
    use Text_Io;
    Tmp,
    Null_Data,
    String_Range,
    Comment,
    Dt_Type : String_Object;
    Type_Used : Data_Type_Type := Data_Type_Type'First;

    Index_Counter : Integer_4 := 0;

  begin
    Code_Debug(" -- start Print_Package_Start_Spec");
    Put_Line("");

    Put_Line("package Table_" & Self.Name.Camel_Case & " is");
    Put_Line("  use Types, Calendar2;");

    Put_Line("  --");
    Put_Line("  --  Table name as string ");
    Put_Line("  --");

    Put_Line("");
    Put_Line("  Table_" & Self.Name.Camel_Case & "_Name     : constant String := " & Quote(Self.Name.Upper_Case) & ";");
    Put_Line("  Table_" & Self.Name.Camel_Case & "_Set_Name : constant String := " & Quote(Self.Name.Upper_Case & "_SET") & ";");
    Put_Line("  Table_" & Self.Name.Camel_Case & "_Row_Name : constant String := " & Quote(Self.Name.Upper_Case & "_ROW") & ";");
    Put_Line("");

    Put_Line("  --");
    Put_Line("  -- Column names as strings");
    Put_Line("  --");
    Put_Line("");

    for Col of Self.Columns loop
      Put_Line("  " & Col.Name.Camel_Case & "_Name : constant String := " & Quote(Col.Name.Upper_Case) & ";");
    end loop;

    Put_Line("  --");
    Put_Line("  -- Column names as enumerator literals");
    Put_Line("  --");
    Put_Line("");

    Tmp.Append("  type Column_Type is (" & Ascii.Lf);
    for Col of Self.Columns loop
      Tmp.Append("    " & Col.Name.Camel_Case & "," & Ascii.Lf);
    end loop;
    -- replace last ',' + LF with ");"
    declare
      Str_Tmp : String := Tmp.Fix_String;
    begin
      Str_Tmp(Str_Tmp'Last -1) := ')';
      Str_Tmp(Str_Tmp'Last)    := ';';
      Put_Line(Str_Tmp);
    end;

    Put_Line("  --");
    Put_Line("  -- The type definition");
    Put_Line("  --");
    Put_Line("");

    Put_Line("  type Data_Type is new Table_Utils.Root_Table_Type with record");

    -- loop over all columns and write like
    --  Bsqpsto :    String (1..2) := (others => ' ') ; -- Primary Key

    for Col of Self.Columns loop
      -- Code_Debug(" -- " & Col.Name.Camel_Case);
      Feedback(Col.Name.Upper_Case & Col.Type_Of'Img);

      Type_Used := Data_Type(Col.Type_Of);
      String_Range.Set(""); --reset
      case Data_Type(Col.Type_Of) is
        when A_Char    =>
          if Col.Size_Of > 1 then
            String_Range.Set("(1 .." & Col.Size_Of'Img & ")");
          end if;
--        when A_Boolean => Type_Used := A_Int; -- 0/1 in db
        when others    => null;
      end case;

      Dt_Type.Set(To_Ada_Type(Type_Used, Col.Size_Of));
      Null_Data.Set(Null_Value(Type_Used, Col.Size_Of));

      if Col.Primary then
        Index_Counter := Index_Counter + 1;
        Comment.Set("-- Primary Key");
      elsif Col.Unique then
        Index_Counter := Index_Counter + 1;
        Comment.Set("-- unique index" & Index_Counter'Img);
      elsif Col.Indexed then
        Index_Counter := Index_Counter + 1;
        Comment.Set("-- non unique index" & Index_Counter'Img);
      else
        Comment.Set("");
      end if;
      Put_Line("    " &
                  Col.Name.Camel_Case & " : " &
                  Dt_Type.Fix_String &
                  String_Range.Fix_String &
                  " := " &
                  Null_Data.Fix_String &
                  " ; " &
                  Comment.Fix_String );
    end loop;
    Put_Line("  end record;");
    Put_Line("");
    Code_Debug(" -- stop Print_Package_Start_Spec");

  end Print_Package_Start_Spec;
  --------------------------------
  procedure Print_Def_Functions_Spec(Self : in out Table_Type ) is
    use Text_Io;
  --##--##--##--##--##--##--##--##--

    procedure Index_Procs_Spec(Idx : I.Index_Type ; Table_Name : String_Object) is
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy;
      Idx_Column_List_2        : C.Columns_Type := Idx.Column_List.Copy;
      All_Fields               : String_Object;
      Tmp                      : String_Object;
    begin
      Code_Debug(" -- start Print_Def_Functions_Spec.Index_Procs_Spec");

      if Index_Has_Several_Fields then

        for Idx_Col of Idx.Column_List loop
          All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
        end loop;
        Put_Line("  procedure Read_One" & All_Fields.Camel_Case & "(");
        Put_Line("                           Data       : in out Table_" & Table_Name.Camel_Case & ".Data_Type;"); --'class
        Put_Line("                           Order      : in     Boolean := False;");
        Put_Line("                           End_Of_Set : in out Boolean);");
        Put_Line("");

        Put_Line("");
        Put_Line("  -- non unique index");
        Put_Line("  procedure Delete" & All_Fields.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type);");
        Put_Line("  --------------------------------------------");

        for Idx_Col of Idx_Column_List_1 loop
          declare
            Cnt : Integer := 0;
          begin
            Tmp.Set(Tmp.Camel_Case & "_" & Idx_Col.Name.Camel_Case );

            Put_Line("");
            Put_Line("  -- non unique index");
            Put_Line("  procedure Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type);");

            Put_Line("  -- non unique index");
            Put_Line("  function Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) & "(");

            Cnt := Integer(Idx_Column_List_2.Length);
            for Idx_Col_2 of Idx_Column_List_2 loop
              if Cnt > 1 then
                Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ";");
              else
                Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ") return Boolean;");
              end if;
              Cnt := Cnt -1;
            end loop;
            Idx_Column_List_2.Delete_Last;
          end;

        end loop;
        Idx_Column_List_1.Clear;
        Idx_Column_List_2.Clear;

      else -- 1 field only in index (Index_Has_Several_Fields)
        declare
          Idx_Col : C.Column_Type := Idx.Column_List.First_Element ;
        begin
          Put_Line("");
          Put_Line("  -- non unique index");
          Put_Line("  procedure Read_One_" & Idx_Col.Name.Camel_Case & "(");
          Put_Line("                           Data       : in out Table_" & Table_Name.Camel_Case & ".Data_Type;");
          Put_Line("                           Order      : in     Boolean := False;");
          Put_Line("                           End_Of_Set : in out Boolean);");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("  -- non unique index");
          Put_Line("  function Count_" & Idx_Col.Name.Camel_Case & "(Data : Table_" & Table_Name.Camel_Case & ".Data_Type) return Integer_4;");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("  -- non unique index");
          Put_Line("  procedure Delete_" & Idx_Col.Name.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type);");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("  -- non unique index");
          Put_Line("  function Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) &  "(" & Idx_Col.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col.Type_Of), Idx_Col.Size_Of) & ") return Boolean;");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("");
        end ;
      end if; -- Index_Has_Several_Fields
      Code_Debug(" -- stop Print_Def_Functions_Spec.Index_Procs_Spec");
    end Index_Procs_Spec;

    --##--##--##--##--##--##--##--##--

    procedure Unique_Procs_Spec(Idx : I.Index_Type ; Table_Name : String_Object) is
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy;
      Idx_Column_List_2        : C.Columns_Type := Idx.Column_List.Copy;
      All_Fields               : String_Object;
      Tmp                      : String_Object;
    begin
      Code_Debug(" -- start Print_Def_Functions_Spec.Unique_Procs_Spec");
      for Idx_Col of Idx.Column_List loop
        All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
      end loop;

      if Index_Has_Several_Fields then

        Put_Line("  procedure Read_One" & All_Fields.Camel_Case & "(");
        Put_Line("                           Data       : in out Table_" & Table_Name.Camel_Case & ".Data_Type;"); --'class
        Put_Line("                           Order      : in     Boolean := False;");
        Put_Line("                           End_Of_Set : in out Boolean);");
        Put_Line("");

        Put_Line("");
        Put_Line("  -- unique index");
        Put_Line("  procedure Delete" & All_Fields.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type);");
        Put_Line("  --------------------------------------------");

        for Idx_Col of Idx_Column_List_1 loop
          declare
            Cnt : Integer := 0;
          begin
            Tmp.Set(Tmp.Camel_Case & "_" & Idx_Col.Name.Camel_Case );

            Put_Line("");
            Put_Line("  -- unique index");
            Put_Line("  procedure Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type);");

            Put_Line("  -- unique index");
            Put_Line("  function Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) & "(");

            Cnt := Integer(Idx_Column_List_2.Length);
            for Idx_Col_2 of Idx_Column_List_2 loop
              if Cnt > 1 then
                Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ";");
              else
                Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ") return Boolean;");
              end if;
              Cnt := Cnt -1;
            end loop;
            Idx_Column_List_2.Delete_Last;
          end;

        end loop;
        Idx_Column_List_1.Clear;
        Idx_Column_List_2.Clear;

      else -- 1 field only in index (Index_Has_Several_Fields)
        declare
          Idx_Col : C.Column_Type := Idx.Column_List.First_Element ;
        begin

          Put_Line("");
          Put_Line("  -- unique index");
          Put_Line("  procedure Read" & All_Fields.Camel_Case & "(");
          Put_Line("                           Data       : in out Table_" & Table_Name.Camel_Case & ".Data_Type;");
          Put_Line("                           End_Of_Set : in out Boolean);");
          Put_Line("  --------------------------------------------");



          Put_Line("");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("  -- unique index");
          Put_Line("  function Count_" & Idx_Col.Name.Camel_Case & "(Data : Table_" & Table_Name.Camel_Case & ".Data_Type) return Integer_4;");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("  -- unique index");
          Put_Line("  procedure Delete_" & Idx_Col.Name.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type);");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("  -- unique index");
          Put_Line("  function Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) &  "(" & Idx_Col.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col.Type_Of), Idx_Col.Size_Of) & ") return Boolean;");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("");
        end ;
      end if; -- Index_Has_Several_Fields
    Code_Debug(" -- stop Print_Def_Functions_Spec.Unique_Procs_Spec");
  end Unique_Procs_Spec;

    --##--##--##--##--##--##--##--##--

    procedure Primary_Procs_Spec(Idx : I.Index_Type; Table_Name : String_Object) is
      All_Fields               : String_Object;
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy  with Warnings => Off;
      Idx_Column_List_2        : C.Columns_Type with Warnings => Off; -- this need to be empty
    begin
    -- for pk's with several fields
      Code_Debug(" -- start Print_Def_Functions_Spec.Primary_Procs_Spec");

      if not Index_Has_Several_Fields then
        -- only one keyfield -> return
        Put_Line("   -- stop Print_Def_Functions_Spec.Primary_Procs_Spec");
        return ;
      end if;

      for Idx_Col of Idx.Column_List loop
        All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
      end loop;

      Put_Line("");
      Put_Line("  -- primary key index several fields");
      Put_Line("  procedure Delete" & All_Fields.Camel_Case & "(Data  : in Table_" & Table_Name.Camel_Case & ".Data_Type);");
      Put_Line("  --------------------------------------------");
      Put_Line("");

      All_Fields.Set("");
      for Idx_Col of Idx_Column_List_1 loop
        declare
          Cnt : Integer := 0;
        begin
          All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
          Idx_Column_List_2.Append(Idx_Col);
          Put_Line("");
          Put_Line("  -- part of primary key index");
          Put_Line("  procedure Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & All_Fields.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type);");

          Put_Line("");
          Put_Line("  -- part of primary key index)");
          Put_Line("  function Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) &  "(");

          Cnt := Integer(Idx_Column_List_2.Length);
          for Idx_Col_2 of Idx_Column_List_2 loop
            if Cnt > 1 then
              Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ";");
            else
              Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ") return Boolean;");
            end if;
            Cnt := Cnt -1;
          end loop;
        end;
      end loop;
      Idx_Column_List_1.Clear;
      Idx_Column_List_2.Clear;

      Code_Debug(" -- stop Print_Def_Functions_Spec.Primary_Procs_Spec");
    end Primary_Procs_Spec;
    --    ##--##--##--##--##--##--##--##--

    begin
     Code_Debug(" -- start Print_Def_Functions_Spec");
     Put_Line("");

    if Self.Entity_Type = Ud4 then
      -- only for db
      Code_Debug("  -- stop Print_Def_Functions_Spec");
      return ;
    end if;

    Put_Line("  -- Procedures for all DBMS");

--  #Functions operating on Primary Key
    Put_Line("");
    Put_Line("  -- Procedures for DBMS SQL");
    -- ##############################################################
    Put_Line("  -- Primary key");
    Put_Line("  function Get(Stm : in Sql.Statement_Type) return Table_" & Self.Name.Camel_Case & ".Data_Type;");
    Put_Line("  --------------------------------------------");
    -- ##############################################################
    Put_Line("  -- Primary key");
    Put_Line("  procedure Read(Data       : in out Table_" & Self.Name.Camel_Case & ".Data_Type;");
    Put_Line("                 End_Of_Set : in out Boolean);");
    Put_Line("  --------------------------------------------");

    --  ##############################################################
    Put_Line("  -- Primary key");
    Put_Line("  function Is_Existing(");
    declare
      Tmp  : String_Object ;
      Tmp2 : Unbounded_String;
    begin
      for Col of Self.Columns loop
      -- the col, based on col_name
        if Col.Primary then
          Tmp.Append("                       " & Col.Name.Camel_Case & " : " & To_Ada_Type(Data_Type(Col.Type_Of), Col.Size_Of) & ";" & Ascii.Lf);
        end if;
      end loop;
      Tmp2 := Tmp.UBString;
      -- kill last ascii.lf
       Replace_Element(Source => Tmp2,
                       Index  => Length(Tmp2),
                       By     => ' ');
      -- kill last ';'
       Replace_Element(Source => Tmp2,
                       Index  => Length(Tmp2) -1 ,
                       By     => ')');
       Put_Line(To_String(Tmp2));
       Put_Line("                           return Boolean;");
    end;
    Put_Line("  --------------------------------------------");


    --  ##############################################################
    Put_Line("  -- Primary key");
    Put_Line("  function Get(");
    declare
      Tmp  : String_Object ;
    begin
      for Col of Self.Primary_Column_List loop
        Tmp.Append("                       " & Col.Name.Camel_Case & " : " & To_Ada_Type(Data_Type(Col.Type_Of), Col.Size_Of) & ";" & Ascii.Lf);
      end loop;
      -- kill last ascii.lf
      Tmp.Delete_Last_Char;
      -- kill last ';'
      Tmp.Delete_Last_Char;
      Tmp.Append(")");
       Put_Line(Tmp.Fix_String);
       Put_Line("                           return Table_" & Self.Name.Camel_Case & ".Data_Type;");
    end;
    Put_Line("  --------------------------------------------");

--    ##############################################################
--
    Put_Line("  -- Primary key");
    Put_Line("  procedure Delete(Data : in Table_" & Self.Name.Camel_Case & ".Data_Type);");
    Put_Line("  --------------------------------------------");
--    ##############################################################

    if not Self.All_Columns_Are_Primary then
    Put_Line("  -- Primary key");
      Put_Line("  procedure Update(Data : in out Table_" & Self.Name.Camel_Case & ".Data_Type; Keep_Timestamp : in Boolean := False);");
      Put_Line("  --------------------------------------------");
    end if;

--    ##############################################################

    Put_Line("  -- Primary key");
    Put_Line("  procedure Insert(Data : in out Table_" & Self.Name.Camel_Case & ".Data_Type; Keep_Timestamp : in Boolean := False);");
    Put_Line("  --------------------------------------------");
--
--    ##############################################################
    case Self.Ixx_Type is
      when None      => null;
      when Date_Time |
           Timestamp =>
           Put_Line("  -- Primary key");
           Put_Line("  procedure Delete_Withcheck(Data : in Table_" & Self.Name.Camel_Case & ".Data_Type);");
           Put_Line("  --------------------------------------------");

           if not Self.All_Columns_Are_Primary then
             Put_Line("  -- Primary key");
             Put_Line("  procedure Update_Withcheck(Data : in out Table_" & Self.Name.Camel_Case & ".Data_Type; Keep_Timestamp : in Boolean := False);");
             Put_Line("  --------------------------------------------");
           end if;
    end case;
--    ##############################################################

    for Idx of Self.Indicies loop
      case Idx.Type_Of is
        when I.Primary =>  Primary_Procs_Spec(Idx, Self.Name) ;
        when I.Unique  =>  Unique_Procs_Spec(Idx, Self.Name) ;
        when I.Index   =>  Index_Procs_Spec(Idx, Self.Name) ;
        when I.Functional => null;
      end case;
      Put_Line("");
    end loop;

    Code_Debug("  -- stop Print_Def_Functions_Spec");
    Put_Line("");

  end Print_Def_Functions_Spec;
  --------------------------------
  procedure Print_Ud4_Functions_Spec(Self : in out Table_Type ) is
    use Text_Io;
  begin
    Code_Debug(" -- start Print_Ud4_Functions_Spec");
    Put_Line("");

    Put_Line("");
    Put_Line("  -- Procedures for DBMS UD4");
    Put_Line("");

    Put_Line("  --------------------------------------------");
    Put_Line("  procedure Get_Values(Request : in     Request_Type;");
    Put_Line("                       Data    : in out Table_" & Self.Name.Camel_Case & ".Data_Type);");
    Put_Line("  --------------------------------------------");
    Put_Line("");
    Put_Line("  procedure Set_Values(Reply  : in out Request_Type;");
    Put_Line("                       Data   : in     Table_" & Self.Name.Camel_Case & ".Data_Type);");
    Put_Line("  --------------------------------------------");
    Put_Line("");
    Put_Line("  procedure Make_Ud4_Telegram(Request   : in out Uniface_Request.Request_Type;");
    Put_Line("                              Operation : in     Operation_Type := Get_One_Record);");
    Put_Line("  --------------------------------------------");
    Put_Line("");
    Put_Line("  procedure Make_Ud4_Telegram(Request   : in out Uniface_Request.Request_Type;");
    Put_Line("                              Data      : in     Table_" & Self.Name.Camel_Case & ".Data_Type;");
    Put_Line("                              Operation : in     Operation_Type := Get_One_Record);");
    Put_Line("  --------------------------------------------");
    Put_Line("");
    Put_Line("");
    Code_Debug(" -- stop Print_Ud4_Functions_Spec");
    Put_Line("");
  end Print_Ud4_Functions_Spec;
  --------------------------------
  procedure Print_XML_Functions_Spec(Self : in out Table_Type ) is
    use Text_Io;
  begin
    Put_Line("");
    Code_Debug(" -- start Print_XML_Functions_Spec");
    Put_Line("");
    Put_Line("  -- Procedures for all DBMS, primitives");
    Put_Line("");
    Put_Line("  function To_String(Data : in Table_" & Self.Name.Camel_Case & ".Data_Type) return String;");
    Put_Line("");
    Put_Line("  function To_JSON(Data : in Table_" & Self.Name.Camel_Case & ".Data_Type) return JSON_Value;");
    Put_Line("");
    Put_Line("  function From_JSON(JSON_Data : in JSON_Value) return Table_" & Self.Name.Camel_Case & ".Data_Type;");
    Put_Line("");
    Put_Line("  function To_Xml(Data      : in Table_" & Self.Name.Camel_Case & ".Data_Type;");
    Put_Line("                  Ret_Start : in Boolean;");
    Put_Line("                  Ret_Data  : in Boolean;");
    Put_Line("                  Ret_End   : in Boolean) return String;");
    Put_Line("");
--    Put_Line("  package " & Self.Name.Camel_Case & "_List_Pack is new Simple_List_Class(Table_" & Self.Name.Camel_Case & ".Data_Type);");
    Put_Line("");
    Put_Line("  package " & Self.Name.Camel_Case & "_List_Pack2 is new Ada.Containers.Doubly_Linked_Lists(Table_" & Self.Name.Camel_Case & ".Data_Type);");


    Put_Line("");
    Put_Line("  subtype Key_Type is String(1..15);");
    Put_Line("");
    Put_Line("  package " &  Self.Name.Camel_Case & "_Map_Pack_String is new Ada.Containers.Hashed_Maps(");
    Put_Line("      Key_Type,");
    Put_Line("      Unbounded_String,");
    Put_Line("      Ada.Strings.Hash,");
    Put_Line("      " & Quote("=") & ",");
    Put_Line("      " & Quote("=") & ");");
    Put_Line("");
    Put_Line("  package " &  Self.Name.Camel_Case & "_Map_Pack_Column_Type is new Ada.Containers.Ordered_Maps(");
    Put_Line("      Column_Type,");
    Put_Line("      Unbounded_String,");
    Put_Line("      " & Quote("<") & ",");
    Put_Line("      " & Quote("=") & ");");
    Put_Line("");

    Code_Debug(" -- stop Print_XML_Functions_Spec");


    Put_Line("");
  end Print_XML_Functions_Spec;
  --------------------------------
  procedure Print_Def_Functions_Lists_Spec(Self : in out Table_Type ) is
    use Text_Io;
  --##--##--##--##--##--##--##--##--

    procedure Index_Procs_Lists_Spec(Idx : I.Index_Type; Table_Name : String_Object) is
      Fields, All_Fields       : String_Object;
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
    begin
      Code_Debug(" -- start Print_Def_Functions_Lists_Spec.Index_Procs_Lists_Spec");

      for Idx_Col of Idx.Column_List loop
        All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
      end loop;

      Put_Line("");
      Put_Line("");
      Put_Line("");
      Put_Line("  procedure Read" & All_Fields.Camel_Case & "(");
      Put_Line("                           Data  : in out Table_" & Table_Name.Camel_Case & ".Data_Type'class;");
      Put_Line("                           List  : in out " & Table_Name.Camel_Case & "_List_Pack2.List;");
      Put_Line("                           Order : in     Boolean := False;");
      Put_Line("                           Max   : in     Integer_4 := Integer_4'Last);");
      Put_Line("");
      Put_Line("");

      if Index_Has_Several_Fields then
        -- we do not want the last idx field.
        -- however if we have 3 fields in idx
        --  is_existing uses
        -- Read_I1_Fname_1
        -- Read_11_Fname_1_Fname_2
        -- Read_Fname_1_Fname_2_Fname3
        -- so always generate
        --Read_Fname_1_Fname_2_Fname3 and Read_I1_Fname_1_Fname_2_Fname3
        -- so we do not use
        -- Idx_Column_List_1.Delete_Last;

        -- do the loop that prints stuff
        for Idx_Col of Idx_Column_List_1 loop
          Fields.Set(Fields.Camel_Case & '_' & Idx_Col.Name.Camel_Case);

          Put_Line("  procedure Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & "(");
          Put_Line("                           Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type'class;");
          Put_Line("                           List  : in out " & Table_Name.Camel_Case & "_List_Pack2.List;");
          Put_Line("                           Order : in     Boolean := False;");
          Put_Line("                           Max   : in     Integer_4 := Integer_4'Last);");
          Put_Line("");
        end loop ;
      end if; --Index_Has_Several_Fields

      Idx_Column_List_1.Clear;
      Code_Debug(" -- stop Print_Def_Functions_Lists_Spec.Index_Procs_Lists_Spec");
      Put_Line("");
    end Index_Procs_Lists_Spec;

    --##--##--##--##--##--##--##--##--

    procedure Unique_Procs_Lists_Spec(Idx : I.Index_Type ; Table_Name : String_Object) is
      Fields,
      All_Fields               : String_Object;
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
    begin
      Code_Debug(" -- start Print_Def_Functions_Lists_Spec.Unique_Procs_Lists_Spec");
      for Idx_Col of Idx.Column_List loop
        All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
      end loop;

      if Index_Has_Several_Fields then
        -- we do not want the last idx field.
        -- however if we have 3 fields in idx
        --  is_existing uses
        -- Read_I1_Fname_1
        -- Read_11_Fname_1_Fname_2
        -- Read_Fname_1_Fname_2_Fname3
        -- so always generate
        --Read_Fname_1_Fname_2_Fname3 and Read_I1_Fname_1_Fname_2_Fname3
        -- so we do not use
        -- Idx_Column_List_1.Delete_Last;

        -- do the loop that prints stuff
        for Idx_Col of Idx_Column_List_1 loop
          Fields.Set(Fields.Camel_Case & '_' & Idx_Col.Name.Camel_Case);

          Put_Line("");
          Put_Line("  procedure Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & "(");
          Put_Line("                           Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type'class;");
          Put_Line("                           List  : in out " & Table_Name.Camel_Case & "_List_Pack2.List;");
          Put_Line("                           Order : in     Boolean := False;");
          Put_Line("                           Max   : in     Integer_4 := Integer_4'Last);");
          Put_Line("");
        end loop ;

      end if; --Index_Has_Several_Fields

      Idx_Column_List_1.Clear;
      Code_Debug(" -- stop  Print_Def_Functions_Lists_Spec.Unique_Procs_Lists_Spec");
      Put_Line("");
    end Unique_Procs_Lists_Spec;

    --##--##--##--##--##--##--##--##--

    procedure Primary_Procs_Lists_Spec(Idx : I.Index_Type ; Table_Name : String_Object) is
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
      All_Fields, Fields       : String_Object;
    begin
      Code_Debug(" -- start Print_Def_Functions_Lists_Spec.Primary_Procs_Lists_Spec");
      if Index_Has_Several_Fields then

        for Idx_Col of Idx.Column_List loop
          All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
        end loop;

        -- we do not want the last idx field.
        -- however if we have 3 fields in idx
        --  is_existing uses
        -- Read_I1_Fname_1
        -- Read_11_Fname_1_Fname_2
        -- Read_Fname_1_Fname_2_Fname3
        -- so always generate
        --Read_Fname_1_Fname_2_Fname3 and Read_I1_Fname_1_Fname_2_Fname3
        -- so we do not use
        -- Idx_Column_List_1.Delete_Last;

        -- do the loop that prints stuff
        for Idx_Col of Idx_Column_List_1 loop
          -- exit on the last pk-column
          Fields.Set(Fields.Camel_Case & '_' & Idx_Col.Name.Camel_Case);

          Put_Line("");
          Put_Line("");
          Put_Line("  procedure Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & "(");
          Put_Line("                           Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type'class;");
          Put_Line("                           List  : in out " & Table_Name.Camel_Case & "_List_Pack2.List;");
          Put_Line("                           Order : in     Boolean := False;");
          Put_Line("                           Max   : in     Integer_4 := Integer_4'Last);");
          Put_Line("");
        end loop ;
      end if; --Index_Has_Several_Fields
      Idx_Column_List_1.Clear;

      Code_Debug("   -- stop Print_Def_Functions_Lists_Spec.Primary_Procs_Lists_Spec");
    end Primary_Procs_Lists_Spec;

    --    ##--##--##--##--##--##--##--##--

  begin --Print_Def_Functions_Lists_Spec
     Code_Debug(" -- start Print_Def_Functions_Lists_Spec");
     Put_Line("");

    if Self.Entity_Type = Ud4 then
      -- only for db
       Code_Debug(" -- stop  Print_Def_Functions_Lists_Spec");
      return ;
    end if;

    -- Always these
    Put_Line("");
    Put_Line("  function To_Map (Data : Table_" & Self.Name.Camel_Case & ".Data_Type'class) return Table_" & Self.Name.Camel_Case & "." & Self.Name.Camel_Case & "_Map_Pack_String.Map;");
    Put_Line("");
    Put_Line("");
    Put_Line("  function To_Map (Data : Table_" & Self.Name.Camel_Case & ".Data_Type'class) return Table_" & Self.Name.Camel_Case & "." & Self.Name.Camel_Case & "_Map_Pack_Column_Type.Map;");
    Put_Line("");
    Put_Line("  --------------------------------------------");
    Put_Line("");
    Put_Line("  --------------------------------------------");
    Put_Line("");
    Put_Line("  procedure Read_List(Stm  : in     Sql.Statement_Type;");
    Put_Line("                      List : in out " & Self.Name.Camel_Case & "_List_Pack2.List;");
    Put_Line("                      Max  : in     Integer_4 := Integer_4'Last);");
    Put_Line("");
    Put_Line("  --------------------------------------------");
    Put_Line("");
    Put_Line("");
    Put_Line("  procedure Read_All(List  : in out " & Self.Name.Camel_Case & "_List_Pack2.List;");
    Put_Line("                     Order : in     Boolean := False;");
    Put_Line("                     Max   : in     Integer_4 := Integer_4'Last);");
    Put_Line("  --------------------------------------------");

--    ##############################################################

    for Idx of Self.Indicies loop
      case Idx.Type_Of is
        when I.Primary => Primary_Procs_Lists_Spec(Idx, Self.Name) ;
        when I.Unique  => Unique_Procs_Lists_Spec(Idx, Self.Name) ;
        when I.Index   => Index_Procs_Lists_Spec(Idx, Self.Name) ;
        when I.Functional => null;
      end case;
      Put_Line("");
    end loop;

    Code_Debug("  -- stop Print_Def_Functions_Lists_Spec");
    Put_Line("");
  end Print_Def_Functions_Lists_Spec;
  --------------------------------
  procedure Print_XML_Functions_Lists_Spec(Self : in out Table_Type ) is
    use Text_Io;
  begin
    Code_Debug(" -- start Print_XML_Functions_Lists_Spec");
    Put_Line("");
    Put_Line("  -- Procedures for all DBMS, others ");
    Put_Line("");
    Put_Line("  procedure From_Xml(Xml_Filename : in Unbounded_String;");
    Put_Line("                     A_List       : in out " & Self.Name.Camel_Case & "_List_Pack2.List);");
    Put_Line("");
    Code_Debug(" -- stop Print_XML_Functions_Lists_Spec");
    Put_Line("");
  end Print_XML_Functions_Lists_Spec;
  --------------------------------
  procedure Print_Package_End_Spec(Self : in out Table_Type ) is
    use Text_Io;
  begin
    Code_Debug(" -- start Print_Package_End_Spec");
    Put_Line("  Empty_Data : Table_" & Self.Name.Camel_Case & ".Data_Type;");
    Put_Line("end Table_" & Self.Name.Camel_Case & " ;");
    Put_Line("");
--    Code_Debug(" -- stop Print_Package_End_Spec");
  end Print_Package_End_Spec;

  --------------------------------

  -- End Headers  --

  -- Start Bodies --
  procedure Print_Header_Body(Self : in out Table_Type ) is
    pragma Unreferenced(Self);
    use Text_Io;
  begin
    Put_Line(Header.Fix_String);
    Put_Line("");
  end Print_Header_Body;
  --------------------------------
  procedure Print_Withs_Body(Self : in out Table_Type ) is
    pragma Unreferenced(Self);
    use Text_Io;
  begin
    Code_Debug(" -- start Print_Withs_Body");
    Put_Line("");
    Put_Line("pragma Warnings(Off);");
    Put_Line("with Process_Io, Utils;");
    Put_Line("with Ada.Strings.Fixed; use Ada.Strings.Fixed;");
    Put_Line("with Sax.Readers;              use Sax.Readers;");
    Put_Line("with Input_Sources.File;       use Input_Sources.File;");
    Put_Line("with Unicode.CES;");
    Put_Line("with Unicode.Encodings;");
    Put_Line("with Sax.Attributes;");
    Put_Line("");
    Code_Debug(" -- stop Print_Withs_Body");
    Put_Line("");
  end Print_Withs_Body;
  --------------------------------
  procedure Print_Package_Start_Body(Self : in out Table_Type ) is
    use Text_Io;
    Idx_Fields_2 : String_Object;
  begin
    Code_Debug(" -- start Print_Package_Start_Body");
    Put_Line("");
    Put_Line("package body Table_" & Self.Name.Camel_Case & " is");
    if Self.Entity_Type = Ud4 then
      -- only for db
      Code_Debug(" -- stop Print_Package_Start_Body");
      return;
    end if;

    Put_Line("");
    Put_Line("  Stm_Select,");
    Put_Line("  Stm_Delete,");
    Put_Line("  Stm_Update,");
    Put_Line("  Stm_Insert,");
    Put_Line("  Stm_Select_All,");
    Put_Line("  Stm_Select_All_O  : Sql.Statement_Type;");
    Put_Line("");
    Put_Line("");

    case Self.Ixx_Type is
      when None                  => null;
      when Date_Time | Timestamp =>
        Put_Line("  Stm_Delete_With_Check,");
        Put_Line("  Stm_Update_With_Check  : Sql.Statement_Type;");
        Put_Line("");
        Put_Line("");
    end case;

    for Idx of Self.Indicies loop
      declare
        Idx_Fields : String_Object;
        use type Ada.Containers.Count_Type;
        Cnt : Ada.Containers.Count_Type := 0;
        Max : Ada.Containers.Count_Type := Idx.Column_List.Length;
      begin
        for Idx_Col of Idx.Column_List loop
          Cnt := Cnt +1;
          Idx_Fields.Set(Idx_Fields.Camel_Case & '_' & Idx_Col.Name.Camel_Case);
          case Idx.Type_Of is
            when I.Primary =>
              Put_Line("  -- Primary keys, when several fields");
              Put_Line("  Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & ",");
              Put_Line("  Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & "_O,");
              Put_Line("  Stm_Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & " : Sql.Statement_Type; ");
              if Max > 1 and then Cnt >= Max then -- if more than 1 field
                Put_Line("");
                Put_Line("  -- non unique index all fields");
                Idx_Fields_2.Set(Idx_Fields.Fix_String);
                Put_Line("  Stm_Delete" & Idx_Fields_2.Fix_String & ": Sql.Statement_Type;");
              end if;

            when I.Unique =>
              Put_Line("  -- unique index max/cnt" & max'Img & "/" & cnt'img);
              Put_Line("  Stm_Select_Count_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & "_Unique,");
              Put_Line("  Stm_Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & "_Unique,");
              Put_Line("  Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & "_Unique_O,");
              Put_Line("  Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & "_Unique : Sql.Statement_Type;");

              Put_Line("");
              Put_Line("  -- unique index all fields");
              Idx_Fields_2.Set(Idx_Fields.Fix_String);
              Put_Line("  Stm_Delete" & Idx_Fields_2.Fix_String & "_Unique: Sql.Statement_Type;");
              Put_Line("  Stm_Select" & Idx_Fields_2.Fix_String & "_Unique: Sql.Statement_Type;");
              Put_Line("  Stm_Select" & Idx_Fields_2.Fix_String & "_Unique_O : Sql.Statement_Type;");

            when I.Index =>
              Put_Line("  -- non unique index ");
              Put_Line("  Stm_Select_Count_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & ",");
              Put_Line("  Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & ",");
              Put_Line("  Stm_Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & ",");
              Put_Line("  Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Idx_Fields.Fix_String & "_O : Sql.Statement_Type;");
              if Max > 1 and then Cnt >= Max then -- if more than 1 field
                Put_Line("");
                Put_Line("  -- non unique index all fields");
                Idx_Fields_2.Set(Idx_Fields.Fix_String);
                Put_Line("  Stm_Delete" & Idx_Fields_2.Fix_String & ": Sql.Statement_Type;");
              end if;
            when I.Functional => null;
          end case;
          Put_Line("");
        end loop;
      end;
    end loop;
    Put_Line("");

    Code_Debug(" -- stop Print_Package_Start_Body");
    Put_Line("");
  end Print_Package_Start_Body;
  --------------------------------

  procedure Print_Def_Functions_Body(Self : in out Table_Type ) is
   -- use Text_Io;
    --------------------------------------------

    procedure Index_Procs_Body(Idx : I.Index_Type ; Table_Name : String_Object) is
      use Text_Io;
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
      Idx_Column_List_2        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
      Idx_Column_List_3        : C.Columns_Type with Warnings => Off; -- do not copy into this one !
      All_Fields               : String_Object;
      Stm, Tmp                 : String_Object;
    begin

      Code_Debug(" -- start Print_Def_Functions_Body.Index_Procs_Body");
      if Index_Has_Several_Fields then

        for Idx_Col of Idx.Column_List loop
          All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
        end loop;
        Stm.Set("Stm_Delete" & All_Fields.Camel_Case );
        Put_Line("");
        Put_Line("  -- non unique index");
        Put_Line("  procedure Delete" & All_Fields.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type) is");
        Put_Line("  begin");

        Index_Sql_Statement(T             => Self,
                            Stm_Name      => Stm.Fix_String,
                            First_Stm_Row => "delete from " & Table_Name.Upper_Case,
                            Order_By_PK   => False,
                            Field_List    => Idx.Column_List);

        Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx.Column_List );

        Put_Line("    " & Stm.Fix_String & ".Execute;");
        Put_Line("  end Delete" & All_Fields.Camel_Case &";");
        Put_Line("");
        Put_Line("  --------------------------------------------");
        Put_Line("");

        Put_Line("");
        Put_Line("  procedure Read_One" & All_Fields.Camel_Case & "(");
        Put_Line("                           Data       : in out Table_" & Table_Name.Camel_Case & ".Data_Type;");
        Put_Line("                           Order      : in     Boolean := False;");
        Put_Line("                           End_Of_Set : in out Boolean) is");
        Put_Line("    List : " & Table_Name.Camel_Case & "_List_Pack2.List;");
        Put_Line("  begin");
        Put_Line("    Data.Read" & All_Fields.Camel_Case & "(List, Order, 1);");
        Put_Line("    if List.Is_Empty then");
        Put_Line("      End_Of_Set := True;");
        Put_Line("    else");
        Put_Line("      End_Of_Set := False;");
        Put_Line("      Data := List.First_Element;");
        Put_Line("    end if;");
        Put_Line("    List.Clear;");
        Put_Line("  end Read_One" & All_Fields.Camel_Case & ";");
        Put_Line("");

        for Idx_Col of Idx_Column_List_1 loop
          declare
            Cnt : Integer := 0;
          begin
            Idx_Column_List_3.Append(Idx_Col);
            Tmp.Set(Tmp.Camel_Case & "_" & Idx_Col.Name.Camel_Case );
            Stm.Set("Stm_Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp.Camel_Case );

            Put_Line("");
            Put_Line("  -- non unique index");
            Put_Line("  procedure Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type) is");
            Put_Line("  begin");
            Index_Sql_Statement(T             => Self,
                                Stm_Name      => Stm.Fix_String,
                                First_Stm_Row => "delete from " & Table_Name.Upper_Case,
                                Order_By_PK   => False,
                                Field_List    => Idx_Column_List_3);

            Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx_Column_List_3 );

            Put_Line("    " & Stm.Fix_String & ".Execute;");
            Put_Line("  end Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp.Camel_Case & ";");
            Put_Line("  --------------------------------------------");
            Put_Line("");

            Put_Line("  -- non unique index");
            Put_Line("  function Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) &  "(");

            Cnt := Integer(Idx_Column_List_2.Length);
            for Idx_Col_2 of Idx_Column_List_2 loop
              if Cnt > 1 then
                Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ";");
              else
                Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ") return Boolean is");
              end if;
              Cnt := Cnt -1;
            end loop;

            Put_Line("    Data       : Table_" & Table_Name.Camel_Case & ".Data_Type;");
            Put_Line("    Is_Exist   : Boolean := False;");
            Put_Line("    List       : " & Table_Name.Camel_Case & "_List_Pack2.List;");
            Put_Line("  begin");
            declare
              Tmp2 : String_Object;
            begin
              for Idx_Col_2 of Idx_Column_List_2 loop
                Put_Line("    Data." & Idx_Col_2.Name.Camel_Case & " := "  & Idx_Col_2.Name.Camel_Case & ";");
                Tmp2.Set(Tmp2.Camel_Case & "_" & Idx_Col_2.Name.Camel_Case);
              end loop;
              Put_Line("    Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp2.Camel_Case & "(Data, List, False, 1);");
            end;
            Put_Line("    Is_Exist := not List.Is_Empty;");
            Put_Line("    List.Clear;");
            Put_Line("    return Is_Exist;");
            Put_Line("  end Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) & ";");
            Put_Line("  --------------------------------------------");
            Put_Line("");
            Idx_Column_List_2.Delete_Last;
          end;
        end loop;

      else -- 1 field only in index (Index_Has_Several_Fields)
        declare
          Idx_Col : C.Column_Type := Idx.Column_List.First_Element;
        begin
          Put_Line("");
          Put_Line("  -- non unique index");
          Put_Line("  procedure Read_One_" & Idx_Col.Name.Camel_Case & "(");
          Put_Line("                           Data       : in out Table_" & Table_Name.Camel_Case & ".Data_Type;");
          Put_Line("                           Order      : in     Boolean := False;");
          Put_Line("                           End_Of_Set : in out Boolean) is");
          Put_Line("    List : " & Table_Name.Camel_Case & "_List_Pack2.List;");
          Put_Line("  begin");
          Put_Line("    Data.Read_" & Idx_Col.Name.Camel_Case & "(List, Order, 1);");
          Put_Line("    if List.Is_Empty then");
          Put_Line("      End_Of_Set := True;");
          Put_Line("    else");
          Put_Line("      End_Of_Set := False;");
          Put_Line("      Data := List.First_Element;");
          Put_Line("    end if;");
          Put_Line("    List.Clear;");
          Put_Line("  end Read_One_" & Idx_Col.Name.Camel_Case & ";");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Stm.Set("Stm_Select_Count_I" & Utils.Trim(Idx.Sequence_Number'Img) & "_" & Idx_Col.Name.Camel_Case);

          Put_Line("  -- non unique index");
          Put_Line("  function Count_" & Idx_Col.Name.Camel_Case & "(Data : Table_" & Table_Name.Camel_Case & ".Data_Type) return Integer_4 is");
          Put_Line("    use Sql;");
          Put_Line("    Count       : Integer_4 := 0;");
          Put_Line("    End_Of_Set  : Boolean := False;");
          Put_Line("    Start_Trans : constant Boolean := Sql.Transaction_Status = Sql.None;");
          Put_Line("    Transaction : Sql.Transaction_Type;");
          Put_Line("  begin");
          Put_Line("    if Start_Trans then Transaction.Start; end if;");
          Put_Line("");

          Index_Sql_Statement(T             => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "select count('a') from " & Table_Name.Upper_Case,
                              Order_By_PK   => False,
                              Field_List    => Idx.Column_List);

          Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx.Column_List );

          Put_Line("");
          Put_Line("    " & Stm.Fix_String & ".Open_Cursor;");
          Put_Line("    " & Stm.Fix_String & ".Fetch(End_Of_Set);");
          Put_Line("    if not End_Of_Set then");
          Put_Line("      " & Stm.Fix_String & ".Get(1, Count);");
          Put_Line("    end if;");
          Put_Line("    " & Stm.Fix_String & ".Close_Cursor;");
          Put_Line("    if Start_Trans then Transaction.Commit; end if;");
          Put_Line("    return Count;");
          Put_Line("  end Count_" & Idx_Col.Name.Camel_Case &";");
          Put_Line("  --------------------------------------------");
          Put_Line("");

          Stm.Set("Stm_Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & "_" & Idx_Col.Name.Camel_Case );
          Put_Line("  -- non unique index");
          Put_Line("  procedure Delete_" & Idx_Col.Name.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type) is");
          Put_Line("  begin");
          Put_Line("");

          Index_Sql_Statement(T             => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "delete from " & Table_Name.Upper_Case,
                              Order_By_PK   => False,
                              Field_List    => Idx.Column_List);

          Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx.Column_List );

          Put_Line("");
          Put_Line("    " & Stm.Fix_String & ".Execute;");
          Put_Line("  end Delete_" & Idx_Col.Name.Camel_Case &";");
          Put_Line("");

          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("  -- non unique index");
          Put_Line("  function Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) &  "(" & Idx_Col.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col.Type_Of), Idx_Col.Size_Of) & ") return Boolean is");
          Put_Line("    Data       : Table_" & Table_Name.Camel_Case & ".Data_Type;");
          Put_Line("    Exists     : Boolean := False;");
          Put_Line("    List       : " & Table_Name.Camel_Case & "_List_Pack2.List;");
          Put_Line("  begin");
          Put_Line("    Data." & Idx_Col.Name.Camel_Case & " := " & Idx_Col.Name.Camel_Case & ";");
          Put_Line("    Read_" & Idx_Col.Name.Camel_Case & "(Data, List, False, 1);");
          Put_Line("    Exists := not List.Is_Empty;");
          Put_Line("    List.Clear;");
          Put_Line("    return Exists;");
          Put_Line("  end Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) & ";");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("");
        end ;
      end if; -- Index_Has_Several_Fields

     Idx_Column_List_1.Clear;
     Idx_Column_List_2.Clear;
     Idx_Column_List_3.Clear;

      Code_Debug(" -- stop Print_Def_Functions_Body.Index_Procs_Body");
    end Index_Procs_Body;
    --++--++--++--++--++--++--++--++--++--++--++--++--++--++--

    procedure Unique_Procs_Body(Idx : I.Index_Type ; Table_Name : String_Object) is
      use Text_Io;
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
      Idx_Column_List_2        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
      Idx_Column_List_3        : C.Columns_Type with Warnings => Off; -- do not copy into this one !
      All_Fields               : String_Object;
      Stm, Tmp                 : String_Object;
    begin

      Code_Debug(" -- start Print_Def_Functions_Body.Unique_Procs_Body");
      if Index_Has_Several_Fields then

        for Idx_Col of Idx.Column_List loop
          All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
        end loop;
        Stm.Set("Stm_Delete" & All_Fields.Camel_Case & "_Unique");
        Put_Line("");
        Put_Line("  --  unique index");
        Put_Line("  procedure Delete" & All_Fields.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type) is");
        Put_Line("  begin");

        Index_Sql_Statement(T             => Self,
                            Stm_Name      => Stm.Fix_String,
                            First_Stm_Row => "delete from " & Table_Name.Upper_Case,
                            Order_By_PK   => False,
                            Field_List    => Idx.Column_List);

        Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx.Column_List );

        Put_Line("    " & Stm.Fix_String & ".Execute;");
        Put_Line("  end Delete" & All_Fields.Camel_Case &";");
        Put_Line("");
        Put_Line("  --------------------------------------------");
        Put_Line("");

        Put_Line("");
        Put_Line("  procedure Read_One" & All_Fields.Camel_Case & "(");
        Put_Line("                           Data       : in out Table_" & Table_Name.Camel_Case & ".Data_Type;");
        Put_Line("                           Order      : in     Boolean := False;");
        Put_Line("                           End_Of_Set : in out Boolean) is");
        Put_Line("    List : " & Table_Name.Camel_Case & "_List_Pack2.List;");
        Put_Line("  begin");
        Put_Line("    Data.Read" & All_Fields.Camel_Case & "(List, Order, 1);");
        Put_Line("    if List.Is_Empty then");
        Put_Line("      End_Of_Set := True;");
        Put_Line("    else");
        Put_Line("      End_Of_Set := False;");
        Put_Line("      Data := List.First_Element;");
        Put_Line("    end if;");
        Put_Line("    List.Clear;");
        Put_Line("  end Read_One" & All_Fields.Camel_Case & ";");
        Put_Line("");

        for Idx_Col of Idx_Column_List_1 loop
          declare
            Cnt : Integer := 0;
          begin
            Idx_Column_List_3.Append(Idx_Col);
            Tmp.Set(Tmp.Camel_Case & "_" & Idx_Col.Name.Camel_Case );
            Stm.Set("Stm_Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp.Camel_Case & "_Unique");

            Put_Line("");
            Put_Line("  -- unique index");
            Put_Line("  procedure Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type) is");
            Put_Line("  begin");
            Index_Sql_Statement(T             => Self,
                                Stm_Name      => Stm.Fix_String,
                                First_Stm_Row => "delete from " & Table_Name.Upper_Case,
                                Order_By_PK   => False,
                                Field_List    => Idx_Column_List_3);

            Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx_Column_List_3 );

            Put_Line("    " & Stm.Fix_String & ".Execute;");
            Put_Line("  end Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp.Camel_Case & ";");
            Put_Line("  --------------------------------------------");
            Put_Line("");

            Put_Line("  -- unique index");
            Put_Line("  function Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) &  "(");

            Cnt := Integer(Idx_Column_List_2.Length);
            for Idx_Col_2 of Idx_Column_List_2 loop
              if Cnt > 1 then
                Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ";");
              else
                Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ") return Boolean is");
              end if;
              Cnt := Cnt -1;
            end loop;

            Put_Line("    Data       : Table_" & Table_Name.Camel_Case & ".Data_Type;");
            Put_Line("    End_Of_Set : Boolean := False;");
            Put_Line("  begin");
            declare
              Tmp2 : String_Object;
            begin
              for Idx_Col_2 of Idx_Column_List_2 loop
                Put_Line("    Data." & Idx_Col_2.Name.Camel_Case & " := "  & Idx_Col_2.Name.Camel_Case & ";");
                Tmp2.Set(Tmp2.Camel_Case & "_" & Idx_Col_2.Name.Camel_Case);
              end loop;
              Put_Line("    Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp2.Camel_Case & "(Data, End_Of_Set);");
            end;
            Put_Line("    return not End_Of_Set;");
            Put_Line("  end Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) & ";");
            Put_Line("  --------------------------------------------");
            Put_Line("");
            Idx_Column_List_2.Delete_Last;
          end;
        end loop;

      else -- 1 field only in index (Index_Has_Several_Fields)
        declare
          Idx_Col : C.Column_Type := Idx.Column_List.First_Element;
        begin
          Stm.Set("Stm_Select_Count_I" & Utils.Trim(Idx.Sequence_Number'Img) & "_" & Idx_Col.Name.Camel_Case & "_Unique");

          Put_Line("  -- unique index");
          Put_Line("  function Count_" & Idx_Col.Name.Camel_Case & "(Data : Table_" & Table_Name.Camel_Case & ".Data_Type) return Integer_4 is");
          Put_Line("    use Sql;");
          Put_Line("    Count       : Integer_4 := 0;");
          Put_Line("    End_Of_Set  : Boolean := False;");
          Put_Line("    Start_Trans : constant Boolean := Sql.Transaction_Status = Sql.None;");
          Put_Line("    Transaction : Sql.Transaction_Type;");
          Put_Line("  begin");
          Put_Line("    if Start_Trans then Transaction.Start; end if;");
          Put_Line("");

          Index_Sql_Statement(T             => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "select count('a') from " & Table_Name.Upper_Case,
                              Order_By_PK   => False,
                              Field_List    => Idx.Column_List);

          Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx.Column_List );

          Put_Line("");
          Put_Line("    " & Stm.Fix_String & ".Open_Cursor;");
          Put_Line("    " & Stm.Fix_String & ".Fetch(End_Of_Set);");
          Put_Line("    if not End_Of_Set then");
          Put_Line("      " & Stm.Fix_String & ".Get(1, Count);");
          Put_Line("    end if;");
          Put_Line("    " & Stm.Fix_String & ".Close_Cursor;");
          Put_Line("    if Start_Trans then Transaction.Commit; end if;");
          Put_Line("    return Count;");
          Put_Line("  end Count_" & Idx_Col.Name.Camel_Case &";");
          Put_Line("  --------------------------------------------");
          Put_Line("");

          Stm.Set("Stm_Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & "_" & Idx_Col.Name.Camel_Case & "_Unique");
          Put_Line("  -- unique index");
          Put_Line("  procedure Delete_" & Idx_Col.Name.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type) is");
          Put_Line("  begin");
          Put_Line("");

          Index_Sql_Statement(T             => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "delete from " & Table_Name.Upper_Case,
                              Order_By_PK   => False,
                              Field_List    => Idx.Column_List);

          Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx.Column_List );

          Put_Line("");
          Put_Line("    " & Stm.Fix_String & ".Execute;");
          Put_Line("  end Delete_" & Idx_Col.Name.Camel_Case &";");
          Put_Line("");

          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("  -- unique index");
          Put_Line("  function Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) &  "(" & Idx_Col.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col.Type_Of), Idx_Col.Size_Of) & ") return Boolean is");
          Put_Line("    Data       : Table_" & Table_Name.Camel_Case & ".Data_Type;");
          Put_Line("    End_Of_Set : Boolean := False;");
          Put_Line("  begin");
          Put_Line("    Data." & Idx_Col.Name.Camel_Case & " := " & Idx_Col.Name.Camel_Case & ";");
          Put_Line("    Read_" & Idx_Col.Name.Camel_Case & "(Data, End_Of_Set);");
          Put_Line("    return not End_Of_Set;");
          Put_Line("  end Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) & ";");
          Put_Line("  --------------------------------------------");
          Put_Line("");
          Put_Line("");
        end ;
      end if; -- Index_Has_Several_Fields

     Idx_Column_List_1.Clear;
     Idx_Column_List_2.Clear;
     Idx_Column_List_3.Clear;

      Code_Debug(" -- stop Print_Def_Functions_Body.Unique_Procs_Body");
    end Unique_Procs_Body;

    --##--##--##--##--##--##--##--##--


    procedure Primary_Procs_Body(Idx : I.Index_Type; Table_Name : String_Object) is
      use Text_Io;
      All_Fields               : String_Object;
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
      Idx_Column_List_2        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
      Idx_Column_List_3        : C.Columns_Type with Warnings => Off; -- do not copy into this one !
      Stm                      : String_Object;
    begin
    -- for pk's with several fields
      Code_Debug(" -- start Print_Def_Functions_Body.Primary_Procs_Body");

      if not Index_Has_Several_Fields then
        -- only one keyfield -> return
        Put_Line("   -- stop Print_Def_Functions_Body.Primary_Procs_Body");
        return ;
      end if;

      for Idx_Col of Idx.Column_List loop
        All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
      end loop;
      Stm.Set("Stm_Delete" &  All_Fields.Camel_Case);
      Put_Line("");
      Put_Line("  -- primary key index several fields");
      Put_Line("  procedure Delete" & All_Fields.Camel_Case & "(Data  : in Table_" & Table_Name.Camel_Case & ".Data_Type) is");
      Put_Line("  begin");

      Keyed_Sql_Statement(Self          => Self,
                          Stm_Name      => Stm.Fix_String,
                          First_Stm_Row => "delete from " & Table_Name.Upper_Case,
                          Key           => I.Primary,
                          Turns         => Integer_4(Idx.Column_List.Length));

      Set_Keyed_Sql_Statement(Self      => Self,
                              Stm_Name  => Stm.Fix_String,
                              Key       => I.Primary,
                              Turns     => Integer_4(Idx.Column_List.Length));

      Put_Line("    " & Stm.Fix_String & ".Execute;");
      Put_Line("  end Delete" & All_Fields.Camel_Case & ";");
      Put_Line("  --------------------------------------------");
      Put_Line("");

      All_Fields.Set("");

      for Idx_Col of Idx_Column_List_1 loop
        declare
          Cnt : Integer := 0;
        begin
          All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
          Idx_Column_List_3.Append(Idx_Col);
          Stm.Set("Stm_Delete_I" & Utils.Trim(Idx.Sequence_Number'Img & All_Fields.Camel_Case));
          Put_Line("");
          Put_Line("  -- part of primary key index");
          Put_Line("  procedure Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & All_Fields.Camel_Case & "(Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type) is");
          Put_Line("  begin");

          Keyed_Sql_Statement(Self          => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "delete from " & Table_Name.Upper_Case,
                              Key           => I.Primary,
                              Turns         => Integer_4(Idx_Column_List_3.Length));

          Set_Keyed_Sql_Statement(Self      => Self,
                                  Stm_Name  => Stm.Fix_String,
                                  Key       => I.Primary,
                                  Turns     => Integer_4(Idx_Column_List_3.Length));

          Put_Line("    " & Stm.Fix_String & ".Execute;");
          Put_Line("  end Delete_I" & Utils.Trim(Idx.Sequence_Number'Img) & All_Fields.Camel_Case & ";");

          Put_Line("");
          Put_Line("  -- part of primary key index");
          Put_Line("  function Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) &  "(");

          Cnt := Integer(Idx_Column_List_3.Length);
          for Idx_Col_2 of Idx_Column_List_3 loop
            if Cnt > 1 then
              Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ";");
            else
              Put_Line("      "  & Idx_Col_2.Name.Camel_Case & " : in " &  To_Ada_Type(Data_Type(Idx_Col_2.Type_Of), Idx_Col_2.Size_Of) & ") return Boolean is");
            end if;
            Cnt := Cnt -1;
          end loop;

          Put_Line("    Data       : Table_" & Table_Name.Camel_Case & ".Data_Type;");
          Put_Line("    Is_Exist   : Boolean := False;");
          Put_Line("    List       : " & Table_Name.Camel_Case & "_List_Pack2.List;");
          Put_Line("  begin");

          declare
            Tmp2 : String_Object;
          begin
            for Idx_Col_2 of Idx_Column_List_3 loop
              Put_Line("    Data." & Idx_Col_2.Name.Camel_Case & " := "  & Idx_Col_2.Name.Camel_Case & ";");
              Tmp2.Set(Tmp2.Camel_Case & "_" & Idx_Col_2.Name.Camel_Case);
            end loop;
            Put_Line("    Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Tmp2.Camel_Case & "(Data, List, False, 1);");
          end;
          Put_Line("    Is_Exist := not List.Is_Empty;");
          Put_Line("    List.Clear;");
          Put_Line("    return Is_Exist;");
          Put_Line("  end Is_Existing_I" & Utils.Trim(Idx.Sequence_Number'Img) & ";");

          Idx_Column_List_2.Delete_Last;
        end;
      end loop;
      Idx_Column_List_1.Clear;
      Idx_Column_List_2.Clear;
      Idx_Column_List_3.Clear;
      Code_Debug(" -- stop Print_Def_Functions_Body.Primary_Procs_Body");
    end Primary_Procs_Body;

--  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
    procedure Get(T : in out Table_Type ) is
      use Text_Io;
    begin
      Code_Debug(" -- start Print_Def_Functions_Body.Get");
      Put_Line("");

      Put_Line("  function Get(Stm : in Sql.Statement_Type) return Table_" & T.Name.Camel_Case & ".Data_Type is");
      Put_Line("    Data : Table_" & T.Name.Camel_Case & ".Data_Type;");
      Put_Line("  begin");

      -- loop over all columns and write like
      --  Sql.Get ...
      for Col of T.Columns loop
        case Data_Type(Col.Type_Of) is
          when A_Char .. A_Short_Code =>
            if Col.Nullable then
              Put_Line("    if not Stm.Is_Null(" & Quote(Col.Name.Upper_Case) & ") then");
              Put_Line("      Stm.Get(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              Put_Line("    else");
              Put_Line("      Data." & Col.Name.Camel_Case & " :=  " & Null_Value(Data_Type(Col.Type_Of), Col.Size_Of) & ";");
              Put_Line("    end if;");
            else
              Put_Line("    Stm.Get(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
            end if;

          when A_Date =>
            if Col.Nullable then
              Put_Line("    if not Stm.Is_Null(" & Quote(Col.Name.Upper_Case) & ") then");
              Put_Line("      Stm.Get_Date(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              Put_Line("    else");
              Put_Line("      Data." & Col.Name.Camel_Case & " :=  " & Null_Value(Data_Type(Col.Type_Of), Col.Size_Of) & ";");
              Put_Line("    end if;");
            else
              Put_Line("    Stm.Get_Date(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
            end if;

          when A_Time =>
            if Col.Nullable then
              Put_Line("    if not Stm.Is_Null(" & Quote(Col.Name.Upper_Case) & ") then");
              Put_Line("      Stm.Get_Time(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              Put_Line("    else");
              Put_Line("      Data." & Col.Name.Camel_Case & " :=  " & Null_Value(Data_Type(Col.Type_Of), Col.Size_Of) & ";");
              Put_Line("    end if;");
            else
              Put_Line("    Stm.Get_Time(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
            end if;

          when A_Timestamp =>
            if Col.Nullable then
              Put_Line("    if not Stm.Is_Null(" & Quote(Col.Name.Upper_Case) & ") then");
              Put_Line("      Stm.Get_Timestamp(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              Put_Line("    else");
              Put_Line("      Data." & Col.Name.Camel_Case & " :=  " & Null_Value(Data_Type(Col.Type_Of), Col.Size_Of) & ";");
              Put_Line("    end if;");
            else
              Put_Line("    Stm.Get_Timestamp(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
            end if;

          when A_Clob =>
            if Col.Nullable then
              Put_Line("    if not Stm.Is_Null(" & Quote(Col.Name.Upper_Case) & ") then");
              Put_Line("      Stm.Get_Lob(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
              Put_Line("    else");
              Put_Line("      Data." & Col.Name.Camel_Case & " :=  " & Null_Value(Data_Type(Col.Type_Of), Col.Size_Of) & ";");
              Put_Line("    end if;");
            else
              Put_Line("    Stm.Get_Lob(" & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
            end if;

          when others =>
            raise Configuration_Error with "Not supported datatype: " &  Data_Type(Col.Type_Of)'Img;
        end case;
      end loop;

      Put_Line("");
      Put_Line("    return Data;");
      Put_Line("  end Get;");
      Put_Line("---------------------------------------------");
      Code_Debug(" -- stop Print_Def_Functions_Body.Get");
      Put_Line("");
    end Get;

    --  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--

    procedure Read(T : in out Table_Type ) is
      use Text_Io;
    begin
      Code_Debug(" -- start Print_Def_Functions_Body.Read");
      Put_Line("  procedure Read(Data       : in out Table_" & T.Name.Camel_Case & ".Data_Type;");
      Put_Line("                 End_Of_Set : in out Boolean) is");
      Put_Line("    use Sql;");
      Put_Line("    Start_Trans   : constant Boolean := Sql.Transaction_Status = Sql.None;");
      Put_Line("    Transaction   : Sql.Transaction_Type;");
      Put_Line("  begin");
      Put_Line("    if Start_Trans then Transaction.Start; end if;");
      T.Keyed_Sql_Statement(Stm_Name      => "Stm_Select",
                            First_Stm_Row => "select * from " & T.Name.Upper_Case,
                            Key           => I.Primary );
      T.Set_Keyed_Sql_Statement(Stm_Name => "Stm_Select",
                                Key      => I.Primary);
      Put_Line("    Stm_Select.Open_Cursor;");
      Put_Line("    Stm_Select.Fetch(End_Of_Set);");
      Put_Line("    if not End_Of_Set then");
      Put_Line("      Data := Get(Stm_Select);");
      Put_Line("    end if;");
      Put_Line("    Stm_Select.Close_Cursor;");
      Put_Line("    if Start_Trans then Transaction.Commit; end if;");
      Put_Line("  end Read;");
      Code_Debug(" -- stop Print_Def_Functions_Body.Read");
      Put_Line("");
    end Read;

    --  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--

    procedure Get_On_Key(T : in out Table_Type ) is
      use Text_Io;
    begin
      Code_Debug(" -- start Print_Def_Functions_Body.Get_On_Key");
      Put_Line("");
      Put_Line("  function Get(");

      -- loop over all columns and write like
      --  Bsqpsto : String; .. for all keys
      -- last key line ends with ')' not with ';'
      for Col of T.Primary_Column_List_But_Last loop
        Put_Line("                  " & Col.Name.Camel_Case & " : " &  To_Ada_Type(Data_Type(Col.Type_Of), Col.Size_Of) & ";");
      end loop;
      Put_Line("                  " &
                   T.Last_Primary_Column.Name.Camel_Case & " : " &
                   To_Ada_Type(Data_Type(T.Last_Primary_Column.Type_Of), T.Last_Primary_Column.Size_Of) &
                   ") return Table_" & T.Name.Camel_Case & ".Data_Type is");

      Put_Line("    Data       : Table_" & T.Name.Camel_Case & ".Data_Type;");
      Put_Line("    End_Of_Set : Boolean := True;");
      Put_Line("  begin");

      for Col of T.Primary_Column_List loop
        Put_Line("    Data." & Col.Name.Camel_Case & " := " &  Col.Name.Camel_Case & ";");
      end loop;
      Put_Line("    Data.Read(End_Of_Set);");
      Put_Line("    return Data;");
      Put_Line("  end Get;");
      Put_Line("");
      Code_Debug(" -- stop Print_Def_Functions_Body.Get_On_Key");
      Put_Line("");
    end Get_On_Key;

    --  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--

    procedure Is_Existing(T : in out Table_Type ) is
      use Text_Io;
    begin
      Code_Debug(" -- start Print_Def_Functions_Body.Is_Existing");
      Put_Line("");
      Put_Line("  function Is_Existing(");

      -- loop over all columns and write like
      --  Bsqpsto : String; .. for all keys
      -- last key line ends with ')' not with ';'
      for Col of T.Primary_Column_List_But_Last loop
        Put_Line("                  " & Col.Name.Camel_Case & " : " &  To_Ada_Type(Data_Type(Col.Type_Of), Col.Size_Of) & ";");
      end loop;
      Put_Line("                  " &
                   T.Last_Primary_Column.Name.Camel_Case & " : " &
                   To_Ada_Type(Data_Type(T.Last_Primary_Column.Type_Of), T.Last_Primary_Column.Size_Of) &
                   ") return Boolean is");

      Put_Line("    Data       : Table_" & T.Name.Camel_Case & ".Data_Type;");
      Put_Line("    End_Of_Set : Boolean := True;");
      Put_Line("  begin");

      for Col of T.Primary_Column_List loop
        Put_Line("    Data." & Col.Name.Camel_Case & " := " &  Col.Name.Camel_Case & ";");
      end loop;
      Put_Line("    Read(Data, End_Of_Set);");
      Put_Line("    return not End_Of_Set;");
      Put_Line("  end Is_Existing;");
      Put_Line("");
      Code_Debug(" -- stop Print_Def_Functions_Body.Is_Existing");
      Put_Line("");
    end Is_Existing;

  --  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--

--# Always these

    procedure Read_List(T : in out Table_Type ) is
      use Text_Io;
    begin
      Code_Debug(" -- start Print_Def_Functions_Body.Read_List");
      Put_Line("");
      Put_Line("  procedure Read_List(Stm   : in     Sql.Statement_Type;");
      Put_Line("                      List  : in out " & T.Name.Camel_Case & "_List_Pack2.List;");
      Put_Line("                      Max   : in     Integer_4 := Integer_4'Last) is");
      Put_Line("    use Sql;");
      Put_Line("    Start_Trans  : constant Boolean := Sql.Transaction_Status = Sql.None;");
      Put_Line("    Transaction  : Sql.Transaction_Type;");
      Put_Line("    Count        : Integer_4 := 0;");
      Put_Line("    Data         : Table_" & T.Name.Camel_Case & ".Data_Type;");
      Put_Line("    Eos          : Boolean := False;");
      Put_Line("  begin");
      Put_Line("    if Start_Trans then Transaction.Start; end if;");
      Put_Line("    Stm.Open_Cursor;");
      Put_Line("    loop");
      Put_Line("      Stm.Fetch(Eos);");
      Put_Line("      exit when Eos or Count > Max;");
      Put_Line("      Data := Get(Stm);");
      Put_Line("      List.Append(Data);");
      Put_Line("      Count := Count +1;");
      Put_Line("    end loop;");
      Put_Line("    Stm.Close_Cursor;");
      Put_Line("    if Start_Trans then Transaction.Commit; end if;");
      Put_Line("  end Read_List;");

      Code_Debug(" -- stop Print_Def_Functions_Body.Read_List");
      Put_Line("");
    end Read_List;
--  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--

--  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--

    procedure Read_All(T : in out Table_Type ) is
      use Text_Io;
    begin
      Code_Debug(" -- start Print_Def_Functions_Body.Read_All");
      Put_Line("");
      Put_Line("  procedure Read_All(List  : in out " & T.Name.Camel_Case & "_List_Pack2.List;");
      Put_Line("                     Order : in     Boolean := False;");
      Put_Line("                     Max   : in     Integer_4 := Integer_4'Last) is");
      Put_Line("    use Sql;");
      Put_Line("    Start_Trans   : constant Boolean := Sql.Transaction_Status = Sql.None;");
      Put_Line("    Transaction   : Sql.Transaction_Type;");
      Put_Line("  begin");
      Put_Line("    if Start_Trans then Transaction.Start; end if;");
      Put_Line("    if Order then");
      Put_Line("      Stm_Select_All_O.Prepare(" & Quote("select * from " & T.Name.Upper_Case & " order by ") & " &");

      -- loop over all pk columns and write like
      --  Bsqpsto ,
      -- last key line ends with ')' not with ','
      for Col of T.Primary_Column_List_But_Last loop
        Put_Line("                  " & Quote(Col.Name.Upper_Case & ", ") & " &");
      end loop;
      Put_Line("                  " & Quote(T.Last_Primary_Column.Name.Upper_Case) & ");");
      Put_Line("      Read_List(Stm_Select_All_O, List, Max);");
      Put_Line("    else");
      Put_Line("      Stm_Select_All.Prepare(" & Quote("select * from " & T.Name.Upper_Case) & ");");
      Put_Line("      Read_List(Stm_Select_All, List, Max);");
      Put_Line("    end if;");
      Put_Line("    if Start_Trans then Transaction.Commit; end if;");
      Put_Line("  end Read_All;");
      Code_Debug("  -- stop Print_Def_Functions_Body.Read_All");
      Put_Line("");
    end Read_All;
--  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
--
    procedure Delete(T : in out Table_Type ) is
      use Text_Io;
    begin
      Code_Debug(" -- start Print_Def_Functions_Body.Delete");
      Put_Line("  procedure Delete(Data : in Table_" & T.Name.Camel_Case & ".Data_Type) is");
      Put_Line("  begin");
      T.Keyed_Sql_Statement(Stm_Name      => "Stm_Delete",
                            First_Stm_Row => "delete from " & T.Name.Upper_Case,
                            Key           => I.Primary );
      T.Set_Keyed_Sql_Statement(Stm_Name => "Stm_Delete",
                                Key      => I.Primary);
      Put_Line("    Stm_Delete.Execute;");
      Put_Line("  end Delete;");
      Code_Debug(" -- stop Print_Def_Functions_Body.Delete");
      Put_Line("");
    end Delete;

--  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--
--
    procedure Update(T : in out Table_Type ) is
      use Text_Io;
    begin
      Code_Debug(" -- start Print_Def_Functions_Body.Update");
      Put_Line("  procedure Update(Data : in out Table_" & T.Name.Camel_Case & ".Data_Type ; Keep_Timestamp : in Boolean := False) is");
      Put_Line("    Now     : Calendar2.Time_Type := Calendar2.Clock;");
      Put_Line("    Process : Process_Io.Process_Type     := Process_Io.This_Process;");
      Put_Line("  begin");

      T.Prepare_All_Columns(Stm_Name      => "Stm_Update",
                            First_Stm_Row => "update " & T.Name.Upper_Case & " set " ,
                            Where_Keys    => True,
                            Old_IXX       => False,
                            Set_Primary   => False);

      T.Set_All_Columns_But_Pk(Stm_Name    => "Stm_Update",
                        Set_Old_IXX => False,
                        Set_Primary => True);
      T.Set_Keyed_Sql_Statement(Stm_Name => "Stm_Update",
                                Key      => I.Primary);

      Put_Line("    Stm_Update.Execute;");
      Put_Line("  end Update;");
      Code_Debug(" -- stop Print_Def_Functions_Body.Update");
      Put_Line("");
    end Update;


--  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--

  procedure Insert (T : in out Table_Type ) is
    use Text_Io;
  begin
    Code_Debug(" -- start Print_Def_Functions_Body.Insert");

    Put_Line("  procedure Insert(Data : in out Table_" & T.Name.Camel_Case & ".Data_Type; Keep_Timestamp : in Boolean := False) is");
    Put_Line("    Now     : Calendar2.Time_Type := Calendar2.Clock;");
    Put_Line("    Process : Process_Io.Process_Type     := Process_Io.This_Process;");
    Put_Line("  begin");

    Put_Line("    if not Keep_Timestamp then");
    case Self.Ixx_Type is
      when None      =>
        Put_Line("      null; --for tables without IXX*");
      when Date_Time =>
        Put_Line("      Data.Ixxluda := Now;");
        Put_Line("      Data.Ixxluti := Now;");
      when Timestamp =>
        Put_Line("      Data.Ixxluts := Now;");
    end case;
    Put_Line("    end if;");

    T.Insert_All_Columns(Stm_Name      => "Stm_Insert",
                         First_Stm_Row => "insert into " & T.Name.Upper_Case & " values (");

    T.Set_All_Columns_Incl_Pk(Stm_Name    => "Stm_Insert",
                              Set_Old_IXX => False,
                              Set_Primary => True);

    Put_Line("    Stm_Insert.Execute;");
    Put_Line("  end Insert;");
    Put_Line("--------------------------------------------");
    Code_Debug(" -- stop Print_Def_Functions_Body.Insert");
  end Insert;

  --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--

  procedure Delete_Withcheck( T : in out Table_Type ) is
      use Text_Io;
  begin
    Code_Debug(" -- start Print_Def_Functions_Body.Delete_Withcheck");
    Put_Line("  procedure Delete_Withcheck(Data : in Table_" & T.Name.Camel_Case & ".Data_Type) is");
    Put_Line("  begin");
    T.Keyed_Sql_Statement(Stm_Name      => "Stm_Delete_With_Check",
                          First_Stm_Row => "delete from " & T.Name.Upper_Case,
                          Key           => I.Primary,
                          Generate_IXX  => True);

    T.Set_Keyed_Sql_Statement(Stm_Name      => "Stm_Delete_With_Check",
                              Key           => I.Primary,
                              Generate_IXX  => True);

    Put_Line("    Stm_Delete_With_Check.Execute;");
    Put_Line("  end Delete_Withcheck;");
    Put_Line("--------------------------------------------");
    Code_Debug(" -- stop Print_Def_Functions_Body.Delete_Withcheck");
  end Delete_Withcheck;
  --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--

  procedure Update_Withcheck( T : in out Table_Type ) is
      use Text_Io;
  begin
    Code_Debug(" -- start Print_Def_Functions_Body.Update_Withcheck");
    Put_Line("  procedure Update_Withcheck(Data : in out Table_" & T.Name.Camel_Case & ".Data_Type; Keep_Timestamp : in Boolean := False) is");
    Put_Line("    Now     : Calendar2.Time_Type := Calendar2.Clock;");
    Put_Line("    Process : Process_Io.Process_Type     := Process_Io.This_Process;");
    Put_Line("  begin");
    T.Prepare_All_Columns(Stm_Name      => "Stm_Update_With_Check",
                          First_Stm_Row => "update " & T.Name.Upper_Case & " set " ,
                          Where_Keys    => True,
                          Old_IXX       => True,
                          Set_Primary   => False);


    T.Set_Keyed_Sql_Statement(Stm_Name      => "Stm_Update_With_Check",
                              Key           => I.Primary,
                              Generate_IXX  => False);

    T.Set_All_Columns_But_Pk(Stm_Name    => "Stm_Update_With_Check",
                      Set_Old_IXX => True,
                      Set_Primary => True);

    Put_Line("    Stm_Update_With_Check.Execute;");
    Put_Line("  end Update_Withcheck;");
    Put_Line("--------------------------------------------");
    Code_Debug(" -- stop Print_Def_Functions_Body.Update_Withcheck");
  end Update_Withcheck;
  --##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--

-- Begin  Print_Def_Functions_Body
    use Text_Io;

  begin
    Code_Debug(" -- start Print_Package_Start_Body");

    if Self.Entity_Type = Ud4 then
      -- only for db
      Code_Debug(" -- stop Print_Package_Start_Body");
      return;
    end if;

    Put_Line("  -- Procedures for all DBMS");


    --Functions operating on Primary Key
    Put_Line("");
    Put_Line("  -- Procedures for DBMS SQL");
    Put_Line("  -- Primary key");

    Get(Self);
    Get_On_Key(Self);
    Read_All(Self);
    Read_List(Self);
    Is_Existing(Self);
    Read(Self);
    Delete(Self);
    if not Self.All_Columns_Are_Primary then
      Update(Self);
    end if;
    Insert(Self);

    case Self.Ixx_Type is
      when None      => null;
      when Date_Time |
           Timestamp      =>
        Delete_Withcheck(Self);
        if not Self.All_Columns_Are_Primary then
          Update_Withcheck(Self);
        end if;
    end case;

    for Idx of Self.Indicies loop
      case Idx.Type_Of is
        when I.Primary => Primary_Procs_Body(Idx, Self.Name);
        when I.Unique  => Unique_Procs_Body(Idx, Self.Name);
        when I.Index   => Index_Procs_Body(Idx, Self.Name);
        when I.Functional => null;
      end case;
      Put_Line("");
    end loop;
    Code_Debug(" -- stop Print_Def_Functions_Body");
    Put_Line("");
  end Print_Def_Functions_Body;
  --------------------------------
  procedure Print_Ud4_Functions_Body(Self : in out Table_Type ) is
    use Text_Io;
  begin

    Code_Debug(" -- start Print_Ud4_Functions_Body");
    Put_Line("");

    Put_Line("");
    Put_Line("  -- Procedures for DBMS UD4");
    Put_Line("");
    Code_Debug(" -- start Print_Ud4_Functions_Body.Get_Values");

    Put_Line("  --------------------------------------------");
    Put_Line("  procedure Get_Values(Request : in     Request_Type;");
    Put_Line("                       Data    : in out Table_" & Self.Name.Camel_Case & ".Data_Type) is");

    Put_Line("  begin");
    for Col of Self.Columns loop
      case Data_Type(Col.Type_Of) is
        when A_Char .. A_Short_Code =>
          Put_Line("    if Has_Value(Request, " & Quote(Col.Name.Upper_Case) & ") then");
          Put_Line("      Get_Value(Request, " & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
          Put_Line("    end if;");
        when A_Date =>
          Put_Line("    if Has_Value(Request, " & Quote(Col.Name.Upper_Case) & ") then");
          Put_Line("      Get_Date_Value(Request, " & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
          Put_Line("    end if;");
        when A_Time =>
          Put_Line("    if Has_Value(Request, " & Quote(Col.Name.Upper_Case) & ") then");
          Put_Line("      Get_Time_Value(Request, " & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
          Put_Line("    end if;");
        when A_Timestamp =>
          Put_Line("    if Has_Value(Request, " & Quote(Col.Name.Upper_Case) & ") then");
          Put_Line("      Get_Timestamp_Value(Request, " & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
          Put_Line("    end if;");
        when A_Clob .. A_Blob =>
          Put_Line("    raise Table_Utils.Configuration_Error with " & Quote(Col.Name.Upper_Case &" not supported datatype for UD4 " & Data_Type(Col.Type_Of)'Img) & ";");
      end case;
    end loop;
    Put_Line("  end Get_Values;");
    Code_Debug(" -- stop Print_Ud4_Functions_Body.Get_Values");
    Put_Line("  --------------------------------------------");


    Put_Line("");
    Code_Debug(" -- start Print_Ud4_Functions_Body.Set_Values");
    Put_Line("  procedure Set_Values(Reply  : in out Request_Type;");
    Put_Line("                       Data   : in     Table_" & Self.Name.Camel_Case & ".Data_Type) is");
    Put_Line("  begin");
    for Col of Self.Columns loop
      case Data_Type(Col.Type_Of) is
        when A_Char .. A_Short_Code =>
          Put_Line("    Set_Value(Reply, " & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Date =>
          Put_Line("    Set_Date_Value(Reply, " & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Time =>
          Put_Line("    Set_Time_Value(Reply, " & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Timestamp =>
          Put_Line("    Set_Timestamp_Value(Reply, " & Quote(Col.Name.Upper_Case) & ", Data." & Col.Name.Camel_Case & ");");
        when A_Clob .. A_Blob =>
          Put_Line("    raise Table_Utils.Configuration_Error with " & Quote(Col.Name.Upper_Case & " not supported datatype for UD4 " & Data_Type(Col.Type_Of)'Img) & ";");
      end case;
    end loop;
    Put_Line("  end Set_Values;");
    Code_Debug(" -- stop Print_Ud4_Functions_Body.Set_Values");
    Put_Line("  --------------------------------------------");


    Put_Line("");
    Code_Debug(" -- start Print_Ud4_Functions_Body.Make_Ud4_Telegram_1");
    Put_Line("  procedure Make_Ud4_Telegram(Request   : in out Uniface_Request.Request_Type;");
    Put_Line("                              Operation : in     Operation_Type := Get_One_Record) is");
    Put_Line("    use Uniface_Request;");
    Put_Line("    Next_Column : Integer_2 := 0;");
    Put_Line("    Offset      : Natural   := 0;");
    Put_Line("  begin");
    Put_Line("    Construct_Ud4_Record(Request, " & Quote(Self.Name.Upper_Case) & "," & Self.Num_Columns'Img & ", Next_Column, Offset, Operation);");
    for Col of Self.Columns loop
      case Data_Type(Col.Type_Of) is
        when A_Char =>
          Put_Line("    Add_Column(Request, " & Quote(Col.Name.Upper_Case) & ", " & Ud4_Type_Mapper(Data_Type(Col.Type_Of)).all & ", Offset, " & Col.Size_Of'Img & ");");
        when A_Int .. A_Timestamp =>
          Put_Line("    Add_Column(Request, " & Quote(Col.Name.Upper_Case) & ", " & Ud4_Type_Mapper(Data_Type(Col.Type_Of)).all & ", Offset);");
        when A_Clob .. A_Blob =>
          Put_Line("    raise Table_Utils.Configuration_Error with " & Quote(Col.Name.Upper_Case & " not supported datatype for UD4 " & Data_Type(Col.Type_Of)'Img) & ";");
      end case;
    end loop;
    Put_Line("  end Make_Ud4_Telegram;");
    Code_Debug(" -- stop Print_Ud4_Functions_Body.Make_Ud4_Telegram_1");
    Put_Line("  --------------------------------------------");


    Put_Line("");
    Code_Debug(" -- start Print_Ud4_Functions_Body.Make_Ud4_Telegram_2");
    Put_Line("  procedure Make_Ud4_Telegram(Request   : in out Uniface_Request.Request_Type;");
    Put_Line("                              Data      : in     Table_" & Self.Name.Camel_Case & ".Data_Type;");
    Put_Line("                              Operation : in     Operation_Type := Get_One_Record) is");
    Put_Line("  begin");
    Put_Line("    Make_Ud4_Telegram(Request, Operation);");
    Put_Line("    Set_Values(Request, Data);");
    Put_Line("  end Make_Ud4_Telegram;");
    Code_Debug(" -- stop Print_Ud4_Functions_Body.Make_Ud4_Telegram_2");
    Put_Line("  --------------------------------------------");
    Put_Line("");
    Code_Debug(" -- stop Print_Ud4_Functions_Body");
    Put_Line("");
  end Print_Ud4_Functions_Body;
  --------------------------------
  procedure Print_XML_Functions_Body(Self : in out Table_Type ) is
    use Text_Io;
    Column_Counter : Integer_4 := 0;
  begin
    Code_Debug(" -- start Print_XML_Functions_Body");

    Put_Line("");
    Put_Line("  -- Procedures for all DBMS");
    Put_Line("");
    Put_Line("");

    Put_Line("  function To_String(Data : in Table_" & Self.Name.Camel_Case & ".Data_Type) return String is");
    Put_Line("  begin");
    Put_Line("    return");

    for Col of Self.Columns loop
      case Data_Type(Col.Type_Of) is
        when A_Char =>
          if Col.Size_Of = 1 then
            Put_Line("          " & Quote(Col.Name.Camel_Case & " = ") & " & Data." & Col.Name.Camel_Case & " & " & Quote(" ") & " &");
          else
            Put_Line("          " & Quote(Col.Name.Camel_Case & " = ") & " & Utils.Trim(Data." & Col.Name.Camel_Case & ") & " & Quote(" ") & " &");
          end if;
        when A_Int .. A_Long | A_Short_Code | A_Boolean =>
          Put_Line("          " & Quote(Col.Name.Camel_Case & " = ") & " & Utils.Trim(Data." & Col.Name.Camel_Case & "'Img) & " & Quote(" ") & " &");
        when A_Float .. A_Double =>
          Put_Line("          " & Quote(Col.Name.Camel_Case & " = ") & " & Utils.F8_Image(Data." & Col.Name.Camel_Case & ") & " & Quote(" ") & " &");
        when A_Date =>
          Put_Line("          " & Quote(Col.Name.Camel_Case & " = ") & " & Calendar2.String_Date(Data." & Col.Name.Camel_Case & ") & " & Quote(" ") & " &");
        when A_Time =>
          Put_Line("          " & Quote(Col.Name.Camel_Case & " = ") & " & Calendar2.String_Time(Data." & Col.Name.Camel_Case& ") & " & Quote(" ") & " &");
        when A_Timestamp =>
          Put_Line("          " & Quote(Col.Name.Camel_Case & " = ") & " & Calendar2.String_Date_And_Time(Data." & Col.Name.Camel_Case & ", Milliseconds => True ) & " & Quote(" ") & " &");
        when A_Clob .. A_Blob | A_Char_Code =>
          Put_Line( "          " & Quote(Col.Name.Camel_Case & " = not supported datatype for XML " & Data_Type(Col.Type_Of)'Img) & " & "  & Quote(" ") & " &");
      end case;
    end loop;
    Put_Line("          " & Quote("") & ";");
    Put_Line("  end To_String;");
    Put_Line("");
    --------
    Put_Line("  function To_JSON(Data : in Table_" & Self.Name.Camel_Case & ".Data_Type) return JSON_Value is");
    Put_Line("    Json_Data : JSON_Value := Create_Object;");
    Put_Line("  begin");
    for Col of Self.Columns loop
      case Data_Type(Col.Type_Of) is
        when A_Char =>
          Put_Line("    Json_Data.Set_Field(Field_Name => " & Quote(Col.Name.Lower_Case) & ", Field => Utils.Trim(Data." & Col.Name.Camel_Case & "));");
        when A_Int .. A_Long | A_Short_Code =>
          Put_Line("    Json_Data.Set_Field(Field_Name => " & Quote(Col.Name.Lower_Case) & ", Field => Long_Integer(Data." & Col.Name.Camel_Case & "));");
        when A_Boolean =>
          Put_Line("    Json_Data.Set_Field(Field_Name => " & Quote(Col.Name.Lower_Case) & ", Field => Data." & Col.Name.Camel_Case & ");");
        when A_Float .. A_Double =>
          Put_Line("    Json_Data.Set_Field(Field_Name => " & Quote(Col.Name.Lower_Case) & ", Field => Float(Data." & Col.Name.Camel_Case & "));");
        when A_Date =>
          Put_Line("    Json_Data.Set_Field(Field_Name => " & Quote(Col.Name.Lower_Case) & ", Field => Data." & Col.Name.Camel_Case & ".To_String);");
        when A_Time =>
          Put_Line("    Json_Data.Set_Field(Field_Name => " & Quote(Col.Name.Lower_Case) & ", Field => Data." & Col.Name.Camel_Case & ".To_String);");
        when A_Timestamp =>
          Put_Line("    Json_Data.Set_Field(Field_Name => " & Quote(Col.Name.Lower_Case) & ", Field => Data." & Col.Name.Camel_Case & ".To_String);");
        when A_Clob .. A_Blob | A_Char_Code =>
          Put_Line( "          " & Quote(Col.Name.Lower_Case & " = not supported datatype for JSON " & Data_Type(Col.Type_Of)'Img) & " & "  & Quote(" ") & " &");
      end case;
    end loop;
    Put_Line("    return Json_Data;");
    Put_Line("  end To_JSON;");
    Put_Line("");


    Put_Line("  function From_JSON(JSON_Data : in JSON_Value) return Table_" & Self.Name.Camel_Case & ".Data_Type is");
    Put_Line("    use Ada.Strings;");
    Put_Line("    Data : Table_" & Self.Name.Camel_Case & ".Data_Type;");

    Put_Line("  begin");
    for Col of Self.Columns loop
      case Data_Type(Col.Type_Of) is
        when A_Char =>
          Put_Line("    if JSON_Data.Has_Field(" & Quote(Col.Name.Lower_Case) & ") then");
          Put_Line("      Move( Source => JSON_Data.Get(" & Quote(Col.Name.Lower_Case) & "), Target => Data." & Col.Name.Camel_Case & " , Drop => Right);");
          Put_Line("    end if;");

        when A_Int | A_Long | A_Short_Code =>
          Put_Line("    if JSON_Data.Has_Field(" & Quote(Col.Name.Lower_Case) & ") then");
          Put_Line("      declare");
          Put_Line("        Tmp : Long_Integer := 0;");
          Put_Line("      begin");
          Put_Line("        Tmp := JSON_Data.Get(" & Quote(Col.Name.Lower_Case) & ");");
          Put_Line("        Data." & Col.Name.Camel_Case & " := Integer_4(Tmp);");
          Put_Line("      end;");
          Put_Line("    end if;");

        when A_Big_Int =>
          Put_Line("    if JSON_Data.Has_Field(" & Quote(Col.Name.Lower_Case) & ") then");
          Put_Line("      declare");
          Put_Line("        Tmp : Long_Integer := 0;");
          Put_Line("      begin");
          Put_Line("        Tmp := JSON_Data.Get(" & Quote(Col.Name.Lower_Case) & ");");
          Put_Line("        Data." & Col.Name.Camel_Case & " := Integer_8(Tmp);");
          Put_Line("      end;");
          Put_Line("    end if;");

        when A_Boolean =>
          Put_Line("    if JSON_Data.Has_Field(" & Quote(Col.Name.Lower_Case) & ") then");
          Put_Line("        Data." & Col.Name.Camel_Case & " := JSON_Data.Get(" & Quote(Col.Name.Lower_Case) & ");");
          Put_Line("    end if;");

        when A_Float .. A_Double =>
          Put_Line("    if JSON_Data.Has_Field(" & Quote(Col.Name.Lower_Case) & ") then");
          Put_Line("      declare");
          Put_Line("        Tmp : Float := 0.0;");
          Put_Line("      begin");
          Put_Line("        Tmp := JSON_Data.Get(" & Quote(Col.Name.Lower_Case) & ");");
          Put_Line("        Data." & Col.Name.Camel_Case & " := Fixed_Type(Tmp);");
          Put_Line("      end;");
          Put_Line("    end if;");

        when A_Date =>
          Put_Line("    if JSON_Data.Has_Field(" & Quote(Col.Name.Lower_Case) & ") then");
          Put_Line("      declare");
          Put_Line("        Tmp : String := JSON_Data.Get(" & Quote(Col.Name.Lower_Case) & ");");
          Put_Line("      begin  -- " & Quote("marketStartTime") & ":" & Quote("2013-06-22T17:39:00.000Z"));
          Put_Line("        Data." & Col.Name.Camel_Case & " := Calendar2.To_Time_Type(Tmp(1..10), Tmp(12..23));");
          Put_Line("      end;");
          Put_Line("    end if;");

        when A_Time =>
          Put_Line("    if JSON_Data.Has_Field(" & Quote(Col.Name.Lower_Case) & ") then");
          Put_Line("      declare");
          Put_Line("        Tmp : String := JSON_Data.Get(" & Quote(Col.Name.Lower_Case) & ");");
          Put_Line("      begin  -- " & Quote("marketStartTime") & ":" & Quote("2013-06-22T17:39:00.000Z"));
          Put_Line("        Data." & Col.Name.Camel_Case & " := Calendar2.To_Time_Type(Tmp(1..10), Tmp(12..23));");
          Put_Line("      end;");
          Put_Line("    end if;");

        when A_Timestamp =>
          Put_Line("    if JSON_Data.Has_Field(" & Quote(Col.Name.Lower_Case) & ") then");
          Put_Line("      declare");
          Put_Line("        Tmp : String := JSON_Data.Get(" & Quote(Col.Name.Lower_Case) & ");");
          Put_Line("      begin  -- " & Quote("marketStartTime") & ":" & Quote("2013-06-22T17:39:00.000Z"));
          Put_Line("        Data." & Col.Name.Camel_Case & " := Calendar2.To_Time_Type(Tmp(1..10), Tmp(12..23));");
          Put_Line("      end;");
          Put_Line("    end if;");

        when A_Clob .. A_Blob | A_Char_Code =>
          Put_Line( "          " & Quote(Col.Name.Lower_Case & " = not supported datatype for JSON " & Data_Type(Col.Type_Of)'Img) & " & "  & Quote(" ") & " &");
      end case;
     Put_Line("");
    end loop;
    Put_Line("    return Data;");
    Put_Line("  end From_JSON;");
    Put_Line("");

  ---------------


    Put_Line("  function To_Xml(Data      : in Table_" & Self.Name.Camel_Case & ".Data_Type;");
    Put_Line("                  Ret_Start : in Boolean;");
    Put_Line("                  Ret_Data  : in Boolean;");
    Put_Line("                  Ret_End   : in Boolean) return String is");
    Put_Line("    Ls      : constant String := " & Quote("") & ";");
    Put_Line("    S_Start : constant String := " & Quote("<" & Self.Name.Upper_Case & "_ROW>") & " & Ls;");
    Put_Line("    S_End   : constant String := "  & Quote("</" & Self.Name.Upper_Case & "_ROW>") & " & Ls;");

    for Col of Self.Columns loop
      Column_Counter := Column_Counter +1;
      Put_Line("    S" & Utils.Trim(Column_Counter'Img) & " : constant String :=");

      case Data_Type(Col.Type_Of) is
        when A_Char =>
          if Col.Size_Of = 1 then
            Put_Line("          " & Quote("<" & Col.Name.Upper_Case & ">" ) & " & Data." & Col.Name.Camel_Case & " & " & Quote("</" & Col.Name.Upper_Case & ">") & " & Ls;");
          else
            Put_Line("          " & Quote("<" & Col.Name.Upper_Case & ">" ) & " & Utils.Trim(Data." & Col.Name.Camel_Case & ") & " & Quote("</" & Col.Name.Upper_Case & ">") & " & Ls;");
          end if;
        when A_Int .. A_Long | A_Short_Code | A_Boolean =>
          Put_Line("          " & Quote("<" & Col.Name.Upper_Case & ">" ) & " & Utils.Trim(Data." & Col.Name.Camel_Case & "'Img) & " & Quote("</" & Col.Name.Upper_Case & ">") & " & Ls;");
        when A_Float .. A_Double =>
          Put_Line("          " & Quote("<" & Col.Name.Upper_Case & ">" ) & " & Utils.F8_Image(Data." & Col.Name.Camel_Case & ") & " & Quote("</" & Col.Name.Upper_Case & ">") & " & Ls;");
        when A_Date =>
          Put_Line("          " & Quote("<" & Col.Name.Upper_Case & ">" ) & " & Calendar2.String_Date(Data." & Col.Name.Camel_Case & ") & " & Quote("</" & Col.Name.Upper_Case & ">") & " & Ls;");
        when A_Time =>
          Put_Line("          " & Quote("<" & Col.Name.Upper_Case & ">" ) & " & Calendar2.String_Time(Data." & Col.Name.Camel_Case & ") & " & Quote("</" & Col.Name.Upper_Case & ">") & " & Ls;");
        when A_Timestamp =>
          Put_Line("          " & Quote("<" & Col.Name.Upper_Case & ">" ) & " & Calendar2.String_Date_And_Time(Data." & Col.Name.Camel_Case & ", Milliseconds => True) & " & Quote("</" & Col.Name.Upper_Case & ">") & " & Ls;");
        when A_Clob .. A_Blob | A_Char_Code  =>
          Put_Line( "          " & Quote(Col.Name.Camel_Case & " = not supported datatype for XML " & Data_Type(Col.Type_Of)'Img) & " & Ls;");
      end case;
    end loop;

    Put_Line("    --------------------------------");
    Put_Line("    function Get_String(S : in String; Ret : in Boolean) return String is");
    Put_Line("    begin");
    Put_Line("      if Ret then return S; else return " & Quote("") & "; end if;");
    Put_Line("    end Get_String;");
    Put_Line("    --------------------------------");
    Put_Line("  begin");
    Put_Line("    return Get_String(S_Start, Ret_Start) & ");
    Put_Line("           Get_String(");
    Put("                      ");
    for j in 1 .. Column_Counter loop
      Put(" S" & Utils.Trim(j'Img));
      if j < Column_Counter then
        Put(" & ");
      else
        Put(" , ");
      end if;
    end loop;
    Put_Line("Ret_Data) &");
    Put_Line("           Get_String(S_End, Ret_End) & Ascii.LF;");
    Put_Line("  end To_Xml;");
    Put_Line("    --------------------------------");


    Put_Line("");
    Put_Line("  --------------------------------------------");
    Put_Line("  type " & Self.Name.Camel_Case & "_Reader is new Sax.Readers.Reader with record");
    Put_Line("    Current_Tag    : Unbounded_String := Null_Unbounded_String;");
    Put_Line("    Accumulated    : Unbounded_String := Null_Unbounded_String;");
    Put_Line("    OK             : Boolean := True;");
    Put_Line("    Found_Set      : Boolean := True;");
    Put_Line("    " & Self.Name.Camel_Case & "_List     : Table_" & Self.Name.Camel_Case & "." & Self.Name.Camel_Case & "_List_Pack2.List;");
    Put_Line("    " & Self.Name.Camel_Case & "_Data     : Table_" & Self.Name.Camel_Case & ".Data_Type := Empty_Data;");
    Put_Line("  end record;");
    Put_Line("");
    Put_Line("  overriding procedure Start_Element(Handler       : in out " & Self.Name.Camel_Case & "_Reader;");
    Put_Line("                                     Namespace_URI : Unicode.CES.Byte_Sequence := " & Quote("") & ";");
    Put_Line("                                     Local_Name    : Unicode.CES.Byte_Sequence := " & Quote("") & ";");
    Put_Line("                                     Qname         : Unicode.CES.Byte_Sequence := " & Quote("") & ";");
    Put_Line("                                     Atts          : Sax.Attributes.Attributes'Class);");
    Put_Line("");
    Put_Line("  overriding procedure End_Element(Handler         : in out " & Self.Name.Camel_Case & "_Reader;");
    Put_Line("                                   Namespace_URI   : Unicode.CES.Byte_Sequence := " & Quote("") & ";");
    Put_Line("                                   Local_Name      : Unicode.CES.Byte_Sequence := " & Quote("") & ";");
    Put_Line("                                   Qname           : Unicode.CES.Byte_Sequence := " & Quote("") & ");");
    Put_Line("");
    Put_Line("  overriding procedure Characters(Handler          : in out " & Self.Name.Camel_Case & "_Reader;");
    Put_Line("                                  Ch               : Unicode.CES.Byte_Sequence := " & Quote("") & ");");
    Put_Line("");
    Put_Line("  --------------------------------------------");
    Put_Line("  overriding procedure Start_Element(Handler       : in out " & Self.Name.Camel_Case & "_Reader;");
    Put_Line("                          Namespace_URI : Unicode.CES.Byte_Sequence := " & Quote("") & ";");
    Put_Line("                          Local_Name    : Unicode.CES.Byte_Sequence := " & Quote("") & ";");
    Put_Line("                          Qname         : Unicode.CES.Byte_Sequence := " & Quote("") & ";");
    Put_Line("                          Atts          : Sax.Attributes.Attributes'Class) is");
    Put_Line("    pragma Warnings(Off,Namespace_URI);");
    Put_Line("    pragma Warnings(Off,Qname);");
    Put_Line("    pragma Warnings(Off,Atts);");
    Put_Line("    The_Tag : constant String := Local_Name;");
    Put_Line("  begin");
    Put_Line("    Handler.Current_Tag := To_Unbounded_String(The_Tag);");
    Put_Line("    Handler.Accumulated := Null_Unbounded_String;");
    Put_Line("    if The_Tag = Table_" & Self.Name.Camel_Case & "_Set_Name then");
    Put_Line("      Handler.Found_Set := True;");
    Put_Line("    end if;");
    Put_Line("  exception");
    Put_Line("    when Ada.Strings.Length_Error => Handler.OK := False;");
    Put_Line("    when Constraint_Error         => Handler.OK := False;");
    Put_Line("  end Start_Element;");
    Put_Line("  --------------------------------------------");
    Put_Line("");
    Put_Line("  --------------------------------------------");
    Put_Line("  overriding procedure End_Element(Handler       : in out " & Self.Name.Camel_Case & "_Reader;");
    Put_Line("                        Namespace_URI : Unicode.CES.Byte_Sequence := " & Quote("") & ";");
    Put_Line("                        Local_Name    : Unicode.CES.Byte_Sequence := " & Quote("") & ";");
    Put_Line("                        Qname         : Unicode.CES.Byte_Sequence := " & Quote("") & ") is");
    Put_Line("    pragma Warnings(Off,Namespace_URI);");
    Put_Line("    pragma Warnings(Off,Qname);");
    Put_Line("    The_Tag : constant String := Local_Name;");
    Put_Line("  begin");
    Put_Line("    if The_Tag = Table_" & Self.Name.Camel_Case & "_Set_Name then");
    Put_Line("      Handler.Found_Set := False;");
    Put_Line("    elsif The_Tag = Table_" & Self.Name.Camel_Case & "_Row_Name then");
    Put_Line("      if Handler.Found_Set then");
    Put_Line("        Handler." & Self.Name.Camel_Case & "_List.Append(Handler." & Self.Name.Camel_Case & "_Data);");
    Put_Line("        Handler." & Self.Name.Camel_Case & "_Data := Empty_Data;");
    Put_Line("      end if;");
    Put_Line("    end if;");
    Put_Line("  exception");
    Put_Line("    when Ada.Strings.Length_Error => Handler.OK := False;");
    Put_Line("  end End_Element;");
    Put_Line("  --------------------------------------------");
    Put_Line("");

    Put_Line("  --------------------------------------------");
    Put_Line("  overriding procedure Characters(Handler          : in out " & Self.Name.Camel_Case & "_Reader;");
    Put_Line("                       Ch               : Unicode.CES.Byte_Sequence := " & Quote("") & ") is");
    Put_Line("    function To_Iso_Latin_15(Str : Unicode.CES.Byte_Sequence) return String is");
    Put_Line("      use Unicode.Encodings;");
    Put_Line("    begin");
    Put_Line("      return  Convert(Str, Get_By_Name(" & Quote("utf-8") & "), Get_By_Name(" & Quote("iso-8859-15") & "));");
    Put_Line("    end To_Iso_Latin_15;");
    Put_Line("    The_Tag   : constant String := To_String(Handler.Current_Tag);");
    Put_Line("    The_Value : constant String := To_Iso_Latin_15(Ch);");
    Put_Line("    procedure Fix_String (Value    : String;");
    Put_Line("                          Variable : in out String) is");
    Put_Line("    begin");
    Put_Line("      Append(Handler.Accumulated, Value);");
    Put_Line("      Ada.Strings.Fixed.Move(To_String(Handler.Accumulated), Variable);");
    Put_Line("    end Fix_String;");
    Put_Line("  begin");
    Put_Line("    if Handler.Found_Set then");
    declare
      Condition_Text : String_Object;
    begin
    Condition_Text.Set("if    ");
      for Col of Self.Columns loop

        Put_Line("      " & Condition_Text.Lower_Case & " The_Tag = " & Col.Name.Camel_Case & "_Name then");
        case Data_Type(Col.Type_Of) is
          when A_Char =>
            if Col.Size_Of = 1 then
              Put_Line("       Handler." & Self.Name.Camel_Case & "_Data." & Col.Name.Camel_Case & " := The_Value(1);");
            else
              Put_Line("       Fix_String(The_Value, Handler." & Self.Name.Camel_Case & "_Data." & Col.Name.Camel_Case & ");");
            end if;
          when A_Int | A_Long | A_Short_Code =>
            Put_Line("       Handler." & Self.Name.Camel_Case & "_Data." & Col.Name.Camel_Case & " := Integer_4'value(The_Value);");
          when A_Big_Int =>
            Put_Line("       Handler." & Self.Name.Camel_Case & "_Data." & Col.Name.Camel_Case & " := Integer_8'value(The_Value);");
          when A_Boolean =>
            Put_Line("       Handler." & Self.Name.Camel_Case & "_Data." & Col.Name.Camel_Case & " := Boolean'value(The_Value);");
          when A_Float .. A_Double =>
            Put_Line("       Handler." & Self.Name.Camel_Case & "_Data." & Col.Name.Camel_Case & " := Fixed_Type'value(The_Value);");
          when A_Date =>
            Put_Line("       Handler." & Self.Name.Camel_Case & "_Data." & Col.Name.Camel_Case & " := Calendar2.To_Time_Type(The_Value," & Quote("00:00:00.000") & ");");
          when A_Time =>
            Put_Line("       Handler." & Self.Name.Camel_Case & "_Data." & Col.Name.Camel_Case & " := Calendar2.To_Time_Type(" & Quote("01-JAN-1901") & ",The_Value);");
          when A_Timestamp =>
            Put_Line("       Handler." & Self.Name.Camel_Case & "_Data." & Col.Name.Camel_Case & " := Calendar2.To_Time_Type(The_Value(1..11), The_Value(13..24));");
          when A_Clob .. A_Blob | A_Char_Code  =>
          Put_Line( "          null; --" & Quote(Col.Name.Camel_Case & " = not supported datatype for XML " & Data_Type(Col.Type_Of)'Img) & ";");
        end case;
        Condition_Text.Set("elsif ");
      end loop;
      Put_Line("      end if;");
    end;

    Put_Line("    end if;");
    Put_Line("  exception");
    Put_Line("    when Ada.Strings.Length_Error => Handler.OK := False;");
    Put_Line("  end Characters;");
    Put_Line("");
    Put_Line("  --------------------------------------------");
    Put_Line("");


    Code_Debug(" -- stop Print_XML_Functions_Body");
  end Print_XML_Functions_Body;
  --------------------------------

  procedure Print_Def_Functions_Lists_Body(Self : in out Table_Type ) is
    use Text_Io;
  --##--##--##--##--##--##--##--##--

    procedure Index_Procs_Lists_Body(Idx : I.Index_Type; Table_Name : String_Object) is
      Fields, All_Fields, Stm  : String_Object;
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
      Idx_Column_List_2        : C.Columns_Type with Warnings => Off;
    begin
      Code_Debug(" -- start Print_Def_Functions_Lists_Spec.Index_Procs_Lists_Body");

      for Idx_Col of Idx.Column_List loop
        All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
      end loop;

      Put_Line("");
      Put_Line("  procedure Read" & All_Fields.Camel_Case & "(");
      Put_Line("                           Data  : in out Table_" & Table_Name.Camel_Case & ".Data_Type'class;");
      Put_Line("                           List  : in out " & Table_Name.Camel_Case & "_List_Pack2.List;");
      Put_Line("                           Order : in     Boolean := False;");
      Put_Line("                           Max   : in     Integer_4 := Integer_4'Last) is");
      Put_Line("    use Sql;");
      Put_Line("    Start_Trans : constant Boolean := Sql.Transaction_Status = Sql.None;");
      Put_Line("    Transaction : Sql.Transaction_Type;");
      Put_Line("  begin");
      Put_Line("    if Start_Trans then Transaction.Start; end if;");
      Put_Line("    if Order then");
      Stm.Set("Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & All_Fields.Camel_Case & "_O");

      Index_Sql_Statement(T             => Self,
                          Stm_Name      => Stm.Fix_String,
                          First_Stm_Row => "select * from " & Table_Name.Upper_Case,
                          Order_By_PK   => True,
                          Field_List    => Idx.Column_List);

      Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx.Column_List );


      Put_Line("      Read_List(" & Stm.Fix_String & ", List, Max);");
      Put_Line("    else");
      Stm.Set("Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & All_Fields.Camel_Case );
      Index_Sql_Statement(T             => Self,
                          Stm_Name      => Stm.Fix_String,
                          First_Stm_Row => "select * from " & Table_Name.Upper_Case,
                          Order_By_PK   => False,
                          Field_List    => Idx.Column_List);

      Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx.Column_List );
      Put_Line("      Read_List(" & Stm.Fix_String & ", List, Max);");
      Put_Line("    end if;");
      Put_Line("    if Start_Trans then Transaction.Commit; end if;      ");
      Put_Line("  end Read" & All_Fields.Camel_Case & ";");
      Put_Line("-----------------------------------------------");



      if Index_Has_Several_Fields then
        -- we do not want the last idx field.
        -- however if we have 3 fields in idx
        --  is_existing uses
        -- Read_I1_Fname_1
        -- Read_11_Fname_1_Fname_2
        -- Read_Fname_1_Fname_2_Fname3
        -- so always generate
        --Read_Fname_1_Fname_2_Fname3 and Read_I1_Fname_1_Fname_2_Fname3
        -- so we do not use
        -- Idx_Column_List_1.Delete_Last;

        -- do the loop that prints stuff
        for Idx_Col of Idx_Column_List_1 loop
          Idx_Column_List_2.Append(Idx_Col);
          Fields.Set(Fields.Camel_Case & '_' & Idx_Col.Name.Camel_Case);

          Put_Line("");
          Put_Line("  procedure Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & "(");
          Put_Line("                           Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type'class;");
          Put_Line("                           List  : in out " & Table_Name.Camel_Case & "_List_Pack2.List;");
          Put_Line("                           Order : in     Boolean := False;");
          Put_Line("                           Max   : in     Integer_4 := Integer_4'Last) is");
          Put_Line("    use Sql;");
          Put_Line("    Start_Trans : constant Boolean := Sql.Transaction_Status = Sql.None;");
          Put_Line("    Transaction : Sql.Transaction_Type;");
          Put_Line("  begin");
          Put_Line("    if Start_Trans then Transaction.Start; end if;");
          Put_Line("    if Order then");
          Stm.Set("Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & "_O");

          Index_Sql_Statement(T             => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "select * from " & Table_Name.Upper_Case,
                              Order_By_PK   => True,
                              Field_List    => Idx_Column_List_2);

          Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx_Column_List_2 );

          Put_Line("      Read_List(" & Stm.Fix_String & ", List, Max);");
          Put_Line("    else");
          Stm.Set("Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case );
          Index_Sql_Statement(T             => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "select * from " & Table_Name.Upper_Case,
                              Order_By_PK   => False,
                              Field_List    => Idx_Column_List_2);

          Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx_Column_List_2 );
          Put_Line("      Read_List(" & Stm.Fix_String & ", List, Max);");
          Put_Line("    end if;");
          Put_Line("    if Start_Trans then Transaction.Commit; end if;      ");
          Put_Line("  end Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & ";");
          Put_Line("");

        end loop ;
      end if; --Index_Has_Several_Fields

      Idx_Column_List_1.Clear;
      Idx_Column_List_2.Clear;
      Code_Debug(" -- stop Print_Def_Functions_Lists_Spec.Index_Procs_Lists_Body");
      Put_Line("");
      end Index_Procs_Lists_Body;

    --##--##--##--##--##--##--##--##--

    procedure Unique_Procs_Lists_Body(Idx : I.Index_Type ; Table_Name : String_Object) is
      Fields, All_Fields, Stm  : String_Object;
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1        : C.Columns_Type := Idx.Column_List.Copy with Warnings => Off;
      Idx_Column_List_2        : C.Columns_Type with Warnings => Off;
    begin
      Code_Debug(" -- start Print_Def_Functions_Lists_Body.Unique_Procs_Lists_Body");
      for Idx_Col of Idx.Column_List loop
        All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
      end loop;

      if Index_Has_Several_Fields then
        -- we do not want the last idx field.
        -- however if we have 3 fields in idx
        --  is_existing uses
        -- Read_I1_Fname_1
        -- Read_11_Fname_1_Fname_2
        -- Read_Fname_1_Fname_2_Fname3
        -- so always generate
        --Read_Fname_1_Fname_2_Fname3 and Read_I1_Fname_1_Fname_2_Fname3
        -- so we do not use
        -- Idx_Column_List_1.Delete_Last;

        -- do the loop that prints stuff
        for Idx_Col of Idx_Column_List_1 loop
          Idx_Column_List_2.Append(Idx_Col);
          Fields.Set(Fields.Camel_Case & '_' & Idx_Col.Name.Camel_Case);
          Put_Line("");
          Put_Line("  procedure Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & "(");
          Put_Line("                           Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type'class;");
          Put_Line("                           List  : in out " & Table_Name.Camel_Case & "_List_Pack2.List;");
          Put_Line("                           Order : in     Boolean := False;");
          Put_Line("                           Max   : in     Integer_4 := Integer_4'Last) is");
          Put_Line("    use Sql;");
          Put_Line("    Start_Trans : constant Boolean := Sql.Transaction_Status = Sql.None;");
          Put_Line("    Transaction : Sql.Transaction_Type;");
          Put_Line("  begin");
          Put_Line("    if Start_Trans then Transaction.Start; end if;");
          Put_Line("    if Order then");
          Stm.Set("Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & "_Unique_O");

          Index_Sql_Statement(T             => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "select * from " & Table_Name.Upper_Case,
                              Order_By_PK   => True,
                              Field_List    => Idx_Column_List_2);

          Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx_Column_List_2 );


          Put_Line("      Read_List(" & Stm.Fix_String & ", List, Max);");
          Put_Line("    else");
          Stm.Set("Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & "_Unique" );
          Index_Sql_Statement(T             => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "select * from " & Table_Name.Upper_Case,
                              Order_By_PK   => False,
                              Field_List    => Idx_Column_List_2);

          Set_Index_Sql_Statement(T => Self, Stm_Name => Stm.Fix_String, Field_List => Idx_Column_List_2 );
          Put_Line("      Read_List(" & Stm.Fix_String & ", List, Max);");
          Put_Line("    end if;");
          Put_Line("    if Start_Trans then Transaction.Commit; end if;      ");
          Put_Line("  end Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & ";");
          Put_Line("");
        end loop ;
      else

        Stm.Set("Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & All_Fields.Camel_Case & "_Unique");
        Put_Line("");
        Put_Line("  -- unique index");
        Put_Line("  procedure Read" & All_Fields.Camel_Case & "(Data       : in out Table_" & Table_Name.Camel_Case & ".Data_Type;");
        Put_Line("                     End_Of_Set : in out Boolean) is");
        Put_Line("    use Sql;");
        Put_Line("    Start_Trans : constant Boolean := Sql.Transaction_Status = Sql.None;");
        Put_Line("    Transaction : Sql.Transaction_Type;");
        Put_Line("  begin");
        Put_Line("    if Start_Trans then Transaction.Start; end if;");

        Keyed_Sql_Statement(Self          => Self,
                            Stm_Name      => Stm.Fix_String,
                            First_Stm_Row => "select * from " & Table_Name.Upper_Case,
                            Key           => I.Unique);

        Set_Keyed_Sql_Statement(Self      => Self,
                                Stm_Name  => Stm.Fix_String,
                                Key       => I.Unique);

        Put_Line("    " & Stm.Fix_String & ".Open_Cursor;");
        Put_Line("    " & Stm.Fix_String & ".Fetch(End_Of_Set);");
        Put_Line("    if not End_Of_Set then");
        Put_Line("      Data := Get(" & Stm.Fix_String & ");");
        Put_Line("    end if;");
        Put_Line("    " & Stm.Fix_String & ".Close_Cursor;");
        Put_Line("    if Start_Trans then Transaction.Commit; end if;");
        Put_Line("  end Read" & All_Fields.Camel_Case & ";");
        Put_Line("");
        Put_Line("  --------------------------------------------");

      end if; --Index_Has_Several_Fields

      Idx_Column_List_1.Clear;
      Idx_Column_List_2.Clear;

      Code_Debug(" -- stop  Print_Def_Functions_Lists_Body.Unique_Procs_Lists_Body");
      Put_Line("");
    end Unique_Procs_Lists_Body;

    --##--##--##--##--##--##--##--##--

    procedure Primary_Procs_Lists_Body(Idx : I.Index_Type ; Table_Name : String_Object) is
      Index_Has_Several_Fields : Boolean        := Integer(Idx.Column_List.Length) > 1;
      Idx_Column_List_1          : C.Columns_Type := Idx.Column_List.Copy;
      Idx_Column_List_2          : C.Columns_Type with Warnings => Off; -- do not pre-fill
      All_Fields, Fields, Stm    : String_Object;
    begin

      Code_Debug(" -- start Print_Def_Functions_Lists_Body.Primary_Procs_Lists_Body");
       -- Code_Debug(" -- Index_Has_Several_Fields : " & Index_Has_Several_Fields'Img);
      if Index_Has_Several_Fields then

        for Idx_Col of Idx.Column_List loop
          All_Fields.Set(All_Fields.Camel_Case & "_" & Idx_Col.Name.Camel_Case);
        end loop;

        -- we do not want the last idx field.
        -- however if we have 3 fields in idx
        --  is_existing uses
        -- Read_I1_Fname_1
        -- Read_11_Fname_1_Fname_2
        -- Read_Fname_1_Fname_2_Fname3
        -- so always generate
        --Read_Fname_1_Fname_2_Fname3 and Read_I1_Fname_1_Fname_2_Fname3
        -- so we do not use
        -- Idx_Column_List_1.Delete_Last;


        -- do the loop that prints stuff
        for Idx_Col of Idx_Column_List_1 loop
          -- exit on the last pk-column
          Idx_Column_List_2.Append(Idx_Col);

          Fields.Set(Fields.Camel_Case & '_' & Idx_Col.Name.Camel_Case);
          Put_Line("-------------------------------------------");
          Put_Line("");
          Put_Line("");
          Put_Line("  procedure Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & "(");
          Put_Line("                           Data  : in     Table_" & Table_Name.Camel_Case & ".Data_Type'class;");
          Put_Line("                           List  : in out " & Table_Name.Camel_Case & "_List_Pack2.List;");
          Put_Line("                           Order : in     Boolean := False;");
          Put_Line("                           Max   : in     Integer_4 := Integer_4'Last) is");
          Put_Line("    use Sql;");
          Put_Line("    Start_Trans : constant Boolean := Sql.Transaction_Status = Sql.None;");
          Put_Line("    Transaction : Sql.Transaction_Type;");
          Put_Line("  begin");
          Put_Line("    if Start_Trans then Transaction.Start; end if;");
          Put_Line("    if Order then");

          Stm.Set("Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & "_O");
          Keyed_Sql_Statement(Self          => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "select * from " & Table_Name.Upper_Case,
                              Key           => I.Primary,
                              Turns         => Integer_4(Idx_Column_List_2.Length),
                              Order_By_PK   => True);

          Set_Keyed_Sql_Statement(Self      => Self,
                                  Stm_Name  => Stm.Fix_String,
                                  Key       => I.Primary,
                                  Turns     => Integer_4(Idx_Column_List_2.Length));

          Put_Line("      Read_List(" & Stm.Camel_Case & " , List, Max);");
          Put_Line("    else");
          Stm.Set("Stm_Select_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case);
          Keyed_Sql_Statement(Self          => Self,
                              Stm_Name      => Stm.Fix_String,
                              First_Stm_Row => "select * from " & Table_Name.Upper_Case,
                              Key           => I.Primary,
                              Turns         => Integer_4(Idx_Column_List_2.Length),
                              Order_By_PK   => False);

          Set_Keyed_Sql_Statement(Self      => Self,
                                  Stm_Name  => Stm.Fix_String,
                                  Key       => I.Primary,
                                  Turns     => Integer_4(Idx_Column_List_2.Length));

          Put_Line("      Read_List(" & Stm.Camel_Case & ", List, Max);");
          Put_Line("    end if;");
          Put_Line("    if Start_Trans then Transaction.Commit; end if;");
          Put_Line("  end Read_I" & Utils.Trim(Idx.Sequence_Number'Img) & Fields.Camel_Case & ";");
          Put_Line("-------------------------------------------");
          Put_Line("");
        end loop ;
      end if; --Index_Has_Several_Fields
      Idx_Column_List_2.Clear;
      Code_Debug("   -- stop Print_Def_Functions_Lists_Body.Primary_Procs_Lists_Body");
    end Primary_Procs_Lists_Body;

    --    ##--##--##--##--##--##--##--##--
  begin --Print_Def_Functions_Lists_Body
    Code_Debug(" -- start Print_Def_Functions_Lists_Body");
    Put_Line("");
    Put_Line("  function To_Map (Data : Table_" & Self.Name.Camel_Case & ".Data_Type'class) return Table_" & Self.Name.Camel_Case & "." & Self.Name.Camel_Case & "_Map_Pack_String.Map is");

    Put_Line("    Tmp_Map :  Table_" & Self.Name.Camel_Case & "." & Self.Name.Camel_Case & "_Map_Pack_String.Map;");
    Put_Line("    Key : Key_Type := (others => ' ');");
    Put_Line("    Tmp_U_B_String : Unbounded_String := Null_Unbounded_String;");
    Put_Line("    Tmp_String     : String(1..1);");
    Put_Line("    pragma Warnings(Off, Tmp_String); -- used only if we have Character fields (ie String(1..1)");
    Put_Line("  begin");
    for Col of Self.Columns loop
      Put_Line("    Move(" & Quote(Col.Name.Upper_Case) & ", Key);");

      case Data_Type(Col.Type_Of) is
        when A_Char              =>
          if Col.Size_Of = 1 then
                                     Put_Line("    Tmp_String(1)  := Data." & Col.Name.Camel_Case & ";");
                                     Put_Line("    Tmp_U_B_String := To_Unbounded_String(Tmp_String);");
          else
                                     Put_Line("    Tmp_U_B_String := To_Unbounded_String(Data." & Col.Name.Camel_Case & ");");
          end if;
        when A_Int .. A_Long  |
           A_Short_Code       |
           A_Boolean          |
           A_Float .. A_Double   =>  Put_Line("    Tmp_U_B_String := To_Unbounded_String(Data." & Col.Name.Camel_Case & "'Img);");
        when A_Date              =>  Put_Line("    Tmp_U_B_String := To_Unbounded_String(String_Date(Data." & Col.Name.Camel_Case & "));");
        when A_Time              =>  Put_Line("    Tmp_U_B_String := To_Unbounded_String(String_Time(Data." & Col.Name.Camel_Case & "));");
        when A_Timestamp         =>  Put_Line("    Tmp_U_B_String := To_Unbounded_String(String_Date_And_Time(Data." & Col.Name.Camel_Case & "));");
        when A_Clob .. A_Blob |
             A_Char_Code         =>  Put_Line( "   null; --" & Quote(Col.Name.Camel_Case & " is not supported datatype for Map " & Data_Type(Col.Type_Of)'Img) & ";");
      end case;
      Put_Line("    Tmp_Map.Insert(Key, Tmp_U_B_String);");
      Put_Line("");
    end loop;
    Put_Line("    return Tmp_Map;");
    Put_Line("  end To_Map;");
    Put_Line(" ---------------------------");
    Put_Line("");
    Put_Line("");
    Put_Line("  function To_Map (Data : Table_" & Self.Name.Camel_Case & ".Data_Type'class) return Table_" & Self.Name.Camel_Case & "." & Self.Name.Camel_Case & "_Map_Pack_Column_Type.Map is");

    Put_Line("    Tmp_Map :  Table_" & Self.Name.Camel_Case & "." & Self.Name.Camel_Case & "_Map_Pack_Column_Type.Map;");
    Put_Line("    Key : Column_Type;");
    Put_Line("    Tmp_U_B_String : Unbounded_String := Null_Unbounded_String;");
    Put_Line("    Tmp_String     : String(1..1);");
    Put_Line("    pragma Warnings(Off, Tmp_String); -- used only if we have Character fields (ie String(1..1)");
    Put_Line("  begin");
    for Col of Self.Columns loop
      Put_Line("    Key := " & Col.Name.Camel_Case & ";");

      case Data_Type(Col.Type_Of) is
        when A_Char              =>
          if Col.Size_Of = 1 then
                                     Put_Line("    Tmp_String(1)  := Data." & Col.Name.Camel_Case & ";");
                                     Put_Line("    Tmp_U_B_String := To_Unbounded_String(Tmp_String);");
          else
                                     Put_Line("    Tmp_U_B_String := To_Unbounded_String(Data." & Col.Name.Camel_Case & ");");
          end if;
        when A_Int .. A_Long  |
           A_Short_Code       |
           A_Boolean          |
           A_Float .. A_Double   =>  Put_Line("    Tmp_U_B_String := To_Unbounded_String(Data." & Col.Name.Camel_Case & "'Img);");
        when A_Date              =>  Put_Line("    Tmp_U_B_String := To_Unbounded_String(String_Date(Data." & Col.Name.Camel_Case & "));");
        when A_Time              =>  Put_Line("    Tmp_U_B_String := To_Unbounded_String(String_Time(Data." & Col.Name.Camel_Case & "));");
        when A_Timestamp         =>  Put_Line("    Tmp_U_B_String := To_Unbounded_String(String_Date_And_Time(Data." & Col.Name.Camel_Case & "));");
        when A_Clob .. A_Blob |
             A_Char_Code         =>  Put_Line( "   null; --" & Quote(Col.Name.Camel_Case & " is not supported datatype for Map " & Data_Type(Col.Type_Of)'Img) & ";");
      end case;
      Put_Line("    Tmp_Map.Insert(Key, Tmp_U_B_String);");
      Put_Line("");
    end loop;
    Put_Line("    return Tmp_Map;");
    Put_Line("  end To_Map;");
    Put_Line(" ---------------------------");

    if Self.Entity_Type = Ud4 then
      -- only for db
       Code_Debug(" -- stop  Print_Def_Functions_Lists_Body");
      return ;
    end if;

    for Idx of Self.Indicies loop
      case Idx.Type_Of is
        when I.Primary => Primary_Procs_Lists_Body(Idx, Self.Name) ;
        when I.Unique  => Unique_Procs_Lists_Body(Idx, Self.Name) ;
        when I.Index   => Index_Procs_Lists_Body(Idx, Self.Name) ;
        when I.Functional => null;
      end case;
      Put_Line("");
    end loop;
    Code_Debug("  -- stop Print_Def_Functions_Lists_Body");
    Put_Line("");
  end Print_Def_Functions_Lists_Body;
  --------------------------------

  procedure Print_XML_Functions_Lists_Body(Self : in out Table_Type ) is
    use Text_Io;
  begin
    Code_Debug(" -- start Print_XML_Functions_Lists_Body");
    Put_Line("  procedure From_Xml(Xml_Filename : in Unbounded_String;");
    Put_Line("                     A_List       : in out " & Self.Name.Camel_Case & "_List_Pack2.List) is");
    Put_Line("    My_Reader   : " & Self.Name.Camel_Case & "_Reader;");
    Put_Line("    Input       : File_Input;");
    Put_Line("  begin");
    Put_Line("    My_Reader." & Self.Name.Camel_Case & "_List := A_List;");
    Put_Line("    My_Reader.Current_Tag := Null_Unbounded_String;");
    Put_Line("    Open(To_String(Xml_Filename), Input);");
    Put_Line("    My_Reader.Set_Feature(Validation_Feature,False);");
    Put_Line("    My_Reader.Parse(Input);");
    Put_Line("    Input.Close;");
    Put_Line("    if not My_Reader.OK then");
    Put_Line("       My_Reader." & Self.Name.Camel_Case & "_List.Clear;");
    Put_Line("    end if;");
    Put_Line("    A_List := My_Reader." & Self.Name.Camel_Case & "_List;");
    Put_Line("  end From_Xml;");
    Put_Line("");
    Code_Debug(" -- stop Print_XML_Functions_Lists_Body");
  end Print_XML_Functions_Lists_Body;
  --------------------------------

  procedure Print_Package_End_Body(Self : in out Table_Type ) is
    use Text_Io;
  begin
    Code_Debug(" -- start Print_Package_End_Body");
    Put_Line("end Table_" & Self.Name.Camel_Case & ";");
    Code_Debug(" -- stop Print_Package_End_Body");
  end Print_Package_End_Body;
  --------------------------------
  -- End bodies --

  procedure Print_Ada_Spec(Self : in out Table_Type ) is
  begin
    Self.Print_Header_Spec;
    Self.Print_Withs_Spec;
    Self.Print_Package_Start_Spec;
    Self.Print_Def_Functions_Spec;
--    Self.Print_Ud4_Functions_Spec;
    Self.Print_XML_Functions_Spec;
    Self.Print_Def_Functions_Lists_Spec;
    Self.Print_XML_Functions_Lists_Spec;
    Self.Print_Package_End_Spec;
  end Print_Ada_Spec;

  --------------------------------
  procedure Print_Ada_Body(Self : in out Table_Type ) is
  begin
    Self.Print_Header_Body;
    Self.Print_Withs_Body;
    Self.Print_Package_Start_Body;
    Self.Print_Def_Functions_Body;
--    Self.Print_Ud4_Functions_Body;
    Self.Print_XML_Functions_Body;
    Self.Print_Def_Functions_Lists_Body;
    Self.Print_XML_Functions_Lists_Body;
    Self.Print_Package_End_Body;
  end Print_Ada_Body;

  ------------------------------

  procedure Print_Ada(Self : in out Table_Type ) is
  begin
    Self.Print_Ada_Spec;
    Self.Print_Ada_Body;
  end Print_Ada;
  -----------------------------

  -- stop auto generating Ada packages --

end Repository.Table ;

