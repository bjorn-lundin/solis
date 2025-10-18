
with Sax;
with Sax.Readers;        use Sax.Readers;
with Input_Sources.File; use Input_Sources.File;
with Unicode.CES;
with Sax.Attributes;
with Utils;
--with Types;
with Ada.Environment_Variables;
with Ada.Directories;

package body Repository.Column is

  Tag_Term            : constant String := "Term";
  Tag_Presentation    : constant String := "Presentation";
  Tag_TranslationList : constant String := "TranslationList";
  Tag_Translation     : constant String := "Translation";
  Tag_CodeList        : constant String := "CodeList";
  Tag_CodeItem        : constant String := "CodeItem";
--  Tag_CodeListTr      : constant String := "CodeListTr";
  Tag_CodeItemTr      : constant String := "CodeItemTr";

--  Attr_Name           : constant String := "Name";
  Attr_Type           : constant String := "Type";
  Attr_Size           : constant String := "Size";
  Attr_Description    : constant String := "Description";
  Attr_Define         : constant String := "Define";
  Attr_Text           : constant String := "Text";
  Attr_Code           : constant String := "Code";
  Attr_Language       : constant String := "Language";
  Attr_Longdesc       : constant String := "LongDesc";
  Attr_Shortdesc      : constant String := "ShortDesc";


  type Reader is new Sax.Readers.Reader with record
    Column            : Column_Type;
    Config            : Config_Type;
    Current_Tag       : Unbounded_String := Null_Unbounded_String;
    Language          : Language_Type;
--    State            : Table_State_Type := Table_State_Type'First;
    Language_Is_Valid : Boolean          := False; -- until we get a valid lang
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

  function Code_Item_List_String( List : Ci.Code_Pkg.List) return String is
    Tmp : String_Object;
  begin
    if not List.Is_Empty then
      for Code_Item_Data of List loop
        Tmp.Set(Tmp.Fix_String & Code_Item_Data.To_String);
      end loop;
      return Ascii.Lf & Tmp.Fix_String;
    else
      return "";
    end if;
  end Code_Item_List_String;
  ------------------------------------------------
  function Translation_Array_String( Arr : Translation_Array_Type) return String is
    Tmp : String_Object;
  begin
    for i in Arr'range loop
      Tmp.Set(Tmp.Fix_String & i'Img & " " & Arr(i).To_String);
    end loop;
    return Ascii.Lf & Tmp.Fix_String;
  end Translation_Array_String;
  ------------------------------------------------
  overriding
  function To_String(Self : Column_Type) return String is
  begin
    return Self.Name.Fix_String & " " &
           Self.Type_Of'Img & " " &
           Self.Size_Of'Img & " " &
           Self.Presentation.To_String & " " &
           Code_Item_List_String(Self.Code_List) & " " &
           Translation_Array_String(Self.Translations);
  end To_String;
  ---------------------------------------------------------

  procedure Create(Self : in out Column_Type ; Config : Repository.Config_Type'class) is
                   My_Reader    : Reader;
                   Input        : File_Input;
  begin
    if Self.Name.UBString = Null_Unbounded_String then
      raise Sequence_Error with "Column name must be set before calling Create";
    end if;

    My_Reader.Column := Self;
    My_Reader.Config := Repository.Config_Type(Config);

    declare
      Filename : String := Utils.Lower_Case(
                               Config.Item(Terms).Directory.Fix_String & "/"  &
                               Config.Item(Terms).Prefix.Fix_String & "_" & Self.Name.Fix_String & ".xml");
      Xsdname  : String := Ada.Environment_Variables.Value(Name => "BOT_CONFIG") & "/repository/trm.xsd";
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
    Self := My_Reader.Column;
    Self.Is_Initialized := True;
  end Create;


  -------------------------------------------------------------------------------------
  procedure Create(Self : in out Column_Type ; Name : String; Config : Repository.Config_Type'class) is
  begin
    Self.Name.Set(Name);
    Self.Create(Config);
  end Create;

  -------------------------------------------------------------------------------------
  overriding
  procedure Reset(Self : in out Column_Type ) is
  begin
    Self.Name.Reset;
    Self.Type_Of := 0;
    Self.Size_Of := 0;
    Self.Define.Reset;
    Self.Description.Reset;

    Self.Presentation.Reset;
    if not Self.Code_List.Is_Empty then
      for Code_Item of Self.Code_List loop
        Code_Item.Reset;
      end loop;
      Self.Code_List.Clear;
    end if;

    for Lang in Self.Translations'range loop
      Self.Translations(Lang).Reset;
    end loop;
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
    -- The subsystem
--    Feedback("Start_Element " & The_Tag );

    if The_Tag = Tag_Term then
      Handler.Column.Type_Of := Type_Type'Value(Atts.Get_Value(Attr_Type));
      Handler.Column.Size_Of := Size_Type'Value(Atts.Get_Value(Attr_Size));

    elsif The_Tag = Tag_Presentation then
      null;

    elsif The_Tag = Tag_CodeList then
       --Handler.Column.Code_List    := Ci.Code_Pkg.Create;
       Handler.Column.Define.Set (To_Iso_Latin_15(Atts.Get_Value(Attr_Define)));
       Handler.Column.Description.Set(To_Iso_Latin_15(Atts.Get_Value(Attr_Description)));

    elsif The_Tag = Tag_CodeItem then
      declare
        Code_Item : Ci.Code_Item_Type;
      begin
        Code_Item.Code   := Code_Type'Value(Atts.Get_Value(Attr_Code));
        Code_Item.Text.Set(To_Iso_Latin_15(Atts.Get_Value(Attr_Text)));
        Code_Item.Define.Set(To_Iso_Latin_15(Atts.Get_Value(Attr_Define)));
        Handler.Column.Code_List.Append(Code_Item);
      end ;

    elsif The_Tag = Tag_TranslationList then
    --  Handler.Column.Translation_List := T.Translation_Pkg.Create;
      null;

    elsif The_Tag = Tag_Translation then
        Handler.Language_Is_Valid := True;
        Handler.Language := To_Language(Atts.Get_Value(Attr_Language));
        Handler.Column.Translations(Handler.Language).Long_Desc.Set (To_Iso_Latin_15(Atts.Get_Value(Attr_Longdesc)));
        Handler.Column.Translations(Handler.Language).Short_Desc.Set(To_Iso_Latin_15(Atts.Get_Value(Attr_Shortdesc)));

    --elsif The_Tag = Tag_CodeListTr then
      -- Handler.Column.Translations(Handler.Language).Code_List_Tr := Cit.Code_Tr_Pkg.Create;

    elsif The_Tag = Tag_CodeItemTr then
      declare
        Code_Item_Tr : Cit.Code_Item_Tr_Type;
      begin
        Code_Item_Tr.Code   := Code_Type'Value(Atts.Get_Value(Attr_Code));
        Code_Item_Tr.Text.Set(To_Iso_Latin_15(Atts.Get_Value(Attr_Text)));
        Handler.Column.Translations(Handler.Language).Code_List_Tr.Append(Code_Item_Tr);
      end ;

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
    pragma Unreferenced(Handler);
    The_Tag : constant String := Local_Name;
  begin
    if The_Tag = Tag_Translation then
      Handler.Language_Is_Valid := False;
    end if;
  exception
    when Ada.Strings.Length_Error =>
    	Feedback("Ada.Strings.Length_Error on Tag '" & The_Tag & "'");
  end End_Element;
  --++--++--++--++--++--++--++--++--++--++--++--++--++--

  overriding procedure Characters(Handler          : in out Reader;
                       Ch               : Unicode.CES.Byte_Sequence := "") is
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

  procedure Set_Name(Self : in out Column_Type; Filename : String; Config : Repository.Config_Type'class) is
    use Ada.Directories;
    Basename : String := Base_Name(Filename);
    Pos : Integer  := 0;
    Prefix : String := Config.Item(Terms).Prefix.Fix_String;
  begin
   -- Feedback("basename: " & Basename);
    Pos := Utils.Position( Basename,Prefix );
   -- Feedback("Pos: " & Pos'Img);
   -- Feedback("Prefix'Last: " & Prefix'Last'Img);
    if Pos /= Basename'First -1 then
     -- Feedback("Filename '" & Basename(Prefix'Last + 2 .. Basename'Last) & "'");
      Self.Name.Set(Basename(Prefix'Last + 2 .. Basename'Last));
    else
      raise Configuration_Error with "cannot set name from file '" & Filename & "'";
    end if;

  end Set_Name;
  ---------------------------------------------------------------

  ---------------------------------------------------------------



end Repository.Column ;

