

with Repository.Column;
with Repository.Index;
--with Sattmate_Types;        use Sattmate_Types;
with Ada.Containers.Doubly_Linked_Lists;

package Repository.Table is

  package C   renames Repository.Column;
  package I   renames Repository.Index;

  type Wiz_Type is new Integer_4 range 0 .. 3;
  type Ixx_Type_Type is (None, Date_Time, Timestamp);

  type Table_Column_Type is new C.Column_Type with record
    Nullable   : Boolean := False;
    Primary    : Boolean := False;
    Foreign    : Boolean := False;
    Indexed    : Boolean := False;
    Unique     : Boolean := False;
    Functional : Boolean := False;
  end record;
  overriding function To_String(Self : Table_Column_Type ) return String;

  package Columns_Pkg is new Ada.Containers.Doubly_Linked_Lists(Table_Column_Type);
  subtype Columns_Type is Columns_Pkg.List;

  package Indicies_Pkg is new Ada.Containers.Doubly_Linked_Lists(I.Index_Type, I."=");
  subtype Index_Type is Indicies_Pkg.List;



  type Table_Type is new Root_Type with record
    Name       : String_Object ;
    Wiz        : Wiz_Type := 3 ;
    Desc       : String_Object ;
    Tablespace : String_Object ;


-- Not used by sattmate:
--    LicId="0"
--    LogMask="0"
--    LogicId="0"
--    SafeAdd="0"
--    Loadable="1"
--    StorageClass="0"
--    CleanSetup="0"
--    CleanProd="0"

    Columns                      : Columns_Type;
    Indicies                     : Index_Type;
    Primary_Column_List          : Columns_Type; -- keeps all pks
    Primary_Column_List_But_Last : Columns_Type; -- keeps all pks but the last one
    Last_Primary_Column          : Table_Column_Type;
    Num_Primary_Keys             : Integer_4        := 0;
    Num_Columns                  : Integer_4        := 0;
    Ixx_Type                     : Ixx_Type_Type    := Ixx_Type_Type'First;
    All_Columns_Are_Primary      : Boolean          := False;
    Entity_Type                  : Entity_Type_Type := Db;
  end record;

  overriding function To_String(Self : Table_Type ) return String;
  procedure   Create(Self : in out Table_Type ; Config : Repository.Config_Type'class);
  procedure   Create(Self : in out Table_Type ; Name : String; Config : Repository.Config_Type'class);
  procedure   Create_Ud4(Self : in out Table_Type ; Config : Repository.Config_Type'class);
  procedure   Create_Ud4(Self : in out Table_Type ; Name : String; Config : Repository.Config_Type'class);
  procedure   Set_Name(Self : in out Table_Type; Filename : String; Config : Repository.Config_Type'class);
  overriding procedure Reset(Self : in out Table_Type ) ;

  -- should be hidden/private?
  procedure Print_Oracle_Create_DDL(Self : in out Table_Type);
  procedure Print_Sql_Server_Create_DDL(Self : in out Table_Type);
  procedure Print_Postgresql_Create_DDL(Self : in out Table_Type);
  procedure Print_Oracle_Drop_DDL(Self : in out Table_Type);
  procedure Print_Sql_Server_Drop_DDL(Self : in out Table_Type);
  procedure Print_Postgresql_Drop_DDL(Self : in out Table_Type);

  -- Start Headers  --
  procedure Print_Header_Spec(Self : in out Table_Type );
  procedure Print_Withs_Spec(Self : in out Table_Type );
  procedure Print_Package_Start_Spec(Self : in out Table_Type );
  procedure Print_Def_Functions_Spec(Self : in out Table_Type );
  procedure Print_Ud4_Functions_Spec(Self : in out Table_Type );
  procedure Print_XML_Functions_Spec(Self : in out Table_Type );
  procedure Print_Def_Functions_Lists_Spec(Self : in out Table_Type );
  procedure Print_XML_Functions_Lists_Spec(Self : in out Table_Type );
  procedure Print_Package_End_Spec(Self : in out Table_Type );
  -- End Headers  --
  -- Start Bodies --
  procedure Print_Header_Body(Self : in out Table_Type );
  procedure Print_Withs_Body(Self : in out Table_Type );
  procedure Print_Package_Start_Body(Self : in out Table_Type );
  procedure Print_Def_Functions_Body(Self : in out Table_Type );
  procedure Print_Ud4_Functions_Body(Self : in out Table_Type );
  procedure Print_XML_Functions_Body(Self : in out Table_Type );
  procedure Print_Def_Functions_Lists_Body(Self : in out Table_Type );
  procedure Print_XML_Functions_Lists_Body(Self : in out Table_Type );
  procedure Print_Package_End_Body(Self : in out Table_Type );
  procedure Print_Ada_Spec(Self : in out Table_Type );
  procedure Print_Ada_Body(Self : in out Table_Type );
  procedure Print_Ada(Self : in out Table_Type );
  -- End Bodies --



  procedure Keyed_Sql_Statement(Self : in out Table_Type ;
                                Stm_Name : String;
                                First_Stm_Row : String;
                                Key : I.Index_Type_Type;
                                Generate_IXX : Boolean := False;
                                Turns : Integer_4 := 99999;
                                Order_By_PK : Boolean := False);

  procedure Set_Keyed_Sql_Statement(Self : in out Table_Type ;
                                    Stm_Name : String;
                                    Key : I.Index_Type_Type;
                                    Generate_IXX : Boolean := False;
                                    Turns : Integer_4 := 99999) ;

  procedure Prepare_All_Columns(Self          : in out Table_Type;
                                Stm_Name      : in     String;
                                First_Stm_Row : in     String;
                                Where_Keys    : in     Boolean;
                                Old_IXX       : in     Boolean;
                                Set_Primary   : in     Boolean);

  procedure Set_All_Columns_Incl_Pk(Self          : in out Table_Type;
                            Stm_Name      : in     String;
                            Set_Old_IXX   : in     Boolean;
                            Set_Primary   : in     Boolean);

  procedure Set_All_Columns_But_Pk(Self          : in out Table_Type;
                            Stm_Name      : in     String;
                            Set_Old_IXX   : in     Boolean;
                            Set_Primary   : in     Boolean);



  procedure Insert_All_Columns(Self          : in out Table_Type;
                               Stm_Name      : in     String;
                               First_Stm_Row : in     String) ;


end Repository.Table ;
