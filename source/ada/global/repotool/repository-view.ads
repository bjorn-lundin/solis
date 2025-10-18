

with Repository.Column;
with Ada.Containers.Doubly_Linked_Lists; 

package Repository.View is

  package C   renames Repository.Column;
  
  type View_Type_Type is new Integer_4 range 2 .. 2;
  
  type View_Column_Type is new C.Column_Type with record
    Original_Table : String_Object;
    Primary  : Boolean := False;  
    As,
    As_Oracle,
    As_Postgresql,
    As_SqlServer : String_Object;
  end record;
  overriding function To_String(Self : View_Column_Type ) return String;
  overriding procedure Reset(Self : in out View_Column_Type );
  
  package Columns_Pkg is new Ada.Containers.Doubly_Linked_Lists(View_Column_Type);
  subtype Columns_Type is Columns_Pkg.List;
  
  type View_Type is new Root_Type with record
    Name            : String_Object ;
    Type_Of         : View_Type_Type := 2 ;
    Description     : String_Object ;
    Columns         : Columns_Type;
    From            : String_Object;
    From_Oracle     : String_Object;
    From_SqlServer  : String_Object;
    From_Postgresql : String_Object;
  end record;
  
  overriding function To_String(Self : View_Type ) return String;
  procedure   Create(Self : in out View_Type ; Config : Repository.Config_Type'class);
  procedure   Create(Self : in out View_Type ; Name : String; Config : Repository.Config_Type'class);
  procedure   Set_Name(Self : in out View_Type; Filename : String; Config : Repository.Config_Type'class);
  overriding procedure Reset(Self : in out View_Type ) ;

  -- should be hidden/private?
  procedure Print_Oracle_Create_DDL(Self : in out View_Type);
  procedure Print_Sql_Server_Create_DDL(Self : in out View_Type);
  procedure Print_Postgresql_Create_DDL(Self : in out View_Type);
  procedure Print_Oracle_Drop_DDL(Self : in out View_Type);
  procedure Print_Sql_Server_Drop_DDL(Self : in out View_Type);
  procedure Print_Postgresql_Drop_DDL(Self : in out View_Type);
  
  
end Repository.View ;
