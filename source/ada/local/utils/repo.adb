
pragma Warnings(Off);
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
pragma Warnings(On);
with Ada.Environment_Variables;

with Stacktrace;
--with Repository_Types;
with Types ; use Types;
with Repository;
with Repository.Presentation;
with Repository.Code_Item;
with Repository.Translation;
with Repository.Code_Item_Tr;
with Repository.Column;
with Repository.Index;
with Repository.Table;
with Repository.View;

with Text_Io; use Text_Io;

with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;

procedure Repo is
  use Repository;
  package EV renames  Ada.Environment_Variables;


pragma Warnings(Off);
  P        : Presentation.Presentation_Type;
  C_I      : Code_Item.Code_Item_Type;
  Tr       : Translation.Translation_Type;
  C_I_T    : Code_Item_Tr.Code_Item_Tr_Type;
  C        : Config_Type;
  I        : Repository.Index.Index_Type;
  Cols     : array (1..2) of Column.Column_Type;
  Col      : Column.Column_Type;
  T        : Table.Table_Type;
  V        : View.View_Type;

  CLI_Opts  : Command_Line_Configuration;
  Not_Implemented : exception;
pragma Warnings(On);


  Ia_Par_All_Tables                 : aliased Integer := 0;
  Ia_Par_Debug_Level                : aliased Integer := 0;
  Sa_Par_Help                       : aliased Gnat.Strings.String_Access;
  Sa_Par_Clreq_Name                 : aliased Gnat.Strings.String_Access;
  Sa_Par_Oracle_Table               : aliased Gnat.Strings.String_Access;
  Sa_Par_Sql_Server_Table           : aliased Gnat.Strings.String_Access;
  Sa_Par_Postgresql_Table           : aliased Gnat.Strings.String_Access;
  Sa_Par_Oracle_View                : aliased Gnat.Strings.String_Access;
  Sa_Par_Sql_Server_View            : aliased Gnat.Strings.String_Access;
  Sa_Par_Postgresql_View            : aliased Gnat.Strings.String_Access;
  Sa_Par_Table                      : aliased Gnat.Strings.String_Access;
  Sa_Par_Print_Internal_Table_Rep   : aliased Gnat.Strings.String_Access;
  Sa_Par_Print_Internal_Clreq_Rep   : aliased Gnat.Strings.String_Access;
  Sa_Par_Print_Internal_View_Rep    : aliased Gnat.Strings.String_Access;
  Sa_Par_C_Sharp_Class              : aliased Gnat.Strings.String_Access;
  Sa_Path_Info                      : aliased Gnat.Strings.String_Access;

  B_Par_List_Database_Tables        : aliased Boolean := False;
  B_Par_List_Clreq_Tables           : aliased Boolean := False;
  B_Par_List_Views                  : aliased Boolean := False;
  B_Par_List_Database_Tables_Full   : aliased Boolean := False;
  B_Par_List_Clreq_Tables_Full      : aliased Boolean := False;
  B_Par_List_Views_Full             : aliased Boolean := False;
  B_Par_Create_Table_Makefile       : aliased Boolean := False;
  B_Par_List_Coded_Values           : aliased Boolean := False;
  B_Par_List_Coded_Values_Full      : aliased Boolean := False;
  B_Par_Make_Coded_Values           : aliased Boolean := False;
  B_Make_Extract_Package            : aliased Boolean := False;
  B_Make_Xml_To_Ud4                 : aliased Boolean := False;
  B_Make_M2_Setup_Separates         : aliased Boolean := False;



  procedure Help(Config : Config_Type) is
  begin
    Put_Line(Standard_Error, "");
    Put_Line(Standard_Error, "The options that produces ada files are printed on stdout. Use redirect (>) to save to file");
    Put_Line(Standard_Error, "  eg 'repo --table=xloc > xloc.ada'");
    Put_Line(Standard_Error, "The file is NOT split into a body and spec file, use gnatchop for that");
    Put_Line(Standard_Error, "");
    Put_Line(Standard_Error, "The Table_XYZ.ad[bs] are divided into database tables");
    Put_Line(Standard_Error, "and clreqs. The clreq ones do NOT have any sql statements.");
    Put_Line(Standard_Error, "");
    Put_Line(Standard_Error, "The XYZ.sql are divided into Oracle, PostgreSQL and SqlServer.");
    Put_Line(Standard_Error, "PostgreSQL is NOT supported by Sattmate at the moment");
    Put_Line(Standard_Error, "");
    Put_Line(Standard_Error, "Input is xml files at");
    Put_Line(Standard_Error, Config.Item(Tables).Directory.Fix_String);
    Put_Line(Standard_Error, "and");
    Put_Line(Standard_Error, Config.Item(Terms).Directory.Fix_String);
    Put_Line(Standard_Error, "");
    Put_Line(Standard_Error, "-a 1|2|5..14");
    Put_Line(Standard_Error, "  ALL tables of one kind where");
    Put_Line(Standard_Error, "   1 -> Oracle     create table DDL files");
    Put_Line(Standard_Error, "   2 -> Sql-Server create table DDL files");
    -- Put_Line(Standard_Error, "  3 -> Table_XYZ.ad[bs] for clreqs.          Use gnatchop on result");
    -- Put_Line(Standard_Error, "  4 -> Table_XYZ.ad[bs] for database tables. Use gnatchop on result");
    Put_Line(Standard_Error, "   5 -> Postgresql create table DDL files");
    Put_Line(Standard_Error, "   6 -> DROP ALL tables, Oracle     sql DDL files");
    Put_Line(Standard_Error, "   7 -> DROP ALL tables, Sql-Server sql DDL files");
    Put_Line(Standard_Error, "   8 -> DROP ALL tables, Postgresql sql DDL files");
    Put_Line(Standard_Error, "   9 -> Oracle     create view DDL files");
    Put_Line(Standard_Error, "  10 -> Sql-Server create view DDL files");
    Put_Line(Standard_Error, "  11 -> Postgresql create view DDL files");
    Put_Line(Standard_Error, "  12 -> DROP ALL views, Oracle     sql DDL files");
    Put_Line(Standard_Error, "  13 -> DROP ALL views, Sql-Server sql DDL files");
    Put_Line(Standard_Error, "  14 -> DROP ALL views, Postgresql sql DDL files");
    Put_Line(Standard_Error, "");
    Put_Line(Standard_Error, "With ALL tables/clreqs, we mean the ones listed by -f and/or -g");

    Put_Line(Standard_Error, "");
  end Help;
  ------------------------------------------------------------------------------------------


  ------------------------------------------------------------------------------------------
begin
  C.Create;
  Set_Usage(CLI_Opts, Help => "This tool generates Table_XYZ.ad[bs] and XYZ.sql on standard output");

  Define_Switch(CLI_Opts,
                Ia_Par_All_Tables'access,
                Switch      => "-a:",
                Long_Switch => "--all=",
                Help        => "Do something for ALL tables where ARG in 1|2|5..14. Exact action, see below");

  Define_Switch(CLI_Opts,
                Sa_Par_Help'access,
                Switch      => "-h",
                Long_Switch => "--help",
                Help        => "Print help");

  Define_Switch(CLI_Opts,
                B_Par_List_Database_Tables'access,
                Switch      => "-f",
                Long_Switch => "--list_database_tables",
                Help        => "List all defined database tables on stdout");

  Define_Switch(CLI_Opts,
                B_Par_List_Clreq_Tables'access,
                Switch      => "-g",
                Long_Switch => "--list_clreq_tables",
                Help        => "List all defined clreq tables    on stdout");
                
  Define_Switch(CLI_Opts,
                B_Par_List_Views'access,
                Long_Switch => "--list_views",
                Help        => "List all defined views           on stdout");

  Define_Switch(CLI_Opts,
                B_Par_List_Coded_Values'access,
                Long_Switch => "--list_coded_values",
                Help        => "List of defined coded values     on stdout");
                
  Define_Switch(CLI_Opts,
                B_Par_List_Database_Tables_Full'access,
                Switch      => "-F",
                Long_Switch => "--list_database_tables_full",
                Help        => "List all defined database tables, full filename");

  Define_Switch(CLI_Opts,
                B_Par_List_Clreq_Tables_Full'access,
                Switch      => "-G",
                Long_Switch => "--list_clreq_tables_full",
                Help        => "List all defined clreq    tables, full filename");

  Define_Switch(CLI_Opts,
                B_Par_List_Views_Full'access,
                Long_Switch => "--list_views_full",
                Help        => "List all defined views,           full filename");              
                
  Define_Switch(CLI_Opts,
                B_Par_List_Coded_Values_Full'access,
                Long_Switch => "--list_coded_values_full",
                Help        => "List of defined coded values,     full filename");
                
  Define_Switch(CLI_Opts,
                B_Par_Create_Table_Makefile'access,
                Switch      => "-m",
                Long_Switch => "--table_makefile",
                Help        => "Create a makefile for the table_XYZ packages");

  Define_Switch(CLI_Opts,
                Sa_Par_Oracle_Table'access,
                Switch      => "-o:",
                Long_Switch => "--oracle=",
                Help        => "ONE XYZ.sql for database table, Oracle for ARG");

  Define_Switch(CLI_Opts,
                Sa_Par_Sql_Server_Table'access,
                Switch      => "-s:",
                Long_Switch => "--sql_server=",
                Help        => "ONE XYZ.sql for database table, SqlServer for ARG");

  Define_Switch(CLI_Opts,
                Sa_Par_Postgresql_Table'access,
                Switch      => "-p:",
                Long_Switch => "--postgresql=",
                Help        => "ONE XYZ.sql for database table, PostgreSQL for ARG");

  Define_Switch(CLI_Opts,
                Sa_Par_Oracle_View'access,
                Switch      => "-O:",
                Long_Switch => "--oracle_view=",
                Help        => "ONE XYZ.sql for database view, Oracle for ARG");

  Define_Switch(CLI_Opts,
                Sa_Par_Sql_Server_View'access,
                Switch      => "-S:",
                Long_Switch => "--sql_server_view=",
                Help        => "ONE XYZ.sql for database view, SqlServer for ARG");

  Define_Switch(CLI_Opts,
                Sa_Par_Postgresql_View'access,
                Switch      => "-P:",
                Long_Switch => "--postgresql_view=",
                Help        => "ONE XYZ.sql for database view, PostgreSQL for ARG");

  Define_Switch(CLI_Opts,
                Sa_Par_Table'access,
                Switch      => "-t:",
                Long_Switch => "--table=",
                Help        => "ONE Table_XYZ.[bs] for database table on stdout for ARG");

  Define_Switch(CLI_Opts,
                Sa_Par_Clreq_Name'access,
                Switch      => "-c:",
                Long_Switch => "--clreq=",
                Help        => "ONE Table_XYZ.ad[bs] for clreq call on stdout for ARG");
                
                
  Define_Switch(CLI_Opts,
                Sa_Par_Print_Internal_Table_Rep'access,
                Long_Switch => "--internal_table_rep=",
                Help        => "To_string for a table");
                
  Define_Switch(CLI_Opts,
                Sa_Par_Print_Internal_Clreq_Rep'access,
                Long_Switch => "--internal_clreq_rep=",
                Help        => "To_string for a clreq");
                
  Define_Switch(CLI_Opts,
                Sa_Par_Print_Internal_View_Rep'access,
                Long_Switch => "--internal_view_rep=",
                Help        => "To_string for a view");


  Define_Switch(CLI_Opts,
                B_Par_Make_Coded_Values'access,
                Long_Switch => "--make_coded_values",
                Help        => "print coded_values.ada on stdout");

  Define_Switch(CLI_Opts,
                Ia_Par_Debug_Level'access,
                Switch      => "-d:",
                Long_Switch => "--debug_level=",
                Help        => "insert commented debug info in generated code if > 0. Overrides SATTMATE_DEBUG_LEVEL");

  Define_Switch(CLI_Opts,
                B_Make_Extract_Package'access,
                Long_Switch => "--make_extract_package",
                Help        => "print make-extract-package on stdout");

  Define_Switch(CLI_Opts,
                B_Make_Xml_To_Ud4'access,
                Long_Switch => "--make_ud4",
                Help        => "print ud4 on stdout");

  Define_Switch(CLI_Opts,
                Sa_Par_C_Sharp_Class'access,
                Long_Switch => "--make_c_sharp=",
                Help        => "print c# class on stdout for ARG. ARG is a clreq.");
                
  Define_Switch(CLI_Opts,
                B_Make_M2_Setup_Separates'access,
                Long_Switch => "--make_m2_setup_separates",
                Help        => "print Insert/Delete/Update procedures as separates on stdout");

  Define_Switch(CLI_Opts,
                Sa_Path_Info'access,
                Switch      => "-i:",
                Long_Switch => "--path_info=",
                Help        => "path to repo. ARG is on of Tables|Views|Clreqs|Terms|Codes|Labels");
                


                
                
  Getopt (CLI_Opts);  -- process the command line


  if Ev.Exists("SATTMATE_DEBUG_LEVEL") then
    declare
      DL : Integer_4 := 0;
    begin
      DL := Integer_4'Value(Ev.Value("SATTMATE_DEBUG_LEVEL"));
      Repository.Set_Debug_Level(DL);
    exception
      when Constraint_Error => null;
    end;
  end if;
  -- override envvar if present on cmd_line
  if Ia_Par_Debug_Level > 0 then
    Repository.Set_Debug_Level(Integer_4(Ia_Par_Debug_Level));
  end if;
  -- find out what to do
  -- only 1 thing at a time so exit after each action

  if Ia_Par_All_Tables > 0 then
    case Ia_Par_All_Tables is
      when  1 => C.Print_DDL_Create_Table_For_All(Oracle) ;
      when  2 => C.Print_DDL_Create_Table_For_All(Sql_Server);
    --  when 3 => raise Not_Implemented with "Ia_Par_All_Tables=" & Ia_Par_All_Tables'Img;
    --  when 4 => raise Not_Implemented with "Ia_Par_All_Tables=" & Ia_Par_All_Tables'Img;
      when  5 => C.Print_DDL_Create_Table_For_All(Postgresql);
      when  6 => C.Print_DDL_Drop_Table_For_All(Oracle);
      when  7 => C.Print_DDL_Drop_Table_For_All(Sql_Server);
      when  8 => C.Print_DDL_Drop_Table_For_All(Postgresql);
      when  9 => C.Print_DDL_Create_View_For_All(Oracle) ;
      when 10 => C.Print_DDL_Create_View_For_All(Sql_Server);
      when 11 => C.Print_DDL_Create_View_For_All(Postgresql);
      when 12 => C.Print_DDL_Drop_View_For_All(Oracle);
      when 13 => C.Print_DDL_Drop_View_For_All(Sql_Server);
      when 14 => C.Print_DDL_Drop_View_For_All(Postgresql);
      when others =>
        raise Gnat.Command_Line.Invalid_Parameter with "Legal value for -all is 1|2|5..14 , not" & Ia_Par_All_Tables'Img;
    end case;
  elsif B_Par_List_Database_Tables then
    Put_Line(C.All_Entities_Defined_Names(Tables).Upper_Case);
    
  elsif B_Par_List_Database_Tables_Full then
    Put_Line(C.All_Entities_Defined_Full_Path(Tables).Fix_String);
    
  elsif B_Par_List_Clreq_Tables then
    Put_Line(C.All_Entities_Defined_Names(Clreqs).Upper_Case);
    
  elsif B_Par_List_Clreq_Tables_Full then
    Put_Line(C.All_Entities_Defined_Full_Path(Clreqs).Fix_String);

  elsif B_Par_List_Views then
    Put_Line(C.All_Entities_Defined_Names(Views).Upper_Case);

  elsif B_Par_List_Views_Full then
    Put_Line(C.All_Entities_Defined_Full_Path(Views).Fix_String);
    
  elsif B_Par_List_Coded_Values then
    Put_Line(C.List_Coded_Values(Name).Fix_String);
    
  elsif B_Par_List_Coded_Values_Full then
    Put_Line(C.List_Coded_Values(Full).Fix_String);

  elsif B_Par_Create_Table_Makefile then
    C.Create_Table_Makefile;
    
  elsif B_Par_Make_Coded_Values then
    C.Make_Coded_Values;
    
  elsif B_Make_Extract_Package then
    C.Make_Extract_Package;
    
  elsif B_Make_Xml_To_Ud4 then
    C.Make_Xml_To_Ud4;    
    
  elsif B_Make_M2_Setup_Separates then
    C.Make_M2_Setup_Separates;
    
  elsif Sa_Par_C_Sharp_Class.all /= "" then    
    declare
      Clq : Types.String_Object;
    begin
      Clq.Set(Sa_Par_C_Sharp_Class.all);
      C.Make_C_Sharp_Class(Clq);
      Clq.Reset;
    end;
    
  elsif Sa_Par_Oracle_Table.all /= "" then
    declare
      Tbl : Table.Table_Type;
    begin
      Tbl.Name.Set(Sa_Par_Oracle_Table.all);
      Tbl.Create(C);
      Tbl.Print_Oracle_Create_DDL;
      Tbl.Reset;
    end;
    
  elsif Sa_Par_Sql_Server_Table.all /= "" then
    declare
      Tbl : Table.Table_Type;
    begin
      Tbl.Name.Set(Sa_Par_Sql_Server_Table.all);
      Tbl.Create(C);
      Tbl.Print_Sql_Server_Create_DDL;
      Tbl.Reset;
    end;

  elsif Sa_Par_Postgresql_Table.all /= "" then
    declare
      Tbl : Table.Table_Type;
    begin
      Tbl.Name.Set(Sa_Par_Postgresql_Table.all);
      Tbl.Create(C);
      Tbl.Print_Postgresql_Create_DDL;
      Tbl.Reset;
    end;
    
  elsif Sa_Par_Oracle_View.all /= "" then
    declare
      Vw : View.View_Type;
    begin
      Vw.Name.Set(Sa_Par_Oracle_View.all);
      Vw.Create(C);
      Vw.Print_Oracle_Create_DDL;
      Vw.Reset;
    end;
    
  elsif Sa_Par_Sql_Server_View.all /= "" then
    declare
      Vw : View.View_Type;
    begin
      Vw.Name.Set(Sa_Par_Sql_Server_View.all);
      Vw.Create(C);
      Vw.Print_Sql_Server_Create_DDL;
      Vw.Reset;
    end;

  elsif Sa_Par_Postgresql_View.all /= "" then
    declare
      Vw : View.View_Type;
    begin
      Vw.Name.Set(Sa_Par_Postgresql_View.all);
      Vw.Create(C);
      Vw.Print_Postgresql_Create_DDL;
      Vw.Reset;
    end;
    
  elsif Sa_Par_Table.all /= "" then
    declare
      Tbl : Table.Table_Type;
    begin
      Tbl.Name.Set(Sa_Par_Table.all);
      Tbl.Create(C);
      Tbl.Print_Ada;
      Tbl.Reset;
    end;
    
  elsif Sa_Par_Clreq_Name.all /= "" then
    declare
      Tbl : Table.Table_Type;
    begin
      Tbl.Name.Set(Sa_Par_Clreq_Name.all);
      Tbl.Create_Ud4(C);
      Tbl.Print_Ada;
      Tbl.Reset;
    end;
    
  elsif Sa_Par_Print_Internal_Table_Rep.all /= "" then
    declare
      Tbl : Table.Table_Type;
    begin
      Tbl.Name.Set(Sa_Par_Print_Internal_Table_Rep.all);
      Tbl.Create(C);
      Put_Line(Tbl.To_String);
      Tbl.Reset;
    end;

  elsif Sa_Par_Print_Internal_Clreq_Rep.all /= "" then
    declare
      Clq : Table.Table_Type;
    begin
      Clq.Name.Set(Sa_Par_Print_Internal_Clreq_Rep.all);
      Clq.Create_Ud4(C);
      Put_Line(Clq.To_String);
      Clq.Reset;
    end;

  elsif Sa_Par_Print_Internal_View_Rep.all /= "" then
    declare
      Vw : View.View_Type;
    begin
      Vw.Name.Set(Sa_Par_Print_Internal_View_Rep.all);
      Vw.Create(C);
      Put_Line(Vw.To_String);
      Vw.Reset;
    end;

    
  elsif Sa_Path_Info.all /= "" then
    declare
      Typ : Types.String_Object;
      Did_Print : Boolean := False;
    begin
      Typ.Set(Sa_Path_Info.all);
      for i in Repository.Config_Type_Type'range loop
        if i'Img = Typ.Upper_Case then
          Put_Line(C.Item(i).Directory.Fix_String);
          Did_Print := True;
          exit;
        end if;
      end loop;
      if not Did_Print then
        Typ.Reset;
        Put_Line (Standard_Error, "ARG must b in one of");
        for i in Repository.Config_Type_Type'range loop
          Typ.Append(i'Img & ", ");
        end loop;
        Typ.Delete_Last_Char;
        Typ.Delete_Last_Char;
        Put_Line(Standard_Error, Typ.Lower_Case);
      end if;      
    end;
    
  else
 
    Help(C);
    Put_Line(Standard_Error, "");
    Put_Line(Standard_Error, "---------------------------------------------------------------------");
    Put_Line(Standard_Error, "  More help on options is available with 'repo -h' or 'repo --help'");
    Put_Line(Standard_Error, "---------------------------------------------------------------------");
    Put_Line(Standard_Error, "");   
  end if;

exception
  when F: Gnat.Command_Line.Invalid_Parameter      =>
    Put_Line(Standard_Error, "Invalid_Parameter - Raised when a parameter is missing, or out of range");
    Put_Line(Standard_Error, Ada.Exceptions.Exception_Message(F));

  when Gnat.Command_Line.Invalid_Switch         =>
    Put_Line(Standard_Error, "Invalid_Switch - Raised when a switch is undefined");
  when Gnat.Command_Line.Exit_From_Command_Line => Help(C);
  when E: others                                => Stacktrace.Tracebackinfo(E);
end Repo;
