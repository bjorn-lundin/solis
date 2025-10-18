------------------------------------------------------------------------------
--                                                                          --
--                        S Q L                                             --
--                                                                          --
--                       S p e c                                            --
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
with Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Pgada.Database; use Pgada.Database;
with Ada.Finalization; use Ada.Finalization;
with Calendar2;
with Types; use Types;

--with Simple_List_Class;
--pragma Elaborate_All (Simple_List_Class);
with Ada.Containers.Doubly_Linked_Lists;

package Sql is

   Not_Connected             : exception;
   No_Transaction            : exception;
   Duplicate_Index           : exception;
   No_Such_Row               : exception;
   Transaction_Conflict      : exception;
   Postgresql_Error          : exception;
   Too_Many_Cursors          : exception;
   Sequence_Error            : exception;
   Too_Many_Input_Parameters : exception;
   No_Such_Column            : exception;
   Null_Value                : exception;
   Transaction_Error         : exception;
   Sql_Error                 : exception;
   No_Such_Object            : exception;
   No_Such_Parameter         : exception;
   Conversion_Error          : exception;

   type Database_Type is (Postgresql);

   -- Function Database returns the name of the current database.
   function Database return Database_Type;

   type Transaction_Status_Type is (None, Read_Write, Read_Only);
   type Transaction_Isolation_Level_Type is (Read_Commited, Serializable);
   type Transaction_Isolation_Level_Scope_Type is (Transaction, Session);

   type Statement_Type is new Limited_Controlled with private ;

   -----------------------------------------------------------

   procedure Close_Session;
   --http://www.postgresql.org/docs/9.3/static/libpq-connect.html
   procedure Connect (Host       : in String  := "";
                      Port       : in Natural := 5432;
                      Options    : in String  := "";
                      Tty        : in String  := "";
                      Db_Name    : in String  := "";
                      Login      : in String := "";
                      Password   : in String := "";
                      SSL_Mode   : in String := "prefer"); --disable,allow,prefer,require,verify-ca,verify-full

   function  Is_Session_Open return Boolean;

   -- Start with Connect
   -- There can be one connection only to a database.
   -- This package is NOT task-safe
   -- if several tasks are needed, create a serialising task, that handles ALL db interaction



   -----------------------------------------------------------

   procedure Set_Transaction_Isolation_Level (Level : in Transaction_Isolation_Level_Type;
                                              Scope : in Transaction_Isolation_Level_Scope_Type);

   type Transaction_Type is new Limited_Controlled with private;

   procedure Start_Read_Write_Transaction (T : in out Transaction_Type);
   procedure Start (T : in out Transaction_Type) renames Start_Read_Write_Transaction;
   procedure Start_Read_Only_Transaction (T : in out Transaction_Type);
   procedure Commit (T : in out Transaction_Type) ;
   procedure Rollback (T : in out Transaction_Type) ;

   function  Transaction_Status return Transaction_Status_Type;

   -----------------------------------------------------------

   procedure Prepare (Statement : in out Statement_Type;
                      Command   : in String) ;

   -- Start a  transaction before prepare/set/get/execute/open_cursor/close_cursor
   -- prepare is done once per statement, even if prepare is called many times

   -- typical usage for reading data:

   --   Select_Stm : Sql.Statement_Type;
   --   T : Sql.Transaction_Type:
   --   Column : String(1.15) := (others => ' ');
   -- begin
   --   Sql.Connect (Host => "localhost", Port=> 5432,Db_Name=> "foo", Login=> "bar", Password => "");
   --   T.Start;
   --   Select_Stm.Prepare("select * from TABLE where STATUS <> :STATUS");
   --   Select_Stm.Set("STATUS","CLOSED");
   --   Select_Stm.Open_Cursor;
   --   loop
   --     Select_Stm.Fetch(End_Of_Set);
   --     exit when End_Of_Set;
   --     Select_Stm.Get("COLUMN", Column);
   --     Text_Io.Put_Line ("3 : Got COLUMN: '" &  Column & "' from db");
   --     Text_Io.New_Line;
   --   end loop;
   --   Select_Stm.Close_Cursor;
   --   T.Commit;
   --   Sql.Close_Session;
   -- end  :

   -- typical usage for Writing data:

   --   Delete_Stm : Sql.Statement_Type;
   --   T : Sql.Transaction_Type:
   --   Num_Rows_Affected : Natural := 0;
   -- begin
   --   Sql.Connect (Host => "localhost", Port=> 5432,Db_Name=> "foo", Login=> "bar", Password => "");
   --   T.Start;
   --   Delete_Stm.Prepare("delete from TABLE where STATUS <> :STATUS");
   --   Delete_Stm.Set("STATUS","CLOSED");
   --   Delete_Stm.Execute(Num_Rows_Affected);
   --   T.Commit;
   --   Sql.Close_Session;
   -- end  :

   procedure Open_Cursor (Statement : in Statement_Type);

   procedure Fetch (Statement  : in Statement_Type;
                    End_Of_Set : out    Boolean) ;

   procedure Close_Cursor (Statement : in Statement_Type);


   procedure Execute (Statement           : in Statement_Type;
                      No_Of_Affected_Rows : out Natural) ;


   procedure Execute (Statement : in Statement_Type) ;

   -----------------------------------------------------------
   -----------------------------------------------------------
   procedure Get (Statement : in Statement_Type;
                  Parameter : in Positive;
                  Value     : out Integer_4) ;

   procedure Get (Statement : in Statement_Type;
                  Parameter : in String;
                  Value     : out Integer_4);

   procedure Get (Statement : in Statement_Type;
                  Parameter : in Positive;
                  Value     : out Integer_8) ;

   procedure Get (Statement : in Statement_Type;
                  Parameter : in String;
                  Value     : out Integer_8);

   procedure Get (Statement : in Statement_Type;
                  Parameter : in Positive;
                  Value     : out String);

   procedure Get (Statement : in Statement_Type;
                  Parameter : in String;
                  Value     : out String);

   procedure Get (Statement : in Statement_Type;
                  Parameter : in Positive;
                  Value     : out Fixed_Type);

   procedure Get (Statement : in Statement_Type;
                  Parameter : in String;
                  Value     : out Fixed_Type);

   procedure Get (Statement : in Statement_Type;
                  Parameter : in Positive;
                  Value     : out Character);

   procedure Get (Statement : in Statement_Type;
                  Parameter : in String;
                  Value     : out Character);

   procedure Get (Statement : in Statement_Type;
                  Parameter : in Positive;
                  Value     : out Boolean);

   procedure Get (Statement : in Statement_Type;
                  Parameter : in String;
                  Value     : out Boolean);

   procedure Get_Date (Statement : in Statement_Type;
                       Parameter : in String;
                       Value     : out Calendar2.Time_Type) ;

   procedure Get_Time (Statement : in Statement_Type;
                       Parameter : in Positive;
                       Value     : out Calendar2.Time_Type) ;


   procedure Get_Date (Statement : in Statement_Type;
                       Parameter : in Positive;
                       Value     : out Calendar2.Time_Type) ;

   procedure Get_Time (Statement : in Statement_Type;
                       Parameter : in String;
                       Value     : out Calendar2.Time_Type) ;

   procedure Get_Timestamp (Statement : in Statement_Type;
                            Parameter : in Positive;
                            Value     : out Calendar2.Time_Type) ;

   procedure Get_Timestamp (Statement : in Statement_Type;
                            Parameter : in String;
                            Value     : out Calendar2.Time_Type) ;

   procedure Get(Statement : in Statement_Type;
                 Parameter : in Positive;
                 Value     : out Calendar2.Time_Type) ;

   procedure Get(Statement : in Statement_Type;
                 Parameter : in String;
                 Value     : out Calendar2.Time_Type) ;

   procedure Get (Statement : in Statement_Type;

                  Parameter : in Positive;
                  Value     : out Ada.Calendar.Time);

   procedure Get (Statement : in Statement_Type;
                  Parameter : in String;
                  Value     : out Ada.Calendar.Time) ;

   -----------------------------------------------------------

   procedure Set (Statement : in out Statement_Type;
                  Parameter : in String;
                  Value     : in Integer_4);

   procedure Set (Statement : in out Statement_Type;
                  Parameter : in String;
                  Value     : in Integer_8);

   procedure Set (Statement : in out Statement_Type;
                  Parameter : in String;
                  Value     : in String);

   procedure Set (Statement : in out Statement_Type;
                  Parameter : in String;
                  Value     : in Character);

   procedure Set (Statement : in out Statement_Type;
                  Parameter : in String;
                  Value     : in Fixed_Type);

   procedure Set (Statement : in out Statement_Type;
                  Parameter : in String;
                  Value     : in Boolean);

   procedure Set_Date (Statement : in out Statement_Type;
                       Parameter : in String;
                       Value     : in Calendar2.Time_Type);

   procedure Set_Time (Statement : in out Statement_Type;
                       Parameter : in String;
                       Value     : in Calendar2.Time_Type);

   procedure Set_Timestamp (Statement : in out Statement_Type;
                            Parameter : in String;
                            Value     : in Calendar2.Time_Type);

   procedure Set (Statement : in out Statement_Type;
                  Parameter : in String;
                  Value     : in Calendar2.Time_Type);

   procedure Set (Statement : in out Statement_Type;
                  Parameter : in String;
                  Value     : in  Ada.Calendar.Time);
   -----------------------------------------------------------
---
   function Is_Null (Statement : Statement_Type;
                     Parameter : Positive) return Boolean ;

   function Is_Null (Statement : Statement_Type;
                     Parameter : String) return Boolean ;

   ------------------------------------------------------------

   procedure Set_Null (Statement : in out Statement_Type;
                       Parameter : String);


   procedure Set_Null_Date (Statement : in out Statement_Type;
                            Parameter : String);

   function Get_Prepared_Statement(Statement : Statement_Type) return String;

   ------------------------------------------------------------
   --  procedure Print_Errors(MyUnit   : in String;
   --                         MyStatus : in Exec_Status_Type);


   ------------------------------------------------------------

   procedure Get_Column_Info
     (Statement   : Statement_Type;
      Parameter   : Positive;
      Name        : out String;
      Namelen     : out Integer_4;
      Datatype    : out Integer_4;
      Datatypelen : out Integer_4);

   function Get_Nbr_Columns (Statement : Statement_Type) return Integer;

   ---------------------------------------------------------------

private
   type Transaction_Identity_Type is mod Integer'Last;

   type Parameter_Type_Type is (
                                Not_Set     ,
                                Null_Type   ,
                                An_Integer  ,
                                A_Float     ,
                                A_String    ,
                                A_Character ,
                                A_Date      ,
                                A_Time      ,
                                A_Timestamp );

   for Parameter_Type_Type'Size use Integer'Size;
   for Parameter_Type_Type use (
                                Not_Set     => -2,
                                Null_Type   => -1,
                                An_Integer  => 0,
                                A_Float     => 1,
                                A_String    => 2,
                                A_Character => 3,
                                A_Date      => 4,
                                A_Time      => 5,
                                A_Timestamp => 6);

   type Parameter_Map_Type is record
      Index          : Integer := 0;
      Name           : Unbounded_String;
      Value          : Unbounded_String;
      Parameter_Type : Parameter_Type_Type;
   end record;

   package Map is new Ada.Containers.Doubly_Linked_Lists (Parameter_Map_Type);

   type Statement_Type_Type is (A_Select, An_Insert, A_Delete, An_Update, A_Ddl);
   subtype Name_Type is String (1 .. 11);

   type Private_Statement_Type is new Limited_Controlled with record
      Index                   : Integer := 0;
      Statement_Name          : Name_Type := (others => ' ');
      Is_Prepared             : Boolean := False;
      Cursor_Name             : Name_Type := (others => ' ');
      Pg_Prepared_Statement   : Unbounded_String := To_Unbounded_String ("");
      Prepared_Statement      : Unbounded_String := To_Unbounded_String ("");
      Original_Statement      : Unbounded_String := To_Unbounded_String ("");
--      Parameter_Map           : Map.List_Type := Map.Create;
      Parameter_Map           : Map.List;
      Result                  : Result_Type;
      Dml_Result              : Result_Type;
      Is_Open                 : Boolean := False; --set/unset in open/close check in fetch
      Type_Of_Statement       : Statement_Type_Type;
      Current_Row             : Natural := 0;
      Number_To_Fetch         : Natural := 10_000;
      Number_Actually_Fetched : Natural := 0;
      Number_Parameters       : Natural := 0;
   end record;

   --  procedure Initialize (Object : in out Private_Statement_Type) ;
   procedure Do_Initialize (Private_Statement : in out Private_Statement_Type) ;
   overriding procedure Finalize (Private_Statement : in out Private_Statement_Type) ;
   procedure Associate (Private_Statement : in out Private_Statement_Type;
                        Bind_Varible      : String;
                        Idx               : Natural) ;
   procedure Check_Is_Prepared (Private_Statement : in out Private_Statement_Type) ;
   procedure Update_Map (Private_Statement      : in out Private_Statement_Type;
                         Bind_Varible           : in     String;
                         Value                  : in     String;
                         Parameter_Type         : in     Parameter_Type_Type) ;
   procedure Exchange_Binder_Variables (Private_Statement : in out Private_Statement_Type) ;
   procedure Fill_Data_In_Prepared_Statement (Private_Statement : in out Private_Statement_Type) ;

   type Private_Statement_Type_Ptr is access all Private_Statement_Type;

   type Statement_Type is new Limited_Controlled with record
      Private_Statement : Private_Statement_Type_Ptr := null;
   end record;
   --  procedure Initialize (Statement : in out Statement_Type);
   procedure Do_Initialize (Statement : in out Statement_Type);
   overriding procedure Finalize (Statement : in out Statement_Type);

   type Transaction_Type is new Limited_Controlled with record
      Status  : Transaction_Status_Type   := None;
      Counter : Transaction_Identity_Type := 0;
   end record;
--   procedure Finalize (T : in out Transaction_Type);

end Sql;




