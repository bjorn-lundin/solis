------------------------------------------------------------------------------
--                                                                          --
--                       P G A D A . D A T A B A S E                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--  Copyright (c) Samuel Tardieu 2000                                       --
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
--  3. Neither the name of Samuel Tardieu nor the names of its contributors --
--     may be used to endorse or promote products derived from this         --
--     software without specific prior written permission.                  --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY SAMUEL TARDIEU AND CONTRIBUTORS ``AS       --
--  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       --
--  FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL SAMUEL      --
--  TARDIEU OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,             --
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES                --
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR      --
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)      --
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN               --
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR            --
--  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,          --
--  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Finalization;
with Pgada.Thin; use Pgada.Thin;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Pgada.Database is

   --   pragma Preelaborate;

   Pg_Error : exception;
   No_Such_Column : exception; --bnl


   type Tuple_Index_Type is new Positive;  -- bnl
   type Field_Index_Type is new Natural;  -- bnl
   type Encoding_Type is (Utf_8, Latin_1); -- bnl

   type Connection_Type is
     new Ada.Finalization.Limited_Controlled with private;

   procedure Set_Db_Login (Connection : in out Connection_Type;
                           Host       : in String  := "";
                           Port       : in Natural := 0;
                           Options    : in String  := "";
                           Tty        : in String  := "";
                           Db_Name    : in String  := "";
                           Login      : in String := "";
                           Password   : in String := "");
   --  Connect to a database

   function Db (Connection : Connection_Type) return String;
   function Host (Connection : Connection_Type) return String;
   function Port (Connection : Connection_Type) return Positive;
   function Options (Connection : Connection_Type) return String;
   function Tty (Connection : Connection_Type) return String;
   procedure Set_Host (Connection : in out Connection_Type; Host : String);
   procedure Set_Port (Connection : in out Connection_Type; Port : Natural);
   procedure Set_Options (Connection : in out Connection_Type; Options : String);
   procedure Set_Tty (Connection : in out Connection_Type; Tty : String);
   procedure Set_Db_Name (Connection : in out Connection_Type; Db_Name : String);
   procedure Set_User (Connection : in out Connection_Type; User : String);
   procedure Set_Password (Connection : in out Connection_Type; Password : String);
   procedure Login(Connection : in out Connection_Type) ;
   procedure Login(Connection : in out Connection_Type; Conn_Info : String);


   --  Query characteristics of an open connection

   type Connection_Status_Type is (Connection_Ok, Connection_Bad);

   function Status (Connection : Connection_Type)
                    return Connection_Status_Type;

   function Error_Message (Connection : Connection_Type) return String;

   procedure Finish (Connection : in out Connection_Type);

   procedure Reset (Connection : in Connection_Type);

   type Result_Type is
     new Ada.Finalization.Controlled with private;

   procedure Exec (Connection : in Connection_Type'Class;
                   Query      : in String;
                   Result     : out Result_Type);
   --  Note: the Connection parameter is of type Connection_Type'Class
   --  because this function cannot be a primitive operation of several
   --  tagged types.

   function Exec (Connection : Connection_Type'Class; Query : String)
                  return Result_Type;
   --  Function form of the subprogram

   --   procedure Exec (Connection : in Connection_Type'Class;
   --                   Query      : in String);
   --   --  This procedure executes the query but does not test the result. It
   --   --  can be used for queries that do not require a result and cannot fail.

   type Exec_Status_Type is (Empty_Query,
                             Command_Ok,
                             Tuples_Ok,
                             Copy_Out,
                             Copy_In,
                             Bad_Response,
                             Non_Fatal_Error,
                             Fatal_Error);

   function Result_Status (Result : Result_Type) return Exec_Status_Type;
   --bnl
   function Result_Error_Message (Result : Result_Type) return String;

   function Nbr_Tuples (Result : Result_Type) return Natural;

   function Nbr_Fields (Result : Result_Type) return Natural;

   function Field_Name (Result      : Result_Type;
                        Field_Index : Field_Index_Type)
                        return String;
   function Field_Index (Result      : Result_Type;
                         Field_Name  : String)
                        return Field_Index_Type ;

   function Get_Value (Result      : Result_Type;
                       Tuple_Index : Tuple_Index_Type;
                       Field_Index : Field_Index_Type)
                       return String;

   --   function Get_Value (Result      : Result_Type;
   --                       Tuple_Index : Tuple_Index_Type;
   --                       Field_Name  : String)
   --     return String;

   --   function Get_Value (Result      : Result_Type;
   --                       Tuple_Index : Tuple_Index_Type;
   --                       Field_Index : Field_Index_Type)
   --     return Integer;

   --   function Get_Value (Result      : Result_Type;
   --                       Tuple_Index : Tuple_Index_Type;
   --                       Field_Name  : String)
   --     return Integer;

   function Get_Length (Result      : Result_Type;
                        Tuple_Index : Tuple_Index_Type;
                        Field_Index : Field_Index_Type)
                        return Natural;

   function Is_Null (Result      : Result_Type;
                     Tuple_Index : Tuple_Index_Type;
                     Field_Index : Field_Index_Type)
                     return Boolean;

   function Command_Status (Result : Result_Type) return String;

   -- bnl
   function Rows_Affected (Result : Result_Type) return Natural;
   --   function Get_Field_Number(Result      : Result_Type;
   --                             Field_Name  : String) return Natural;


   procedure Set_Client_Encoding (Connection : in out Connection_Type;
                                  Encoding   : in String);

   function Client_Encoding_Code (Connection : Connection_Type) return Natural;

   function Client_Encoding_Name (Connection : Connection_Type) return String;

   --  procedure Exec_Prepared(Connection    : in  Connection_Type'Class;
   --                          Statment_Name : in  String;
   --                          Number_Params : in  Natural;
   --                          Param_Values  : in  String_Array_Type;
   --                          Param_Lengths : in  Int_Array_Type;
   --                          Param_Formats : in  Int_Array_Type;
   --                          Result        : out Result_Type) ;
   --
   --  procedure Prepare(Connection    : in  Connection_Type'Class;
   --                    Statment_Name : in  String;
   --                    Query         : in  String;
   --                    Number_Params : in  Natural;
   --                    Param_Types   : in Int_Array_Type;
   --                    Result        : out Result_Type);

   -- bnl

   function Oid_Status (Result : Result_Type) return String;

   procedure Clear (Result : in out Result_Type);
   ----------------------------------------------------------------------------
   function Parameter_Status (Conn : Connection_Type; Parameter : String) return String ;
   function Database_Encoding (Conn : Connection_Type) return String ;

   function  Get_Encoding (Conn : Connection_Type) return Encoding_Type ;
   procedure Set_Encoding (Conn :  in out Connection_Type; Encoding : Encoding_Type)  ;
   procedure Set_Encoding (Res  : in out  Result_Type;     Encoding : Encoding_Type)  ;
   procedure Set_Connected (Connection : in out Connection_Type; Connected : in Boolean);
   function Get_Connected (Connection : Connection_Type) return Boolean;

   function Error_Severity (Res : Result_Type) return String;
   function Error_Sql_State (Res : Result_Type) return String;
   function Error_Message_Primary (Res : Result_Type) return String;
   function Error_Message_Detail (Res : Result_Type) return String;
   function Error_Message_Hint (Res : Result_Type) return String;
   function Error_Statement_Position (Res : Result_Type) return String;
   function Error_Internal_Position (Res : Result_Type) return String;
   function Error_Internal_Query (Res : Result_Type) return String;
   function Error_Context (Res : Result_Type) return String;
   function Error_Source_File (Res : Result_Type) return String;
   function Error_Source_Line (Res : Result_Type) return String;
   function Error_Source_Function (Res : Result_Type) return String;

   function Escape (Conn   : in Connection_Type;
                    Source : in String) return String;

private

   type Connection_Type is new Ada.Finalization.Limited_Controlled with record
      Actual       : Thin.Pg_Conn_Access;
      Encoding     : Encoding_Type := Utf_8; --bnl
      Is_Connected : Boolean := False;       --bnl
      Host         : Unbounded_String  := Null_Unbounded_String;
      Port         : Natural := 5432;
      Options      : Unbounded_String  := Null_Unbounded_String;
      Tty          : Unbounded_String  := Null_Unbounded_String;
      Db_Name      : Unbounded_String  := Null_Unbounded_String;
      User         : Unbounded_String  := Null_Unbounded_String;
      Password     : Unbounded_String  := Null_Unbounded_String;
   end record;

   overriding procedure Finalize (Connection : in out Connection_Type);

   type Natural_Access is access Natural;

   type Result_Type is new Ada.Finalization.Controlled with record
      Actual    : Thin.Pg_Result_Access;
--      Ref_Count : Natural_Access := new Integer'(1);
      Encoding  : Encoding_Type := Utf_8;    --bnl
   end record;
  -- procedure Adjust (Result : in out Result_Type);
   overriding procedure Finalize (Result : in out Result_Type);

end Pgada.Database;

