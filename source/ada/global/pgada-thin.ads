------------------------------------------------------------------------------
--                                                                          --
--                           P G A D A . T H I N                            --
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

with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;

package Pgada.Thin is

   --   pragma Preelaborate;

   type Conn_Status_Type is (Connection_Ok, Connection_Bad);
   for Conn_Status_Type'Size use Int'Size;
   pragma Convention (C, Conn_Status_Type);

   type Exec_Status_Type is (Pgres_Empty_Query,
                             Pgres_Command_Ok,
                             Pgres_Tuples_Ok,
                             Pgres_Copy_Out,
                             Pgres_Copy_In,
                             Pgres_Bad_Response,
                             Pgres_Nonfatal_Error,
                             Pgres_Fatal_Error);
   for Exec_Status_Type'Size use Int'Size;
   pragma Convention (C, Exec_Status_Type);

   type Pg_Conn is null record;
   type Pg_Conn_Access is access Pg_Conn;
   pragma Convention (C, Pg_Conn_Access);

   
   
   
   type Pg_Result is null record;
   type Pg_Result_Access is access Pg_Result;
   pragma Convention (C, Pg_Result_Access);

   type Oid is new Unsigned;

   function Pq_Set_Db_Login (Pg_Host    : Chars_Ptr;
                             Pg_Port    : Chars_Ptr;
                             Pg_Options : Chars_Ptr;
                             Pg_Tty     : Chars_Ptr;
                             Db_Name    : Chars_Ptr;
                             Login      : Chars_Ptr;
                             Password   : Chars_Ptr)
                             return Pg_Conn_Access;
   pragma Import (C, Pq_Set_Db_Login, "PQsetdbLogin");

   
   function PQ_Connectdb (Conn_Info : Chars_Ptr) return Pg_Conn_Access;
   pragma Import (C, PQ_Connectdb, "PQconnectdb");
   
   function Pq_Db (Conn : Pg_Conn_Access) return Chars_Ptr;
   pragma Import (C, Pq_Db, "PQdb");

   function Pq_Host (Conn : Pg_Conn_Access) return Chars_Ptr;
   pragma Import (C, Pq_Host, "PQhost");

   function Pq_Port (Conn : Pg_Conn_Access) return Chars_Ptr;
   pragma Import (C, Pq_Port, "PQport");

   function Pq_Options (Conn : Pg_Conn_Access) return Chars_Ptr;
   pragma Import (C, Pq_Options, "PQoptions");

   function Pq_Tty (Conn : Pg_Conn_Access) return Chars_Ptr;
   pragma Import (C, Pq_Tty, "PQtty");

   function Pq_Status (Conn : Pg_Conn_Access) return Conn_Status_Type;
   pragma Import (C, Pq_Status, "PQstatus");

   function Pq_Error_Message (Conn : Pg_Conn_Access) return Chars_Ptr;
   pragma Import (C, Pq_Error_Message, "PQerrorMessage");

   function Pq_Result_Error_Message (Res : Pg_Result_Access) return Chars_Ptr;
   pragma Import (C, Pq_Result_Error_Message, "PQresultErrorMessage");

   procedure Pq_Finish (Conn : in Pg_Conn_Access);
   pragma Import (C, Pq_Finish, "PQfinish");

   procedure Pq_Reset (Conn : in Pg_Conn_Access);
   pragma Import (C, Pq_Reset, "PQreset");

   function Pq_Exec (Conn  : Pg_Conn_Access;
                     Query : Chars_Ptr)
                     return Pg_Result_Access;
   pragma Import (C, Pq_Exec, "PQexec");

   function Pq_Result_Status (Res : Pg_Result_Access) return Exec_Status_Type;
   pragma Import (C, Pq_Result_Status, "PQresultStatus");

   function Pq_N_Tuples (Res : Pg_Result_Access) return Int;
   pragma Import (C, Pq_N_Tuples, "PQntuples");

   function Pq_N_Fields (Res : Pg_Result_Access) return Int;
   pragma Import (C, Pq_N_Fields, "PQnfields");

   function Pq_F_Name (Res         : Pg_Result_Access;
                       Field_Index : Int)
                       return Chars_Ptr;
   pragma Import (C, Pq_F_Name, "PQfname");

   function Pq_F_Number (Res         : Pg_Result_Access;
                         Field_Index : Chars_Ptr)
                         return Int;
   pragma Import (C, Pq_F_Number, "PQfnumber");

   function Pq_F_Type (Res         : Pg_Result_Access;
                       Field_Index : Int)
                       return Oid;
   pragma Import (C, Pq_F_Type, "PQftyp");

   function Pq_Get_Value (Res       : Pg_Result_Access;
                          Tup_Num   : Int;
                          Field_Num : Int)
                          return Chars_Ptr;
   pragma Import (C, Pq_Get_Value, "PQgetvalue");

   function Pq_Get_Length (Res       : Pg_Result_Access;
                           Tup_Num   : Int;
                           Field_Num : Int)
                           return Int;
   pragma Import (C, Pq_Get_Length, "PQgetlength");

   function Pq_Get_Is_Null (Res       : Pg_Result_Access;
                            Tup_Num   : Int;
                            Field_Num : Int)
                            return Int;
   pragma Import (C, Pq_Get_Is_Null, "PQgetisnull");

   function Pq_Cmd_Status (Res : Pg_Result_Access) return Chars_Ptr;
   pragma Import (C, Pq_Cmd_Status, "PQcmdStatus");

   --bnl
   function Pq_Cmd_Tuples (Res : Pg_Result_Access) return Chars_Ptr;
   pragma Import (C, Pq_Cmd_Tuples, "PQcmdTuples");
   --bnl

   function Pq_Oid_Status (Res : Pg_Result_Access) return Chars_Ptr;
   pragma Import (C, Pq_Oid_Status, "PQoidStatus");

   procedure Pq_Clear (Res : in Pg_Result_Access);
   pragma Import (C, Pq_Clear, "PQclear");


   --bnl
   function Pq_Set_Client_Encoding (Conn     : Pg_Conn_Access ;
                                    Encoding : Chars_Ptr) return Int;
   pragma Import (C, Pq_Set_Client_Encoding, "PQsetClientEncoding");
   --int PQsetClientEncoding(PGconn *conn, const char *encoding);
   --where conn is a connection to the server, and encoding is the
   --encoding you want to use. If the function successfully sets
   --the encoding, it returns 0, otherwise -1. The current encoding
   --for this connection can be determined by using:

   function Pq_Client_Encoding (Conn : Pg_Conn_Access) return Int;
   pragma Import (C, Pq_Client_Encoding, "PQclientEncoding");
   --int PQclientEncoding(const PGconn *conn);
   --Note that it returns the encoding ID, not a symbolic string
   --such as EUC_JP. To convert an encoding ID to an
   -- encoding name, you can use:

   function Pg_Encoding_To_Char (Encoding_Id : Int) return Chars_Ptr;
   pragma Import (C, Pg_Encoding_To_Char, "pg_encoding_to_char");
   --char *pg_encoding_to_char(int encoding_id);

   --  type String_Array_Type is array (Positive range <>) of Chars_Ptr;
   --  type String_Array_Type_Ptr is access all String_Array_Type;
   --  type String_Array_Type_Ptr_Ptr is access all String_Array_Type_Ptr;
   --
   --  type Int_Array_Type is array (Positive range <>) of int;
   --  pragma Convention (C, Int_Array_Type);
   --  type Int_Array_Type_Ptr is access all Int_Array_Type;
   --  pragma Convention (C, Int_Array_Type_Ptr);
   --   pragma Convention (C_Pass_By_Copy, SAFEARRAY);


   --  function PQ_Exec_Prepared(Conn          : PG_Conn_Access;
   --                            Stmt_Name     : Chars_Ptr;
   --                            N_Params      : Int;
   --                            Param_Values  : String_Array_Type_Ptr_Ptr;
   --                            Param_Lengths : Int_Array_Type_Ptr;
   --                            Param_Formats : Int_Array_Type_Ptr;
   --                            Result_Format : Int) return PG_Result_Access;
   --PGresult *PQexecPrepared(PGconn *conn,
   --                       const char *stmtName,
   --                       int nParams,
   --                       const char * const *paramValues,
   --                       const int *paramLengths,
   --                       const int *paramFormats,
   --                       int resultFormat);
   --Sends a request to execute a prepared statement with given parameters,
   --and waits for the result.
   --PQexecPrepared is like PQexecParams, but the command to be executed
   --is specified by naming a previously-prepared statement, instead of
   --giving a query string. This feature allows commands that will be
   --used repeatedly to be parsed and planned just once, rather than each
   --time they are executed. The statement must have been prepared previously
   -- in the current session.
   --  pragma Import(C, PQ_Exec_Prepared, "PQexecPrepared");


   --Submits a request to create a prepared statement with the given parameters,
   -- and waits for completion.

   --PGresult *PQprepare(PGconn *conn,
   --                    const char *stmtName,
   --                    const char *query,
   --                    int nParams,
   --                    const Oid *paramTypes);

   --  function PQ_Prepare(Conn        : PG_Conn_Access;
   --                      Stmt_Name   : Chars_Ptr;
   --                      Query       : Chars_Ptr;
   --                      N_Params    : Int;
   --                      Param_Types : Int_Array_Type_Ptr) return PG_Result_Access;

   --PQprepare creates a prepared statement for later execution with PQexecPrepared.
   -- This feature allows commands that will be used repeatedly to be parsed and
   -- planned just once, rather than each time they are executed. P
   --The function creates a prepared statement named stmtName from the
   --query string, which must contain a single SQL command. stmtName
   --may be "" to create an unnamed statement, in which case any
   --pre-existing unnamed statement is automatically replaced; otherwise it is
   --an error if the statement name is already defined in the current session.
   --If any parameters are used, they are referred to in the query as $1, $2, etc.
   --nParams is the number of parameters for which types are pre-specified
   --in the array paramTypes[]. (The array pointer may be NULL when
   --nParams is zero.) paramTypes[] specifies, by OID, the data types to
   --be assigned to the parameter symbols. If paramTypes is NULL, or any
   --particular element in the array is zero, the server assigns a data type
   --to the parameter symbol in the same way it would do for an untyped
   --literal string. Also, the query may use parameter symbols with numbers
   --higher than nParams; data types will be inferred for these symbols
   -- as well. (See PQdescribePrepared for a means to find out what data types were inferred.)
   --Also, although there is no libpq function for deleting a prepared statement,
   --the SQL DEALLOCATE statement can be used for that purpose.
   --  pragma Import(C, PQ_Prepare, "PQprepare");



   function Pq_Parameter_Status (Conn     : Pg_Conn_Access ;
                                 Encoding : Chars_Ptr) return Chars_Ptr;
   pragma Import (C, Pq_Parameter_Status, "PQparameterStatus");
   --    const char *PQparameterStatus(const PGconn *conn, const char *paramName);


   --Certain parameter values are reported by the server automatically at
   -- connection startup or whenever their values change. PQparameterStatus
   --can be used to interrogate these settings. It returns the current value of
   --a parameter if known, or NULL if the parameter is not known.

   --Parameters reported as of the current release include server_version,
   --server_encoding, client_encoding, is_superuser, session_authorization,
   --DateStyle, TimeZone, integer_datetimes, and standard_conforming_strings.
   --(server_encoding, TimeZone, and integer_datetimes were not reported by
   --releases before 8.0; standard_conforming_strings was not reported by
   --releases before 8.1.) Note that server_version, server_encoding
   --and integer_datetimes cannot change after startup.
   --bnl


   --         char *PQresultErrorField(const PGresult *res, int fieldcode);
   --#define 	PG_DIAG_SEVERITY   'S'
   --#define 	PG_DIAG_SQLSTATE   'C'
   --#define 	PG_DIAG_MESSAGE_PRIMARY   'M'
   --#define 	PG_DIAG_MESSAGE_DETAIL   'D'
   --#define 	PG_DIAG_MESSAGE_HINT   'H'
   --#define 	PG_DIAG_STATEMENT_POSITION   'P'
   --#define 	PG_DIAG_INTERNAL_POSITION   'p'
   --#define 	PG_DIAG_INTERNAL_QUERY   'q'
   --#define 	PG_DIAG_CONTEXT   'W'
   --#define 	PG_DIAG_SOURCE_FILE   'F'
   --#define 	PG_DIAG_SOURCE_LINE   'L'
   --#define 	PG_DIAG_SOURCE_FUNCTION   'R'

   Pg_Diag_Severity           : constant Int := Character'Pos ('S');
   Pg_Diag_Sqlstate           : constant Int := Character'Pos ('C');
   Pg_Diag_Message_Primary    : constant Int := Character'Pos ('M');
   Pg_Diag_Message_Detail     : constant Int := Character'Pos ('D');
   Pg_Diag_Message_Hint       : constant Int := Character'Pos ('H');
   Pg_Diag_Statement_Position : constant Int := Character'Pos ('P');
   Pg_Diag_Internal_Position  : constant Int := Character'Pos ('p');
   Pg_Diag_Internal_Query     : constant Int := Character'Pos ('q');
   Pg_Diag_Context            : constant Int := Character'Pos ('W');
   Pg_Diag_Source_File        : constant Int := Character'Pos ('F');
   Pg_Diag_Source_Line        : constant Int := Character'Pos ('L');
   Pg_Diag_Source_Function    : constant Int := Character'Pos ('R');

   function Pq_Result_Error_Field (Res : Pg_Result_Access; Field_Code : Int) return Chars_Ptr;
   pragma Import (C, Pq_Result_Error_Field, "PQresultErrorField");



   --     size_t PQescapeStringConn (PGconn *conn,
   --                                char *to, const char *from, size_t length,
   --                                int *error);
   -- PQescapeStringConn writes an escaped version of the from string to the to buffer, escaping special characters so that --they cannot cause any harm, and adding a terminating zero byte. The single quotes that must surround PostgreSQL string --literals are not included in the result string; they should be provided in the SQL command that the result is inserted --into. The parameter from points to the first character of the string that is to be escaped, and the length parameter
   --gives the number of bytes in this string. A terminating zero byte is not required, and should not be counted in length. --(If a terminating zero byte is found before length bytes are processed, PQescapeStringConn stops at the zero; the
   --behavior is thus rather like strncpy.) to shall point to a buffer that is able to hold at least one more byte than
   --twice the value of length, otherwise the behavior is undefined. Behavior is likewise undefined if the to and from
   -- strings overlap.

   --If the error parameter is not NULL, then *error is set to zero on success, nonzero on error. Presently the only
   --possible error conditions involve invalid multibyte encoding in the source string. The output string is still generated --on error, but it can be expected that the server will reject it as malformed. On error, a suitable message is stored in --the conn object, whether or not error is NULL.

   --PQescapeStringConn returns the number of bytes written to to, not including the terminating zero byte.
   function Pq_Escape_String_Conn (Conn   : Pg_Conn_Access;
                                   Target : Chars_Ptr;
                                   Source : Chars_Ptr;
                                   Len    : Size_T;
                                   Error  : access Int) return Size_T;
   pragma Import (C, Pq_Escape_String_Conn, "PQescapeStringConn");


--  char *PQescapeLiteral(PGconn *conn, const char *str, size_t length);

   function Pq_Escape_Literal     (Conn   : Pg_Conn_Access;
                                   Source : Chars_Ptr;
                                   Len    : Size_T) return  Chars_Ptr;
   pragma Import (C, Pq_Escape_Literal, "PQescapeLiteral");

--   -- void PQfreemem(void *ptr);
--   procedure Pq_Freemem (Conn : in Chars_Ptr);
--   pragma Import (C, PQ_freemem, "PQfreemem ");
   
   

   

end Pgada.Thin;

