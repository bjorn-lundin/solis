--------------------------------------------------------------------------------
--
--	COPYRIGHT	Consafe Logistics AB, Lund
--
--	RESPONSIBLE	Björn Lundin
--
--	DESCRIPTION	Rewrite of package Settings. Uses String instead
--              of String and on unix, it works even if inifile is in dos format
--
--------------------------------------------------------------------------------
-- 6.7      21-AUG-1996  Henrik Dannberg
--                       Original version (Settings)
-- 9.4-6643 03-Sep-2004  SNE
--                       New procedure LOAD with filename as input.
--------------------------------------------------------------------------------
-- 9.8-17902    30-Oct-2009  New name+use String+work on unix when dos-style inifile
--                       introduced exception No_Ini_File_Found
--------------------------------------------------------------------------------

package Ini is


  -- This portable Ada package handles MS Windows style configuration files
  -- (ini-files). A Windows configuration file consists of one or several
  -- sections (identified by brackets '[' and ']') and variable/value pairs
  -- each belonging to a specific section. Example :

  --	[windows]
  --	spooler=yes
  --	load=
  --	NullPort=None
  --	BorderWidth=3
  --
  --	[intl]
  --	sLanguage=sve
  --	sCountry=Sverige
  --	iCountry=46


  -- First step is to read configuration data from a text file into an
  -- internal data structure in memory. This is done by the Load procedure.

  -- Specific variable values can then be retreived using the various
  -- Get_Value function or set using the Set_Value procedures.

  -- You may also, at any time, save the current configuration into a specified
  -- text file using the Save procedure.

  -- Section and variable names are not case sensitive but will be saved using
  -- the original case.

  -- Comment lines starts with and ';'.



  -- Load configuration data from a text file. The name of the file is
  -- assumed to be <executable>.ini where <executable> is the full path
  -- for the current executable image.

  -- 9.8-17902
  -- If not found, we look in $SATTMATE_CONFIG/processes for the
  -- <executable.ini> if not found there, we raise No_Ini_File_Found
  No_Ini_File_Found : exception;
  -- 9.8-17902


--  procedure Load;

  -- Load configuration data from a specified text file.

  -- v9.4-6643
  procedure Load (File_Name : in String);

  function Is_Loaded return Boolean;

  -- Write current configuration to the last loaded text file. All comments
  -- will be lost !

  procedure Save;


  -- Get current number of sections

  function Get_Section_Count return Natural;


  -- Get the name of a section

  function Get_Section_Name (No: Positive) return String;


  -- Get current number of variables within a specified section.
  -- Zero will be returned if the specified section does not exist.

  function Get_Variable_Count (Section: String) return Natural;


  -- Get the name of a variable. An empty string will be returned if
  -- the variable does not exist within the specified section.

  function Get_Variable_Name (Section: String; No: Positive) return String;


  -- Get variable values. The default value will be returned if the
  -- specified variable does not exist within the specified section

  function Get_Value (Section : String;
                      Variable: String;
                      Default : String) return String;

  function Get_Value (Section : String;
                      Variable: String;
                      Default : Boolean) return Boolean;

  function Get_Value (Section : String;
                      Variable: String;
                      Default : Integer) return Integer;

  generic
    type Enumeration_Type is (<>);
  function Get_Enumeration_Value
             (Section : String;
              Variable: String;
              Default : Enumeration_Type) return Enumeration_Type;


  -- Set variable values. The variable will be created if it does not yet
  -- exists.

  procedure Set_Value (Section: String; Variable: String; Value: String);

  procedure Set_Value (Section: String; Variable: String; Value: Boolean);

  procedure Set_Value (Section: String; Variable: String; Value: Integer);

  generic
    type Enumeration_Type is (<>);
  procedure Set_Enumeration_Value (Section : String;
                                   Variable: String;
                                   Value   : Enumeration_Type);
end Ini;
