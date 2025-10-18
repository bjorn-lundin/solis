-------------------------------------------------------------------------------

with Calendar;
with Types; use Types;
with Unchecked_Conversion;

package Calendar2 is

  subtype Year_Type is Integer_2 range 1901 .. 2099;
  subtype Month_Type is Integer_2 range 01 .. 12;
  subtype Day_Type is Integer_2 range 01 .. 31;
  subtype Hour_Type is Integer_2 range 00 .. 23;
  subtype Minute_Type is Integer_2 range 00 .. 59;
  subtype Second_Type is Integer_2 range 00 .. 59;
  subtype Millisecond_Type is Integer_2 range 000 .. 999;

  subtype Week_Type is Integer_2 range 1 .. 53;
  subtype Year_Day_Type is Integer_2 range 1 .. 366;
  type Week_Day_Type is (
                         Monday,
                         Tuesday,
                         Wednesday,
                         Thursday,
                         Friday,
                         Saturday,
                         Sunday);

  subtype Interval_Day_Type is Integer_4 range 0 .. Integer_4'Last;
  subtype Seconds_Type is Integer_4 range 0 .. Integer_4'Last;



  type Short_Month_Type is (Jan, Feb, Mar, Apr, May, Jun,
                            Jul, Aug, Sep, Oct, Nov, Dec);
  for Short_Month_Type use (Jan => 1, Feb => 2, Mar => 3, Apr =>  4, May =>  5, Jun =>  6,
                            Jul => 7, Aug => 8, Sep => 9, Oct => 10, Nov => 11, Dec => 12);
  for Short_Month_Type'Size use Integer_4'Size;
  function Short_Month is new Unchecked_Conversion(Short_Month_Type, Integer_4);
  function Short_Month is new Unchecked_Conversion(Integer_4, Short_Month_Type);


  type Long_Month_Type is (January, February, March, April, May, June,
                           July, August, September, October, November, December);
  for Long_Month_Type use (January   => 1, February => 2, March     => 3,  April    => 4,
                           May       => 5, June     => 6, July      => 7,  August   => 8,
                           September => 9, October  => 10, November => 11, December => 12);
  for Long_Month_Type'Size use Integer_4'Size;
  function Long_Month is new Unchecked_Conversion(Short_Month_Type, Integer_4);
  function Long_Month is new Unchecked_Conversion(Integer_4, Short_Month_Type);



  ------------------------ Interval type start --------------------------------
  type Interval_Type is record
    Days         : Interval_Day_Type;
    Hours        : Hour_Type;
    Minutes      : Minute_Type;
    Seconds      : Second_Type;
    Milliseconds : Millisecond_Type;
  end record;



  function "<" (Left, Right : in Interval_Type) return Boolean;
  function "<=" (Left, Right : in Interval_Type) return Boolean;
  function ">" (Left, Right : in Interval_Type) return Boolean;
  function ">=" (Left, Right : in Interval_Type) return Boolean;

  function To_Interval
    (Day_Duration : in Calendar.Day_Duration)
      return         Interval_Type;

  function To_Interval (Seconds : in Seconds_Type) return Interval_Type;
  function To_Seconds (Interval : in Interval_Type) return Seconds_Type;

  function To_Day_Duration
    (Interval : in Interval_Type)
      return     Calendar.Day_Duration;
  -- This function does not use the term DAYS in the record INTERVAL_TYPE.

  function "+" (Left, Right : in Interval_Type) return Interval_Type;

  function "-" (Left, Right : in Interval_Type) return Interval_Type;


  function String_Interval
    (Interval     : in Interval_Type;
     Days         : in Boolean := True;
     Hours        : in Boolean := True;
     Minutes      : in Boolean := True;
     Seconds      : in Boolean := True;
     Milliseconds : in Boolean := True)
      return         String;

  -- Enter in the BOOLEAN parameters the values you wish to be returned.
  -- Example of returned a interval is : 00001:15:30:05.003

  ---------------------------Interval_Type  Stop ----------------------------------------------------------

  type Clock_Type is record
    Hour   : Hour_Type;
    Minute : Minute_Type;
    Second : Second_Type;
  end record;


  ------------------------ Time_Type start --------------------------------

  type Time_Type is tagged record
    Year        : Year_Type;
    Month       : Month_Type;
    Day         : Day_Type;
    Hour        : Hour_Type;
    Minute      : Minute_Type;
    Second      : Second_Type;
    Millisecond : Millisecond_Type;
  end record;


  function Is_Legal (Time : in Time_Type) return Boolean;

  function Clock return Calendar.Time;
  function Clock return Time_Type;

  -- Date_And_Time_Str => "YYYY-MM-DD HH:MM:SS.ZZZ"
  function To_Time_Type
    (Date_And_Time_Str : String)
      return     Time_Type;


  -- Date_Str => "DD-MON-YYYY" ("09-DEC-2008),
  -- Time_Str => "HH:MM:SS.ZZZ"("10:01:32.123")
  -- .ZZZ not mandatory
  -- or
  -- Date_Str => "YYYY-MM-DD" ("2104-05-23),
  -- Time_Str => "HH:MM:SS.ZZZ"("10:01:32.123")
  -- .ZZZ not mandatory


  function To_Time_Type
    (Date_Str : String;
     Time_Str : String)
      return     Time_Type;

  Invalid_Date_Format : exception;
  -- is raised by bad data in To_Time_Type

  function To_Time (Date : in Calendar.Time) return Time_Type;
  function To_Calendar_Time (Date : in Time_Type) return Calendar.Time;
  function To_Time
    (Year        : in Year_Type;
     Year_Day    : in Year_Day_Type;
     Hour        : in Hour_Type;
     Minute      : in Minute_Type;
     Second      : in Second_Type;
     Millisecond : in Millisecond_Type)
      return        Time_Type;

  function To_Time
    (Year        : in Year_Type;
     Week        : in Week_Type;
     Day         : in Week_Day_Type;
     Hour        : in Hour_Type        := Hour_Type'First;
     Minute      : in Minute_Type      := Minute_Type'First;
     Second      : in Second_Type      := Second_Type'First;
     Millisecond : in Millisecond_Type := Millisecond_Type'First)
      return        Time_Type;

  function "<" (Left, Right : in Time_Type) return Boolean;
  function "<=" (Left, Right : in Time_Type) return Boolean;
  function ">" (Left, Right : in Time_Type) return Boolean;
  function ">=" (Left, Right : in Time_Type) return Boolean;


  function "+"
    (Left  : in Time_Type;
     Right : in Interval_Type)
      return  Time_Type;
  function "+"
    (Left  : in Interval_Type;
     Right : in Time_Type)
      return  Time_Type;

  function "+"
    (Left  : in Time_Type;
     Right : in Year_Day_Type)
      return  Time_Type;
  function "+"
    (Left  : in Year_Day_Type;
     Right : in Time_Type)
      return  Time_Type;

  function "-" (Left, Right : in Time_Type) return Interval_Type;
  function "-"
    (Left  : in Time_Type;
     Right : in Interval_Type)
      return  Time_Type;
  function "-"
    (Left  : in Time_Type;
     Right : in Year_Day_Type)
      return  Time_Type;

  function Year_Day_Of (Date : in Time_Type) return Year_Day_Type;
  function Week_Day_Of (Date : in Time_Type) return Week_Day_Type;
  function Week_Of (Date : in Time_Type) return Week_Type;
  function String_Date_Iso (Date : in Time_Type) return String ;
  function String_Date_Time_Iso (Date : in Time_Type; T : String := "T"; Tz : String := "Z"; Milliseconds : Boolean := True) return String ;

  function To_String(D : Time_Type; Milliseconds : Boolean := True) return String;


  function String_Date (Date : in Time_Type) return String;
  function String_Time
    (Date         : in Time_Type;
     Hours        : in Boolean := True;
     Minutes      : in Boolean := True;
     Seconds      : in Boolean := True;
     Milliseconds : in Boolean := False)
      return         String;
  function String_Date_And_Time
    (Date         : in Time_Type;
     Hours        : in Boolean := True;
     Minutes      : in Boolean := True;
     Seconds      : in Boolean := True;
     Milliseconds : in Boolean := False)
      return         String;


  function Clock_Of (T : in Time_Type) return Clock_Type;

  ------------------------ Time_Type start --------------------------------


  function "<=" (Left, Right : in Clock_Type) return Boolean;
  function ">=" (Left, Right : in Clock_Type) return Boolean;
  function To_String (C : in Clock_Type) return String;

  function Time_Type_First return Time_Type;
  function Time_Type_Last  return Time_Type;

  function Time_Type_Last  return Calendar.Time ;
  function Time_Type_First return Calendar.Time ;



  Interval_Type_First : constant Interval_Type :=
                          (Days         => Interval_Day_Type'First,
                           Hours        => Hour_Type'First,
                           Minutes      => Minute_Type'First,
                           Seconds      => Second_Type'First,
                           Milliseconds => Millisecond_Type'First);

  Interval_Type_Last : constant Interval_Type :=
                         (Days         => Interval_Day_Type'Last,
                          Hours        => Hour_Type'Last,
                          Minutes      => Minute_Type'Last,
                          Seconds      => Second_Type'Last,
                          Milliseconds => Millisecond_Type'Last);

  In_Parameter_Incorrect : exception;
  --                        raised when the input date is impossible
  --                        (e.g. 2014.02.30)
  Time_Error             : exception;
  --                        raised when the result comes to a date before
  --                        TIME_TYPE_FIRST or after TIME_TYPE_LAST, or the left
  --              parameter is less than the right one in subtractions.

  function Is_Legal
    (Year  : in Year_Type;
     Month : in Month_Type;
     Day   : in Day_Type)
      return  Boolean;

  function Is_Legal
    (Year     : in Year_Type;
     Year_Day : in Year_Day_Type)
      return     Boolean;

  function Calendar_Clock return  Calendar.Time;
  function Clock return Calendar.Time renames Calendar_Clock;

  function Is_Leap_Year (Year : in Year_Type) return Boolean;

  function Year_Day_Of
    (Year  : in Year_Type;
     Month : in Month_Type;
     Day   : in Day_Type)
      return  Year_Day_Type;

  function Days_In (Year : in Year_Type) return Year_Day_Type;
  function Days_In
    (Year  : in Year_Type;
     Month : in Month_Type)
      return  Day_Type;


  function Week_Day_Of
    (Year  : in Year_Type;
     Month : in Month_Type;
     Day   : in Day_Type)
      return  Week_Day_Type;

  function Week_Of
    (Year  : in Year_Type;
     Month : in Month_Type;
     Day   : in Day_Type)
      return  Week_Type;
  function Week_Of
    (Year     : in Year_Type;
     Year_Day : in Year_Day_Type)
      return     Week_Type;

  function String_Date
    (Date : in Calendar.Time := To_Calendar_Time (Clock))
      return String;


  -- Enter in the BOOLEAN parameters in the STRING_TIME functions the values
  -- you wish to be returned.
  -- Example of returned date is : 01-Jan-1984
  -- Example of returned time is : 15:30:05.001


  function String_Time
    (Date         : in Calendar.Time := To_Calendar_Time (Clock);
     Hours        : in Boolean       := True;
     Minutes      : in Boolean       := True;
     Seconds      : in Boolean       := True;
     Milliseconds : in Boolean       := False)
      return         String;
  function String_Date_And_Time
    (Date         : in Calendar.Time := To_Calendar_Time (Clock);
     Hours        : in Boolean       := True;
     Minutes      : in Boolean       := True;
     Seconds      : in Boolean       := True;
     Milliseconds : in Boolean       := False)
      return         String;


end Calendar2;
