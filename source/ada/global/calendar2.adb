--------------------------------------------------------------------------------

with Text_Io;

with Ada.Characters.Handling;

package body Calendar2 is

  package Integer_2_Io is new Text_Io.Integer_Io (Integer_2);
  package Integer_4_Io is new Text_Io.Integer_Io (Integer_4);

  -- Constants:

  Seconds_Per_Minute : constant Integer_4 := 60;
  Minutes_Per_Hour   : constant Integer_4 := 60;
  Hours_Per_Day      : constant Integer_4 := 24;

  Seconds_Per_Hour   : constant Integer_4 := Minutes_Per_Hour * Seconds_Per_Minute;
  Seconds_Per_Day    : constant Integer_4 := Hours_Per_Day * Seconds_Per_Hour;

  Month_Day          : constant array (Month_Type)
    of Year_Day_Type := (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);


  function Is_Legal (Year  : in Year_Type;
                     Month : in Month_Type;
                     Day   : in Day_Type) return Boolean is
  begin
    return Day <= Days_In (Year, Month);
  end Is_Legal;


  function Is_Legal (Year     : in Year_Type;
                     Year_Day : in Year_Day_Type) return Boolean is
  begin
    return Year_Day < 366 or Is_Leap_Year (Year);
  end Is_Legal;


  function Is_Legal (Time : in Time_Type) return Boolean is
  begin
    return Is_Legal (Time.Year, Time.Month, Time.Day);
  end Is_Legal;


  procedure To_Month_And_Day (Year     : in Year_Type;
                              Year_Day : in Year_Day_Type;
                              Month    : out Month_Type;
                              Day      : out Day_Type) is
    Days_Left : Year_Day_Type := Year_Day;
  begin
    for Month_Number in Month_Type'First .. (Month_Type'Last) loop
      Month := Month_Number;
      exit when Days_Left <= Days_In (Year, Month_Number);
      Days_Left := Days_Left - Days_In (Year, Month_Number);
    end loop;
    Day := Day_Type (Days_Left);
  end To_Month_And_Day;

  function Clock return Time_Type is
  begin
    return To_Time(Calendar.Clock);
  end Clock;

  function Calendar_Clock return Calendar.Time is
  begin
    return Calendar.Clock;
  end Calendar_Clock;


   -- Date_And_Time_Str => "YYYY-MM-DD HH:MM:SS.ZZZ"
  function To_Time_Type (Date_And_Time_Str : String) return Time_Type is
  --reindex strings to start with 1
    Date_Ts : String (1 .. Date_And_Time_Str'Last - Date_And_Time_Str'First + 1) := Date_And_Time_Str;
  begin
    return To_Time_Type(Date_Ts(1..10), Date_Ts(12..23));
  end To_Time_Type;



  function To_Time_Type (Date_Str : String;
                         Time_Str : String) return Time_Type is
  --reindex strings to start with 1
    Date : String (1 .. Date_Str'Last - Date_Str'First + 1) := Date_Str;
    Time : String (1 .. Time_Str'Last - Time_Str'First + 1) := Time_Str;

    A_Time_Type : Time_Type := Time_Type_First;
    function Month (Month_Str : in String) return Month_Type is
      Tmp_Month : constant String := Ada.Characters.Handling.To_Upper (Month_Str);
    begin
      if    Tmp_Month = "JAN"  then return  1;
      elsif Tmp_Month = "FEB"  then return  2;
      elsif Tmp_Month = "MAR"  then return  3;
      elsif Tmp_Month = "APR"  then return  4;
      elsif Tmp_Month = "MAY"  then return  5;
      elsif Tmp_Month = "JUN"  then return  6;
      elsif Tmp_Month = "JUL"  then return  7;
      elsif Tmp_Month = "AUG"  then return  8;
      elsif Tmp_Month = "SEP"  then return  9;
      elsif Tmp_Month = "OCT"  then return 10;
      elsif Tmp_Month = "NOV"  then return 11;
      elsif Tmp_Month = "DEC"  then return 12;
      else raise Invalid_Date_Format with "bad month: " & Month_Str;
      end if;
    end Month;
  begin

    if Date_Str'Length = 11 then
      A_Time_Type.Year  := Year_Type'Value (Date (8 .. 11));
      A_Time_Type.Month := Month (Date (4 .. 6));
      A_Time_Type.Day   := Day_Type'Value (Date (1 .. 2));
    elsif Date_Str'Length = 10 then
      A_Time_Type.Year  := Year_Type'Value (Date (1 .. 4));
      A_Time_Type.Month := Month_Type'Value (Date (6 .. 7));
      A_Time_Type.Day   := Day_Type'Value (Date (9 .. 10));
    end if;


    if Time'Length  >= 8 then
      A_Time_Type.Hour   := Hour_Type'Value (Time (1 .. 2));
      A_Time_Type.Minute := Minute_Type'Value (Time (4 .. 5));
      A_Time_Type.Second := Second_Type'Value (Time (7 .. 8));
      if Time'Length  >= 12 then
        A_Time_Type.Millisecond := Millisecond_Type'Value (Time (10 .. 12));
      end if;
    end if;
    return A_Time_Type;
  exception
    when Constraint_Error => raise Invalid_Date_Format with "Date_Str='" & Date_Str & "' time_str='" & Time_Str & "'" ;  --     chg-25546
  end To_Time_Type;

  function To_Time (Date : in Calendar.Time) return Time_Type is
    Seconds_Times_1000 : Integer_4 := Integer_4 (Fixed_Type (
                                                 Calendar.Seconds (Date)) * 1000.0);
    Seconds            : Integer_4 := Seconds_Times_1000 / 1000;
  begin
    if Seconds_Times_1000 = 86_400_000 then
      Seconds_Times_1000 := Seconds_Times_1000 - 1;
      Seconds := Seconds_Times_1000 / 1000;
    end if;

    return (Year        => Year_Type (Calendar.Year (Date)),
            Month       => Month_Type (Calendar.Month (Date)),
            Day         => Day_Type (Calendar.Day (Date)),
            Hour        => Hour_Type (Seconds / Seconds_Per_Hour),
            Minute      => Minute_Type ((Seconds rem Seconds_Per_Hour)
              / Seconds_Per_Minute),
            Second      => Second_Type ((Seconds rem Seconds_Per_Hour)
              rem Seconds_Per_Minute),
            Millisecond => Millisecond_Type (Seconds_Times_1000 rem 1000));
  end To_Time;


  function To_Calendar_Time (Date : in Time_Type) return Calendar.Time is
    Time  : Calendar.Day_Duration;
    Tmp_1 : constant Integer_4 := Integer_4 (Date.Hour) * Seconds_Per_Hour;
    Tmp_2 : constant Integer_4 := Integer_4 (Date.Minute) * Seconds_Per_Minute;
  begin
    Time := Calendar.Day_Duration (Tmp_1) +
      Calendar.Day_Duration (Tmp_2) +
      Calendar.Day_Duration (Date.Second) +
      Calendar.Day_Duration (Date.Millisecond) / 1000;
    return Calendar.Time_Of (Calendar.Year_Number (Date.Year),
                             Calendar.Month_Number (Date.Month),
                             Calendar.Day_Number (Date.Day), Time);
  end To_Calendar_Time;


  function To_Time (Year        : in Year_Type;
                    Year_Day    : in Year_Day_Type;
                    Hour        : in Hour_Type;
                    Minute      : in Minute_Type;
                    Second      : in Second_Type;
                    Millisecond : in Millisecond_Type) return Time_Type is
    Date_Time : Time_Type;
  begin
    if not Is_Legal (Year, Year_Day) then
      raise In_Parameter_Incorrect;
    end if;
    To_Month_And_Day (Year, Year_Day, Date_Time.Month, Date_Time.Day);
    Date_Time.Year        := Year;
    Date_Time.Hour        := Hour;
    Date_Time.Minute      := Minute;
    Date_Time.Second      := Second;
    Date_Time.Millisecond := Millisecond;
    return Date_Time;
  end To_Time;

  function To_Time (Year        : in Year_Type;
                    Week        : in Week_Type;
                    Day         : in Week_Day_Type;
                    Hour        : in Hour_Type   := Hour_Type'First;
                    Minute      : in Minute_Type := Minute_Type'First;
                    Second      : in Second_Type := Second_Type'First;
                    Millisecond : in Millisecond_Type := Millisecond_Type'First)
                     return Time_Type is
    Decode_Day        : Integer_2 := Week_Day_Type'Pos (Day) + 1;
    End_Of_First_Week : Year_Day_Type;
    First_Week_Day    : Week_Day_Type;
    My_Year           : Year_Type;
    My_Julian_Date    : Year_Day_Type;
    Tmp_Julian_Date   : Integer_2;

  begin

    My_Year := Year;
    First_Week_Day := Week_Day_Of (My_Year, 1, 1);

    End_Of_First_Week := Year_Day_Type (7 - Week_Day_Type'Pos (First_Week_Day));

    if Week = 1 then  -- First week of the year needs special treatment

      if First_Week_Day > Thursday then
        My_Julian_Date := Decode_Day + End_Of_First_Week;
      elsif Decode_Day < Week_Day_Type'Pos (First_Week_Day) + 1 then
        My_Year := My_Year - 1;
        My_Julian_Date := Year_Day_Of (My_Year, 12, 31) -
          (Week_Day_Type'Pos (First_Week_Day) - Decode_Day);
      else
        My_Julian_Date := Decode_Day - Week_Day_Type'Pos (First_Week_Day);
      end if;

    else              --week=in the middle of the year

      if First_Week_Day > Thursday then  -- 1/1 belongs to week 52 or 53 of previous year
        Tmp_Julian_Date := (Week - 1) * 7 + Decode_Day + End_Of_First_Week;
      else  -- 1/1 belongs to week 1 this year
        Tmp_Julian_Date := (Week - 2) * 7 + Decode_Day + End_Of_First_Week;
      end if;

      -- If last week of this year is in next year
      if Tmp_Julian_Date > Days_In (My_Year) then
        My_Julian_Date := Tmp_Julian_Date - Days_In (My_Year);
        My_Year := My_Year + 1;
      else
        My_Julian_Date := Tmp_Julian_Date;
      end if;

    end if;
    return To_Time (My_Year, My_Julian_Date, Hour, Minute, Second, Millisecond);
  end To_Time;


  function "<"  (Left, Right : in Time_Type) return Boolean is
  begin
    if Left.Year /= Right.Year then
      return (Left.Year < Right.Year);
    elsif Left.Month /= Right.Month then
      return (Left.Month < Right.Month);
    elsif Left.Day /= Right.Day then
      return (Left.Day < Right.Day);
    elsif Left.Hour /= Right.Hour then
      return (Left.Hour < Right.Hour);
    elsif Left.Minute /= Right.Minute then
      return (Left.Minute < Right.Minute);
    elsif Left.Second /= Right.Second then
      return (Left.Second < Right.Second);
    elsif Left.Millisecond /= Right.Millisecond then
      return (Left.Millisecond < Right.Millisecond);
    else
      return False;
    end if;
  end "<";


  function "<=" (Left, Right : in Time_Type) return Boolean is
  begin
    if Left.Year /= Right.Year then
      return (Left.Year < Right.Year);
    elsif Left.Month /= Right.Month then
      return (Left.Month < Right.Month);
    elsif Left.Day /= Right.Day then
      return (Left.Day < Right.Day);
    elsif Left.Hour /= Right.Hour then
      return (Left.Hour < Right.Hour);
    elsif Left.Minute /= Right.Minute then
      return (Left.Minute < Right.Minute);
    elsif Left.Second /= Right.Second then
      return (Left.Second < Right.Second);
    elsif Left.Millisecond /= Right.Millisecond then
      return (Left.Millisecond < Right.Millisecond);
    else
      return True;
    end if;
  end "<=";

  --

  function ">"  (Left, Right : in Time_Type) return Boolean is
  begin
    if Left.Year /= Right.Year then
      return (Left.Year > Right.Year);
    elsif Left.Month /= Right.Month then
      return (Left.Month > Right.Month);
    elsif Left.Day /= Right.Day then
      return (Left.Day > Right.Day);
    elsif Left.Hour /= Right.Hour then
      return (Left.Hour > Right.Hour);
    elsif Left.Minute /= Right.Minute then
      return (Left.Minute > Right.Minute);
    elsif Left.Second /= Right.Second then
      return (Left.Second > Right.Second);
    elsif Left.Millisecond /= Right.Millisecond then
      return (Left.Millisecond > Right.Millisecond);
    else
      return False;
    end if;
  end ">";

  --

  function ">=" (Left, Right : in Time_Type) return Boolean is
  begin
    if Left.Year /= Right.Year then
      return (Left.Year > Right.Year);
    elsif Left.Month /= Right.Month then
      return (Left.Month > Right.Month);
    elsif Left.Day /= Right.Day then
      return (Left.Day > Right.Day);
    elsif Left.Hour /= Right.Hour then
      return (Left.Hour > Right.Hour);
    elsif Left.Minute /= Right.Minute then
      return (Left.Minute > Right.Minute);
    elsif Left.Second /= Right.Second then
      return (Left.Second > Right.Second);
    elsif Left.Millisecond /= Right.Millisecond then
      return (Left.Millisecond > Right.Millisecond);
    else
      return True;
    end if;
  end ">=";

  --

  function "<"  (Left, Right : in Interval_Type) return Boolean is
  begin
    if Left.Days /= Right.Days then
      return (Left.Days < Right.Days);
    elsif Left.Hours /= Right.Hours then
      return (Left.Hours < Right.Hours);
    elsif Left.Minutes /= Right.Minutes then
      return (Left.Minutes < Right.Minutes);
    elsif Left.Seconds /= Right.Seconds then
      return (Left.Seconds < Right.Seconds);
    elsif Left.Milliseconds /= Right.Milliseconds then
      return (Left.Milliseconds < Right.Milliseconds);
    else
      return False;
    end if;
  end "<";

  --

  function "<=" (Left, Right : in Interval_Type) return Boolean is
  begin
    if Left.Days /= Right.Days then
      return (Left.Days < Right.Days);
    elsif Left.Hours /= Right.Hours then
      return (Left.Hours < Right.Hours);
    elsif Left.Minutes /= Right.Minutes then
      return (Left.Minutes < Right.Minutes);
    elsif Left.Seconds /= Right.Seconds then
      return (Left.Seconds < Right.Seconds);
    elsif Left.Milliseconds /= Right.Milliseconds then
      return (Left.Milliseconds < Right.Milliseconds);
    else
      return True;
    end if;
  end "<=";

  --

  function ">"  (Left, Right : in Interval_Type) return Boolean is
  begin
    if Left.Days /= Right.Days then
      return (Left.Days > Right.Days);
    elsif Left.Hours /= Right.Hours then
      return (Left.Hours > Right.Hours);
    elsif Left.Minutes /= Right.Minutes then
      return (Left.Minutes > Right.Minutes);
    elsif Left.Seconds /= Right.Seconds then
      return (Left.Seconds > Right.Seconds);
    elsif Left.Milliseconds /= Right.Milliseconds then
      return (Left.Milliseconds > Right.Milliseconds);
    else
      return False;
    end if;
  end ">";

  --

  function ">=" (Left, Right : in Interval_Type) return Boolean is
  begin
    if Left.Days /= Right.Days then
      return (Left.Days > Right.Days);
    elsif Left.Hours /= Right.Hours then
      return (Left.Hours > Right.Hours);
    elsif Left.Minutes /= Right.Minutes then
      return (Left.Minutes > Right.Minutes);
    elsif Left.Seconds /= Right.Seconds then
      return (Left.Seconds > Right.Seconds);
    elsif Left.Milliseconds /= Right.Milliseconds then
      return (Left.Milliseconds > Right.Milliseconds);
    else
      return True;
    end if;
  end ">=";

  function To_Interval (Day_Duration : in Calendar.Day_Duration) return Interval_Type is
    Hours        : Integer_4;
    Minutes      : Integer_4;
    Seconds      : Integer_4;
    Milliseconds : Integer_4 := Integer_4 (Fixed_Type (Day_Duration) * 1000.0);
  begin

    if (Milliseconds = 86_400_000) then
      --
      -- Special case. 24:00:00.000 is not a legal time. The user has probably
      -- used CALENDAR.DAY_DURATION'LAST. We therefore substract one msec
      -- to get 23:59:59.999 instead.
      --
      Milliseconds := Milliseconds - 1;
    end if;

    Hours        := Milliseconds / 3_600_000;
    Milliseconds := Milliseconds - Hours * 3_600_000;

    Minutes      := Milliseconds / 60_000;
    Milliseconds := Milliseconds - Minutes * 60_000;

    Seconds      := Milliseconds / 1_000;
    Milliseconds := Milliseconds - Seconds * 1_000;

    return (Days         => 0,
            Hours        => Hour_Type (Hours),
            Minutes      => Minute_Type (Minutes),
            Seconds      => Second_Type (Seconds),
            Milliseconds => Millisecond_Type (Milliseconds));
  end To_Interval;


  function To_Interval (Seconds  : in Seconds_Type)  return Interval_Type is
    Seconds_Left  : Seconds_Type := 0;
    Time_Interval : Interval_Type := (0, 0, 0, 0, 0);
  begin
    Time_Interval.Days    := Seconds / Seconds_Per_Day;
    Seconds_Left          := Seconds - Time_Interval.Days * Seconds_Per_Day;
    Time_Interval.Hours   := Integer_2 (Seconds_Left / Seconds_Per_Hour);
    Seconds_Left          := Seconds_Left -
      Integer_4 (Time_Interval.Hours) * Seconds_Per_Hour;
    Time_Interval.Minutes := Integer_2 (Seconds_Left / Seconds_Per_Minute);
    Time_Interval.Seconds := Integer_2 (Seconds_Left) -
      Time_Interval.Minutes * Integer_2 (Seconds_Per_Minute);
    return Time_Interval;
  end To_Interval;

  function To_Seconds  (Interval : in Interval_Type) return Seconds_Type is
  begin
    return Interval.Days               * Seconds_Per_Day    +
      Integer_4 (Interval.Hours)   * Seconds_Per_Hour   +
      Integer_4 (Interval.Minutes) * Seconds_Per_Minute +
      Integer_4 (Interval.Seconds);
  end To_Seconds;

  function To_Day_Duration (Interval : in Interval_Type)
                             return Calendar.Day_Duration is

  begin
    return Calendar.Day_Duration (
                                  Integer_4 (Interval.Hours) * Seconds_Per_Hour +

                                    Integer_4 (Interval.Minutes) * Seconds_Per_Minute) +

      Calendar.Day_Duration (Interval.Seconds) +

      Calendar.Day_Duration (Interval.Milliseconds) / 1000;
  end To_Day_Duration;

  --

  function Add_Days (Left : in Interval_Type; Right : in Interval_Day_Type)
                      return Interval_Type is
  begin
    return Interval_Type'(Left.Days + Right,
                          Left.Hours,
                          Left.Minutes,
                          Left.Seconds,
                          Left.Milliseconds);
  exception
    when Constraint_Error =>
      raise Time_Error;
  end Add_Days;

  function Add_Hours (Left : in Interval_Type; Right : in Hour_Type)
                       return Interval_Type is
  begin
    return Interval_Type'(Left.Days,
                          Left.Hours + Right,
                          Left.Minutes,
                          Left.Seconds,
                          Left.Milliseconds);
  exception
    when Constraint_Error =>
      return Add_Days
        (Interval_Type'(Left.Days,
         Left.Hours + Right - 24,
         Left.Minutes,
         Left.Seconds,
         Left.Milliseconds), 1);
  end Add_Hours;

  function Add_Minutes (Left : in Interval_Type; Right : in Minute_Type)
                         return Interval_Type is
  begin
    return Interval_Type'(Left.Days,
                          Left.Hours,
                          Left.Minutes + Right,
                          Left.Seconds,
                          Left.Milliseconds);
  exception
    when Constraint_Error =>
      return Add_Hours
        (Interval_Type'(Left.Days,
         Left.Hours,
         Left.Minutes + Right - 60,
         Left.Seconds,
         Left.Milliseconds), 1);
  end Add_Minutes;

  function Add_Seconds (Left  : in Interval_Type;
                        Right : in Second_Type) return Interval_Type is
  begin
    return Interval_Type'(Left.Days,
                          Left.Hours,
                          Left.Minutes,
                          Left.Seconds + Right,
                          Left.Milliseconds);
  exception
    when Constraint_Error =>
      return Add_Minutes
        (Interval_Type'(Left.Days,
         Left.Hours,
         Left.Minutes,
         Left.Seconds + Right - 60,
         Left.Milliseconds), 1);
  end Add_Seconds;

  function Add_Milliseconds (Left : in Interval_Type; Right : in Millisecond_Type)
                              return Interval_Type is
  begin
    return Interval_Type'(Left.Days,
                          Left.Hours,
                          Left.Minutes,
                          Left.Seconds,
                          Left.Milliseconds + Right);
  exception
    when Constraint_Error =>
      return Add_Seconds
        (Interval_Type'(Left.Days,
         Left.Hours,
         Left.Minutes,
         Left.Seconds,
         Left.Milliseconds + Right - 1000), 1);
  end Add_Milliseconds;




  function Subtract_Days (Left : in Interval_Type; Right : in Interval_Day_Type)
                           return Interval_Type is
  begin
    return Interval_Type'(Left.Days - Right,
                          Left.Hours,
                          Left.Minutes,
                          Left.Seconds,
                          Left.Milliseconds);
  exception
    when Constraint_Error =>
      raise Time_Error;
  end Subtract_Days;

  function Subtract_Hours (Left : in Interval_Type; Right : in Hour_Type)
                            return Interval_Type is
  begin
    return Interval_Type'(Left.Days,
                          Left.Hours - Right,
                          Left.Minutes,
                          Left.Seconds,
                          Left.Milliseconds);
  exception
    when Constraint_Error =>
      return Subtract_Days
        (Interval_Type'(Left.Days,
         Left.Hours - Right + 24,
         Left.Minutes,
         Left.Seconds,
         Left.Milliseconds), 1);
  end Subtract_Hours;

  function Subtract_Minutes (Left : in Interval_Type; Right : in Minute_Type)
                              return Interval_Type is
  begin
    return Interval_Type'(Left.Days,
                          Left.Hours,
                          Left.Minutes - Right,
                          Left.Seconds,
                          Left.Milliseconds);
  exception
    when Constraint_Error =>
      return Subtract_Hours
        (Interval_Type'(Left.Days,
         Left.Hours,
         Left.Minutes - Right + 60,
         Left.Seconds,
         Left.Milliseconds), 1);
  end Subtract_Minutes;

  function Subtract_Seconds (Left  : in Interval_Type;
                             Right : in Second_Type) return Interval_Type is
  begin
    return Interval_Type'(Left.Days,
                          Left.Hours,
                          Left.Minutes,
                          Left.Seconds - Right,
                          Left.Milliseconds);
  exception
    when Constraint_Error =>
      return Subtract_Minutes
        (Interval_Type'(Left.Days,
         Left.Hours,
         Left.Minutes,
         Left.Seconds - Right + 60,
         Left.Milliseconds), 1);
  end Subtract_Seconds;

  function Subtract_Milliseconds (Left : in Interval_Type; Right : in Millisecond_Type)
                                   return Interval_Type is
  begin
    return Interval_Type'(Left.Days,
                          Left.Hours,
                          Left.Minutes,
                          Left.Seconds,
                          Left.Milliseconds - Right);
  exception
    when Constraint_Error =>
      return Subtract_Seconds
        (Interval_Type'(Left.Days,
         Left.Hours,
         Left.Minutes,
         Left.Seconds,
         Left.Milliseconds - Right + 1000), 1);
  end Subtract_Milliseconds;

  function Add_Days (Left : in Time_Type; Right : in Interval_Day_Type)
                      return Time_Type is
    Result : Time_Type         := Left;
    Days   : Interval_Day_Type := Right + Interval_Day_Type (Left.Day);
  begin
    while Days > Interval_Day_Type (Days_In (Result.Year, Result.Month)) loop
      Days := Days - Interval_Day_Type (Days_In (Result.Year, Result.Month));
      if Result.Month = 12 then
        if Result.Year = Year_Type'Last then
          raise Time_Error;
        end if;
        Result.Month := 1;
        Result.Year := Result.Year + 1;
      else
        Result.Month := Result.Month + 1;
      end if;
    end loop;
    Result.Day := Day_Type (Days);
    return Result;
  end Add_Days;

  function Add_Hours (Left : in Time_Type; Right : in Hour_Type)
                       return Time_Type is
  begin
    return Time_Type'(Left.Year,
                      Left.Month,
                      Left.Day,
                      Left.Hour + Right,
                      Left.Minute,
                      Left.Second,
                      Left.Millisecond);
  exception
    when Constraint_Error =>
      return Add_Days
        (Time_Type'(Left.Year,
         Left.Month,
         Left.Day,
         Left.Hour + Right - 24,
         Left.Minute,
         Left.Second,
         Left.Millisecond), 1);
  end Add_Hours;

  function Add_Minutes (Left : in Time_Type; Right : in Minute_Type)
                         return Time_Type is
  begin
    return Time_Type'(Left.Year,
                      Left.Month,
                      Left.Day,
                      Left.Hour,
                      Left.Minute + Right,
                      Left.Second,
                      Left.Millisecond);
  exception
    when Constraint_Error =>
      return Add_Hours
        (Time_Type'(Left.Year,
         Left.Month,
         Left.Day,
         Left.Hour,
         Left.Minute + Right - 60,
         Left.Second,
         Left.Millisecond), 1);
  end Add_Minutes;

  function Add_Seconds (Left  : in Time_Type;
                        Right : in Second_Type) return Time_Type is
  begin
    return Time_Type'(Left.Year,
                      Left.Month,
                      Left.Day,
                      Left.Hour,
                      Left.Minute,
                      Left.Second + Right,
                      Left.Millisecond);
  exception
    when Constraint_Error =>
      return Add_Minutes
        (Time_Type'(Left.Year,
         Left.Month,
         Left.Day,
         Left.Hour,
         Left.Minute,
         Left.Second + Right - 60,
         Left.Millisecond), 1);
  end Add_Seconds;

  function Add_Milliseconds (Left : in Time_Type; Right : in Millisecond_Type)
                              return Time_Type is
  begin
    return Time_Type'(Left.Year,
                      Left.Month,
                      Left.Day,
                      Left.Hour,
                      Left.Minute,
                      Left.Second,
                      Left.Millisecond + Right);
  exception
    when Constraint_Error =>
      return Add_Seconds
        (Time_Type'(Left.Year,
         Left.Month,
         Left.Day,
         Left.Hour,
         Left.Minute,
         Left.Second,
         Left.Millisecond + Right - 1000), 1);
  end Add_Milliseconds;

  function Subtract_Days (Left : in Time_Type; Right : in Interval_Day_Type)
                           return Time_Type is
    Result : Time_Type         := Left;
    Days   : Interval_Day_Type := Right;
  begin
    while Days >= Interval_Day_Type (Result.Day) loop
      Days := Days - Interval_Day_Type (Result.Day);
      if Result.Month = 1 then
        if Result.Year = Year_Type'First then
          raise Time_Error;
        end if;
        Result.Month := 12;
        Result.Year := Result.Year - 1;
      else
        Result.Month := Result.Month - 1;
      end if;
      Result.Day := Days_In (Result.Year, Result.Month);
    end loop;
    Result.Day := Day_Type (Interval_Day_Type (Result.Day) - Days);
    return Result;
  end Subtract_Days;

  function Subtract_Hours (Left : in Time_Type; Right : in Hour_Type)
                            return Time_Type is
  begin
    return Time_Type'(Left.Year,
                      Left.Month,
                      Left.Day,
                      Left.Hour - Right,
                      Left.Minute,
                      Left.Second,
                      Left.Millisecond);
  exception
    when Constraint_Error =>
      return Subtract_Days
        (Time_Type'(Left.Year,
         Left.Month,
         Left.Day,
         Left.Hour - Right + 24,
         Left.Minute,
         Left.Second,
         Left.Millisecond), 1);
  end Subtract_Hours;

  function Subtract_Minutes (Left : in Time_Type; Right : in Minute_Type)
                              return Time_Type is
  begin
    return Time_Type'(Left.Year,
                      Left.Month,
                      Left.Day,
                      Left.Hour,
                      Left.Minute - Right,
                      Left.Second,
                      Left.Millisecond);
  exception
    when Constraint_Error =>
      return Subtract_Hours
        (Time_Type'(Left.Year,
         Left.Month,
         Left.Day,
         Left.Hour,
         Left.Minute - Right + 60,
         Left.Second,
         Left.Millisecond), 1);
  end Subtract_Minutes;

  function Subtract_Seconds (Left  : in Time_Type;
                             Right : in Second_Type) return Time_Type is
  begin
    return Time_Type'(Left.Year,
                      Left.Month,
                      Left.Day,
                      Left.Hour,
                      Left.Minute,
                      Left.Second - Right,
                      Left.Millisecond);
  exception
    when Constraint_Error =>
      return Subtract_Minutes
        (Time_Type'(Left.Year,
         Left.Month,
         Left.Day,
         Left.Hour,
         Left.Minute,
         Left.Second - Right + 60,
         Left.Millisecond), 1);
  end Subtract_Seconds;

  function Subtract_Milliseconds (Left : in Time_Type; Right : in Millisecond_Type)
                                   return Time_Type is
  begin
    return Time_Type'(Left.Year,
                      Left.Month,
                      Left.Day,
                      Left.Hour,
                      Left.Minute,
                      Left.Second,
                      Left.Millisecond - Right);
  exception
    when Constraint_Error =>
      return Subtract_Seconds
        (Time_Type'(Left.Year,
         Left.Month,
         Left.Day,
         Left.Hour,
         Left.Minute,
         Left.Second,
         Left.Millisecond - Right + 1000), 1);
  end Subtract_Milliseconds;

  function "+" (Left : in Time_Type; Right : in Interval_Type) return Time_Type is
    Result : Time_Type := Left;
  begin
    if not Is_Legal (Left) then
      raise In_Parameter_Incorrect;
    end if;
    Result := Add_Milliseconds (Result, Right.Milliseconds);
    Result := Add_Seconds      (Result, Right.Seconds);
    Result := Add_Minutes      (Result, Right.Minutes);
    Result := Add_Hours        (Result, Right.Hours);
    Result := Add_Days         (Result, Right.Days);
    return Result;
  end "+";

  --

  function "+" (Left : in Interval_Type; Right : in Time_Type) return Time_Type is
  begin
    return Right + Left;
  end "+";

  --

  function "+" (Left : in Time_Type; Right : in Year_Day_Type) return Time_Type is
  begin
    return Add_Days (Left, Interval_Day_Type (Right));
  end "+";

  --

  function "+" (Left : in Year_Day_Type; Right : in Time_Type) return Time_Type is
  begin
    return Right + Left;
  end "+";

  --

  function "-" (Left, Right : in Time_Type) return Interval_Type is
    Result : Interval_Type := (Days         => 0,
                               Hours        => Left.Hour,
                               Minutes      => Left.Minute,
                               Seconds      => Left.Second,
                               Milliseconds => Left.Millisecond);
  begin
    if not Is_Legal (Left) or not Is_Legal (Right) then
      raise In_Parameter_Incorrect;
    elsif Left < Right then
      --9.3-0085      raise TIME_ERROR;
      return Interval_Type_First;  --9.3-0085
    end if;
    for Year_Number in (Right.Year + 1) .. (Left.Year - 1) loop
      Result.Days := Result.Days +
        Interval_Day_Type (Days_In (Year_Number));
    end loop;

    if Right.Year = Left.Year then
      for Month_Number in (Right.Month + 1) .. (Left.Month - 1) loop
        Result.Days := Result.Days +
          Interval_Day_Type (Days_In (Left.Year, Month_Number));
      end loop;
    else
      for Month_Number in (Right.Month + 1) .. Month_Type'Last loop
        Result.Days := Result.Days +
          Interval_Day_Type (Days_In (Right.Year, Month_Number));
      end loop;
      for Month_Number in Month_Type'First .. (Left.Month - 1) loop
        Result.Days := Result.Days +
          Interval_Day_Type (Days_In (Left.Year, Month_Number));
      end loop;
    end if;

    if Right.Year  = Left.Year and then
      Right.Month = Left.Month then
      Result.Days := Result.Days +
        Interval_Day_Type (Left.Day) -
        Interval_Day_Type (Right.Day );
    else
      Result.Days := Result.Days +
        Interval_Day_Type (
                           Days_In (Right.Year, Right.Month) - Right.Day) +
          Interval_Day_Type (Left.Day);
    end if;

    Result := Subtract_Milliseconds (Result, Right.Millisecond);
    Result := Subtract_Seconds      (Result, Right.Second);
    Result := Subtract_Minutes      (Result, Right.Minute);
    Result := Subtract_Hours        (Result, Right.Hour);

    return Result;
  end "-";

  --

  function "-" (Left : in Time_Type; Right : in Interval_Type) return Time_Type is
    Result : Time_Type := Left;
  begin
    if not Is_Legal (Left) then
      raise In_Parameter_Incorrect;
    end if;
    Result := Subtract_Milliseconds (Result, Right.Milliseconds);
    Result := Subtract_Seconds      (Result, Right.Seconds);
    Result := Subtract_Minutes      (Result, Right.Minutes);
    Result := Subtract_Hours        (Result, Right.Hours);
    Result := Subtract_Days         (Result, Right.Days);
    return Result;
  end "-";

  --

  function "-" (Left : in Time_Type; Right : in Year_Day_Type) return Time_Type is
  begin
    return Subtract_Days (Left, Interval_Day_Type (Right));
  end "-";

  --

  function "+" (Left, Right : in Interval_Type) return Interval_Type is
    Result : Interval_Type := Left;
  begin
    Result := Add_Milliseconds (Result, Right.Milliseconds);
    Result := Add_Seconds      (Result, Right.Seconds);
    Result := Add_Minutes      (Result, Right.Minutes);
    Result := Add_Hours        (Result, Right.Hours);
    Result := Add_Days         (Result, Right.Days);
    return Result;
  end "+";

  --

  function "-" (Left, Right : in Interval_Type) return Interval_Type is
    Result : Interval_Type := Left;
  begin
    Result := Subtract_Milliseconds (Result, Right.Milliseconds);
    Result := Subtract_Seconds      (Result, Right.Seconds);
    Result := Subtract_Minutes      (Result, Right.Minutes);
    Result := Subtract_Hours        (Result, Right.Hours);
    Result := Subtract_Days         (Result, Right.Days);
    return Result;
  end "-";

  --

  function Is_Leap_Year (Year : in Year_Type) return Boolean is
  begin
    if Year mod 100 = 0 then
      return Year mod 400 = 0;
    else
      return Year mod 4 = 0;
    end if;
  end Is_Leap_Year;

  --

  function Year_Day_Of (Date : in Time_Type) return Year_Day_Type is
  begin
    return Year_Day_Of (Date.Year, Date.Month, Date.Day);
  end Year_Day_Of;

  --

  function Year_Day_Of (Year  : in Year_Type;
                        Month : in Month_Type;
                        Day   : in Day_Type) return Year_Day_Type is
    Cumulative_Days : Year_Day_Type := Year_Day_Type (Day);
  begin
    if not Is_Legal (Year, Month, Day) then
      raise In_Parameter_Incorrect;
    end if;
    for Month_Number in Month_Type'First .. (Month - 1) loop
      Cumulative_Days := Cumulative_Days + Year_Day_Type (
                                                          Days_In (Year, Month_Number));
    end loop;
    return Cumulative_Days;
  end Year_Day_Of;

  --

  function Days_In (Year : in Year_Type) return Year_Day_Type is
  begin
    if Is_Leap_Year (Year) then
      return 366;
    else
      return 365;
    end if;
  end Days_In;

  --

  function Days_In (Year     : in Year_Type;
                    Month    : in Month_Type) return Day_Type is
  begin
    if Is_Leap_Year (Year) and Month = 2 then
      return 29;
    else
      return Month_Day (Month);
    end if;
  end Days_In;

  --

  function Week_Day_Of (Date : in Time_Type) return Week_Day_Type is
  begin
    return Week_Day_Of (Date.Year, Date.Month, Date.Day);
  end Week_Day_Of;

  --

  function Week_Day_Of (Year  : in Year_Type;
                        Month : in Month_Type;
                        Day   : in Day_Type) return Week_Day_Type is
  -- For every non leap year that elapses from a given date the week day
  -- is modified by one (365 days = 52 weeks of seven days + 1 day).
  -- Leap years modifies the week day by two.
  -- Our reference point in time is 31/12 1899, that happened to be a
  -- Sunday. From this point the number of modifications to the week-day
  -- is calculated. The calculated value modulus seven gives
  -- the offset from Sunday to the current week-day.
    The_Elapsed_Years : constant Integer_2 := Integer_2 (Year) - 1900;
    Adjustment        : Integer_2 :=
                          ((The_Elapsed_Years  - 1) / 4 + The_Elapsed_Years +
                             Integer_2 (Year_Day_Of (Year, Month, Day))) mod 7;
  begin
    if not Is_Legal (Year, Month, Day) then
      raise In_Parameter_Incorrect;
    end if;
    if Adjustment = 0 then
      Adjustment := 7;
    end if;
    return Week_Day_Type'Val (Adjustment - 1);
  end Week_Day_Of;

  --

  function Week_Of (Date : in Time_Type) return Week_Type is
  -- This version only works for the ISO R-2015 standard for week-numbers.
  begin
    return Week_Of (Date.Year, Year_Day_Of (Date.Year, Date.Month, Date.Day));
  end Week_Of;

  --

  function Week_Of (Year     : in Year_Type;
                    Month    : in Month_Type;
                    Day      : in Day_Type) return Week_Type is
  -- This version only works for the ISO R-2015 standard for week-numbers.
  begin
    return Week_Of (Year, Year_Day_Of (Year, Month, Day));
  end Week_Of;

  --

  function Week_Of (Year     : in Year_Type;
                    Year_Day : in Year_Day_Type) return Week_Type is
  -- This version of only works for the ISO R-2015 standard for week-numbers.
  -- This standard dates back to 30 June, 1972.

    -- According to ISO R-2015 the first week of a year containing a Thursday
    -- is labeled week one. This definition makes calculation of week numbers
    -- a bit tricky. Observe also that there is room for some ambiguity since
    -- dates at both the start and end of a year might belong to week one or
    -- week fifty-three.
    First_Week_Day     : constant Week_Day_Type := Week_Day_Of (Year, 1, 1);
    End_Of_First_Week  : constant Year_Day_Type :=
                           Year_Day_Type (7 -
                                                                 Week_Day_Type'Pos (First_Week_Day));
    Last_Week_Day      : constant Week_Day_Type := Week_Day_Of (Year, 12, 31);
    Start_Of_Last_Week : constant Year_Day_Type := Days_In (Year) -
                           Week_Day_Type'Pos (Last_Week_Day);
  begin
    if not Is_Legal (Year, Year_Day) then
      raise In_Parameter_Incorrect;
    end if;
    if Year_Day >= Start_Of_Last_Week and then Last_Week_Day < Thursday then
      -- Year_Day belongs to the last week of the year and this week
      -- is numbered one since Thursday is part of the next year.
      return 1;
    elsif First_Week_Day > Thursday then
      -- The first day of Year does not belong to week one since Thursday
      -- of this week is part of the previous year.
      if Year_Day <= End_Of_First_Week then
        -- Year_Day belongs to this week.
        return Week_Of (Year - 1, 12, 31);
      else
        -- Year_Day does not belong to this week.
        return Week_Type ((Year_Day - End_Of_First_Week - 1) / 7 + 1);
      end if;
    else
      -- The first day of the year is part of week one.
      return Week_Type (
                        (Year_Day + Week_Day_Type'Pos (First_Week_Day) - 1) / 7 + 1);
    end if;
  end Week_Of;


  function String_Date (Date : in Time_Type) return String is
    function Month_Name (Month : in Month_Type) return String is
    begin
      case Month is
        when  1 => return "Jan";
        when  2 => return "Feb";
        when  3 => return "Mar";
        when  4 => return "Apr";
        when  5 => return "May";
        when  6 => return "Jun";
        when  7 => return "Jul";
        when  8 => return "Aug";
        when  9 => return "Sep";
        when 10 => return "Oct";
        when 11 => return "Nov";
        when 12 => return "Dec";
          --        when others => return "   ";
      end case;
    end Month_Name;
  begin
    declare
      Date_String : String (1 .. 11) := "dd-mmm-yyyy";
    begin
      Integer_2_Io.Put (Date_String (1 .. 2), Date.Day);
      Date_String (4 .. 6) := Month_Name (Date.Month);
      Integer_2_Io.Put (Date_String (8 .. 11), Date.Year);
      if Date_String (1) = ' ' then Date_String (1) := '0'; end if;
      return Date_String;
    end;
  end String_Date;

  function String_Date_Iso (Date : in Time_Type) return String is
  begin
    declare
      Date_String : String (1 .. 10) := "yyyy-mm-dd";
    begin
      Integer_2_Io.Put (Date_String (1 .. 4), Date.Year);
      Integer_2_Io.Put (Date_String (6 .. 7), Date.Month);
      Integer_2_Io.Put (Date_String (9 .. 10), Date.Day);
      if Date_String (6) = ' ' then Date_String (6) := '0'; end if;
      if Date_String (9) = ' ' then Date_String (9) := '0'; end if;
      return Date_String;
    end;
  end String_Date_Iso;


  function String_Time
    (Date         : in Time_Type;
     Hours        : in Boolean := True;
     Minutes      : in Boolean := True;
     Seconds      : in Boolean := True;
     Milliseconds : in Boolean := False) return String is
    Result : String (1 .. 12) := (others => ' ');
    Curr   : Positive := Result'First;
  begin
    if Hours then
      Integer_2_Io.Put (Result (Curr .. Curr + 1), Date.Hour);
      Curr := Curr + 2;
    end if;
    if Minutes then
      if Curr /= Result'First then
        Result (Curr) := ':';
        Curr := Curr + 1;
      end if;
      Integer_2_Io.Put (Result (Curr .. Curr + 1), Date.Minute);
      Curr := Curr + 2;
    end if;
    if Seconds then
      if Curr /= Result'First then
        Result (Curr) := ':';
        Curr := Curr + 1;
      end if;
      Integer_2_Io.Put (Result (Curr .. Curr + 1), Date.Second);
      Curr := Curr + 2;
    end if;
    if Milliseconds then
      if Curr /= Result'First then
        Result (Curr) := '.';
        Curr := Curr + 1;
      end if;
      Integer_2_Io.Put (Result (Curr .. Curr + 2), Date.Millisecond);
      Curr := Curr + 3;
    end if;
    for I in Result'First .. Curr - 1 loop
      if Result (I) = ' ' then Result (I) := '0'; end if;
    end loop;
    return Result (Result'First .. Curr - 1);
  end String_Time;


  function String_Date_And_Time
    (Date         : in Time_Type;
     Hours        : in Boolean := True;
     Minutes      : in Boolean := True;
     Seconds      : in Boolean := True;
     Milliseconds : in Boolean := False) return String is
  begin
    return String_Date (Date) & " " &
      String_Time (Date, Hours, Minutes, Seconds, Milliseconds);
  end String_Date_And_Time;

  function String_Date (Date : in Calendar.Time := To_Calendar_Time (Clock))
                         return String is
  begin
    return String_Date (To_Time (Date));
  end String_Date;


  function String_Time
    (Date         : in Calendar.Time := To_Calendar_Time (Clock);
     Hours        : in Boolean := True;
     Minutes      : in Boolean := True;
     Seconds      : in Boolean := True;
     Milliseconds : in Boolean := False) return String is
  begin
    return String_Time (To_Time (Date), Hours, Minutes, Seconds, Milliseconds);
  end String_Time;


  function String_Date_And_Time
    (Date         : in Calendar.Time := To_Calendar_Time (Clock);
     Hours        : in Boolean := True;
     Minutes      : in Boolean := True;
     Seconds      : in Boolean := True;
     Milliseconds : in Boolean := False) return String is
  begin
    return String_Date_And_Time
      (To_Time (Date), Hours, Minutes, Seconds, Milliseconds);
  end String_Date_And_Time;

  function String_Interval
    (Interval     : in Interval_Type;
     Days         : in Boolean := True;
     Hours        : in Boolean := True;
     Minutes      : in Boolean := True;
     Seconds      : in Boolean := True;
     Milliseconds : in Boolean := True) return String is
    Result : String (1 .. 18) := (others => ' ');
    Curr   : Natural := Result'First;
  begin
    if Days then
      Integer_4_Io.Put (Result (Curr .. Curr + 4), Interval.Days);
      Curr := Curr + 5;
    end if;
    if Hours then
      if Curr /= Result'First then
        Result (Curr) := ':';
        Curr := Curr + 1;
      end if;
      Integer_2_Io.Put (Result (Curr .. Curr + 1), Interval.Hours);
      Curr := Curr + 2;
    end if;
    if Minutes then
      if Curr /= Result'First then
        Result (Curr) := ':';
        Curr := Curr + 1;
      end if;
      Integer_2_Io.Put (Result (Curr .. Curr + 1), Interval.Minutes);
      Curr := Curr + 2;
    end if;
    if Seconds then
      if Curr /= Result'First then
        Result (Curr) := ':';
        Curr := Curr + 1;
      end if;
      Integer_2_Io.Put (Result (Curr .. Curr + 1), Interval.Seconds);
      Curr := Curr + 2;
    end if;
    if Milliseconds then
      if Curr /= Result'First then
        Result (Curr) := '.';
        Curr := Curr + 1;
      end if;
      Integer_2_Io.Put (Result (Curr .. Curr + 2), Interval.Milliseconds);
      Curr := Curr + 3;
    end if;
    for I in Result'First .. Curr - 1 loop
      if Result (I) = ' ' then Result (I) := '0'; end if;
    end loop;
    return Result (Result'First .. Curr - 1);
  end String_Interval;


  function Clock_Of (T : in Time_Type) return Clock_Type is
  begin
    return (T.Hour, T.Minute, T.Second);
  end Clock_Of;

  function "<=" (Left, Right : in Clock_Type) return Boolean is
    Dummyleft  : Time_Type := Clock;
    Dummyright : Time_Type := Dummyleft;
  begin
    Dummyleft.Hour := Left.Hour;
    Dummyleft.Minute := Left.Minute;
    Dummyleft.Second := Left.Second;
    Dummyright.Hour := Right.Hour;
    Dummyright.Minute := Right.Minute;
    Dummyright.Second := Right.Second;
    return "<=" (Dummyleft, Dummyright);
  end "<=";

  function ">=" (Left, Right : in Clock_Type) return Boolean is
    Dummyleft  : Time_Type := Clock;
    Dummyright : Time_Type := Dummyleft;
  begin
    Dummyleft.Hour := Left.Hour;
    Dummyleft.Minute := Left.Minute;
    Dummyleft.Second := Left.Second;
    Dummyright.Hour := Right.Hour;
    Dummyright.Minute := Right.Minute;
    Dummyright.Second := Right.Second;
    return ">=" (Dummyleft, Dummyright);
  end ">=";

  function To_String (C : in Clock_Type) return String is
    Dummy : Time_Type := Clock;
  begin
    Dummy.Hour := C.Hour;
    Dummy.Minute := C.Minute;
    Dummy.Second := C.Second;
    return String_Time (Dummy);
  end To_String;


  function String_Date_Time_Iso (Date : in Time_Type; T : String := "T"; Tz : String := "Z"; Milliseconds : Boolean := True) return String is
    Da : String := String_Date_Iso (Date);
    Ti : String := String_Time(Date, Milliseconds => Milliseconds);
  begin
    return Da & T & Ti & Tz;
  end String_Date_Time_Iso;


  function To_String(D : Time_Type; Milliseconds : Boolean := True) return String is
  begin
    return String_Date_Time_Iso(Date => D, T => " " , Tz => "", Milliseconds => Milliseconds );
  end To_String;

  function Time_Type_First return Time_Type is
  begin
    return (Year        => Year_Type'First,
            Month       => Month_Type'First,
            Day         => Day_Type'First,
            Hour        => Hour_Type'First,
            Minute      => Minute_Type'First,
            Second      => Second_Type'First,
            Millisecond => Millisecond_Type'First);
  end Time_Type_First;

  function Time_Type_Last return Time_Type is
  begin
    return (Year        => Year_Type'Last,
            Month       => Month_Type'Last,
            Day         => Day_Type'Last,
            Hour        => Hour_Type'Last,
            Minute      => Minute_Type'Last,
            Second      => Second_Type'Last,
            Millisecond => Millisecond_Type'Last);
  end Time_Type_Last;

  function Time_Type_Last return Calendar.Time is
  begin
    return To_Calendar_Time(Time_Type_Last);
  end Time_Type_Last;

  function Time_Type_First return Calendar.Time is
  begin
    return To_Calendar_Time(Time_Type_First);
  end Time_Type_First;


end Calendar2;
