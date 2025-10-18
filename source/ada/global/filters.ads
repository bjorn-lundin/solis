
with Types; use Types;
with Calendar2; use Calendar2;

package Filters is

  type Value_Type is record
    Value     :  Fixed_Type := 0.0;
    Timestamp :  Time_Type := Time_Type_First;
  end record;

  type Value_Array_Type is array(1..10) of Value_Type;
  type Fixed_Array_Type is array(1..10) of Fixed_Type;

--  type Filter_Type is tagged private ;
  type Filter_Type is tagged record
    Selectionid : Integer_4  := 0;
    Values      : Value_Array_Type ;--:= (others => 0.0);
    Weights     : Fixed_Array_Type := (others => 1.0); --(1.0,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1);
    Mean        : Fixed_Type := 0.0;
  end record;

  procedure Add(Self : in out Filter_Type; Value : Fixed_Type; Timestamp :  Time_Type);
  procedure Recalculate(Self : in out Filter_Type);

--private


end Filters;
