

package body Filters is

  procedure Add(Self : in out Filter_Type; Value : Fixed_Type; Timestamp : Time_Type) is
  begin
    -- if first values is 200ms or older then ok
    if Timestamp - Self.Values(1).Timestamp >= (0,0,0,0,200) then
      for I in reverse Self.Values'First +1 .. Self.Values'Last loop
        Self.Values(I) := Self.Values(I-1);
      end loop;
      Self.Values(Self.Values'First) := (Value,Timestamp);
    end if;
  end Add;

  procedure Recalculate(Self : in out Filter_Type) is
  begin
    Self.Mean := 0.0;
    for I in Self.Values'Range loop
      Self.Mean := Self.Mean + Self.Values(I).Value * Self.Weights(I);
    end loop;
    Self.Mean := Self.Mean / Fixed_Type(Self.Values'Length);
  end Recalculate;


end Filters;
