
with Stacktrace;
with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Strings;
with Tics;
with Text_Io;
with Types; use Types;

procedure Price_To_Tics is
  Cmd_Line    : Command_Line_Configuration;
  Sa_Value    : aliased Gnat.Strings.String_Access;
  Ba_To_Price : aliased Boolean := False;
  Ba_Tic_Table : aliased Boolean := False;
begin

  Define_Switch
    (Cmd_Line,
     Sa_value'Access,
     Long_Switch => "--value=",
     Help        => "price to be converted to tics");

  Define_Switch
    (Cmd_Line,
     Ba_To_Price'Access,
     Long_Switch => "--toprice",
     Help        => "tics to be converted to price");

  Define_Switch
    (Cmd_Line,
     Ba_Tic_Table'Access,
     Long_Switch => "--tictable",
     Help        => "Print tic-table");

  Getopt (Cmd_Line);  -- process the command line

  if Ba_Tic_Table then
    Tics.Tic_Table;
  else
    if Ba_To_Price then
      Text_Io.Put_Line(Tics.Get_Tic_Price(I => Tics.Tics_Type'Value(Sa_Value.all))'Img);
    else
      Text_Io.Put_Line(Tics.Get_Tic_Index(Price => Fixed_Type'Value(Sa_Value.all))'Img);
    end if;
  end if;



exception
  when GNAT.Command_Line.Exit_From_Command_Line =>
    null;
  when E : others =>
    Stacktrace.Tracebackinfo (E);
end Price_To_Tics;
