
package body Runners is

  function Empty_Data return Runner_Type is
    ED : Runner_Type;
  begin
    return ED;
  end Empty_Data;
  -----------------------------------------
  procedure Read_List(Stm  : in     Sql.Statement_Type;
                      List : in out Lists.List;
                      Max  : in     Integer_4 := Integer_4'Last) is
    Old_List :Table_Arunners.Arunners_List_Pack2.List;
    New_Data : Runner_Type;
  begin
    Table_Arunners.Read_List(Stm, Old_List, Max);  
    for i of Old_List loop
      New_Data := (
          Marketid           => i.Marketid,
          Selectionid        => i.Selectionid,
          Sortprio           => I.Sortprio,  
          Status             => i.Status,
          Handicap           => i.Handicap,
          Runnername         => i.Runnername,
          Runnernamestripped => i.Runnernamestripped,
          Runnernamenum      => i.Runnernamenum,
          Ixxlupd            => i.Ixxlupd,
          Ixxluts            => i.Ixxluts
      );             
      List.Append(New_Data);
    end loop;
  end Read_List;  
  ----------------------------------------
  function Is_Winner(Self : in out Runner_Type) return Boolean is
  begin
    return Self.Status(1..6) = "WINNER";
  end Is_Winner;  

  ----------------------------------------
  function Is_Removed(Self : in out Runner_Type) return Boolean is
  begin
    return Self.Status(1..7) = "REMOVED";
  end Is_Removed;  

  ----------------------------------------
  
end Runners;

