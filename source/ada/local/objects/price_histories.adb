package body Price_Histories is

  function Empty_Data return Price_History_Type is
    ED : Price_History_Type;
  begin
    return ED;
  end Empty_Data;

  ----------------------------------------

  procedure Read_List(Stm  : in     Sql.Statement_Type;
                      List : in out Lists.List;
                      Max  : in     Integer_4 := Integer_4'Last) is
    Old_List :Table_Apriceshistory.Apriceshistory_List_Pack2.List;
    New_Data : Price_History_Type;
  begin
    Table_Apriceshistory.Read_List(Stm, Old_List, Max);  
    for i of Old_List loop
      New_Data := (
          Marketid     => i.Marketid,
          Selectionid  => i.Selectionid,
          Pricets      => i.Pricets,
          Status       => i.Status,
          Totalmatched => i.Totalmatched,
          Backprice    => i.Backprice,
          Layprice     => i.Layprice,
          Ixxlupd      => i.Ixxlupd,
          Ixxluts      => i.Ixxluts
      );        
      List.Append(New_Data);
    end loop;
  end Read_List;  
  ----------------------------------------
end Price_Histories;
