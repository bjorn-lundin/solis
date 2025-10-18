package body Events is

  function Empty_Data return Event_Type is
    ED : Event_Type;
  begin
    return ED;
  end Empty_Data;

  ----------------------------------------

--    procedure Read_List(Stm  : in     Sql.Statement_Type;
--                        List : in out List_Pack.List;
--                        Max  : in     Integer_4 := Integer_4'Last) is
--      AM_List :Table_Amarkets.Amarkets_List_Pack2.List;
--      M : Market_Type;
--    begin
--      Table_Amarkets.Read_List(Stm,AM_List,Max);
--      for i of AM_List loop
--        M := (
--          Marketid         => i.Marketid,
--          Marketname       => i.Marketname,
--          Startts          => i.Startts,
--          Eventid          => i.Eventid,
--          Markettype       => i.Markettype,
--          Status           => i.Status,
--          Betdelay         => i.Betdelay,
--          Numwinners       => i.Numwinners,
--          Numrunners       => i.Numrunners,
--          Numactiverunners => i.Numactiverunners,
--          Totalmatched     => i.Totalmatched,
--          Totalavailable   => i.Totalavailable,
--          Ixxlupd          => i.Ixxlupd,
--          Ixxluts          => i.Ixxluts
--        );
--        List.Append(M);
--      end loop;
--    end Read_List;
--    ----------------------------------------
--
--
--    procedure Read_Eventid(  Data  : in out Market_Type'Class;
--                             List  : in out List_Pack.List;
--                             Order : in     Boolean := False;
--                             Max   : in     Integer_4 := Integer_4'Last) is
--
--      AM_List :Table_Amarkets.Amarkets_List_Pack2.List;
--      Am_Data : Market_Type;
--      M : Market_Type;
--    begin
--      Am_Data.Eventid := Data.Eventid;
--      Table_Amarkets.Read_Eventid(Am_Data, AM_List, Order, Max);
--      for i of AM_List loop
--        M := (
--          Marketid         => i.Marketid,
--          Marketname       => i.Marketname,
--          Startts          => i.Startts,
--          Eventid          => i.Eventid,
--          Markettype       => i.Markettype,
--          Status           => i.Status,
--          Betdelay         => i.Betdelay,
--          Numwinners       => i.Numwinners,
--          Numrunners       => i.Numrunners,
--          Numactiverunners => i.Numactiverunners,
--          Totalmatched     => i.Totalmatched,
--          Totalavailable   => i.Totalavailable,
--          Ixxlupd          => i.Ixxlupd,
--          Ixxluts          => i.Ixxluts
--        );
--        List.Append(M);
--      end loop;
--    end Read_Eventid;
--    ----------------------------------------



end Events;
