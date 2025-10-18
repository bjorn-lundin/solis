
with Ada.Exceptions;
package Stacktrace is

  procedure Tracebackinfo(E : Ada.Exceptions.Exception_Occurrence) ;
  function Pure_Hexdump(Input : in String) return String ;
  
end Stacktrace;
