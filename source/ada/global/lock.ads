with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Posix; use Posix;

with Ada.Finalization;
package Lock is

  procedure Write_File(Name : String;  Content : String);
  function Read_File(Name : String ) return String;

  type Lock_Type is tagged private;
  Lock_Error : exception;
  procedure Take(A_Lock : in out Lock_Type; Name : in String);
private
  type Lock_Type is new Ada.Finalization.Controlled with record
    Name                   : Unbounded_String := Null_Unbounded_String;
    Fd                     : Posix.Int        := Posix.Int'First;
    Currently_Holding_Lock : Boolean          := False;
  end record;
  
  overriding procedure Finalize(A_Lock : in out Lock_Type);
end Lock;