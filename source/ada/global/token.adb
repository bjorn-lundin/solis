
package body Token is


  procedure Init(A_Token : in out Token_Type ;
                  Username   : in     String;
                  Password   : in     String;
                  Product_Id : in     String;
                  Vendor_Id  : in     String;
                  App_Key    : in     String) is
  begin
    A_Token.Username   := To_Unbounded_String(Username);
    A_Token.Password   := To_Unbounded_String(Password);
    A_Token.Product_Id := To_Unbounded_String(Product_Id);
    A_Token.Vendor_Id  := To_Unbounded_String(Vendor_Id);
    A_Token.App_Key    := To_Unbounded_String(App_Key);
  end Init;
  

  function Is_Set(A_token : Token_Type) return Boolean is
  begin
    return A_Token.Token_Is_Set;
  end Is_Set;

  --------------------------------------------------------
  function  Get(A_Token : Token_Type) return String is
  begin
    if A_Token.Token_Is_Set then
      return To_String(A_Token.The_Token);
    else
      raise Not_Valid_Token;
    end if;
  end Get;
  --------------------------------------------------------
  procedure Set (A_Token : in out Token_Type; The_Token : String ) is
  begin
    A_Token.The_Token    := To_Unbounded_String(The_Token);
    A_Token.Token_Is_Set := True;
  end Set;
  -------------------------------------------------------------
  procedure Unset (A_Token : in out Token_Type) is
  begin
    A_Token.The_Token    := Null_Unbounded_String;
    A_Token.Token_Is_Set := False;
  end Unset;
  -------------------------------------------------------------
  
  function  Get_App_Key (A_Token : Token_Type) return String is
  begin
    return To_String(A_Token.App_Key);
  end Get_App_Key;
  -------------------------------------------------------------
  
  function  Get_Username (A_Token : Token_Type) return String is
  begin
    return To_String(A_Token.Username);
  end Get_Username;
  -------------------------------------------------------------
  
  function  Get_Password (A_Token : Token_Type) return String is
  begin
    return To_String(A_Token.Password);
  end Get_Password;
  -------------------------------------------------------------
  
  function  Get_Product_Id (A_Token : Token_Type) return String is
  begin
    return To_String(A_Token.Product_Id);
  end Get_Product_Id;
  -------------------------------------------------------------
  
  function  Get_Vendor_Id (A_Token : Token_Type) return String is
  begin
    return To_String(A_Token.Vendor_Id);
  end Get_Vendor_Id;
  -----------------------------------------------------------
  
end Token;
