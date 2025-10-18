

with aws;
with AWS.Response;
with text_io;
with aws.client;

procedure get_test is
  res : aws.response.data;
begin

  res := aws.Client.Get(URL => "https://www.google.com");
  Text_IO.Put_Line (aws.Response.Message_Body (res));
end get_test;


