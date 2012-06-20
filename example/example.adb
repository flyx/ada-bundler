with Ada.Text_IO;

with Bundle;

procedure Example is
   use Ada.Text_IO;

   Global_Data_File_Path : String := Bundle.Data_Path ("global-data-file.txt");
   OS_Data_File_Path     : String := Bundle.Data_Path ("os-specific-data.txt");

   Global_Data_File : Ada.Text_IO.File_Type;
   OS_Data_File     : Ada.Text_IO.File_Type;
begin
   Open (Global_Data_File, In_File, Global_Data_File_Path);
   Put_Line ("Contents of global-data-file.txt:");
   Put_Line ("---------------------------------");
   Put_Line (Ada.Text_IO.Get_Line (Global_Data_File));
   Close (Global_Data_File);
   
   New_Line (2);
   
   Open (OS_Data_File, In_File, OS_Data_File_Path);
   Put_Line ("Contents of os-specific-data.txt:");
   Put_Line ("---------------------------------");
   Put_Line (Ada.Text_IO.Get_Line (OS_Data_File));
   Close (OS_Data_File);
end Example;