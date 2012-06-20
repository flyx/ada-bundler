with Ada.Text_IO;
with Ada.Command_Line;

with Bundle;

procedure Bundle_Test is
   use Ada.Text_IO;
begin
   Bundle.Set_Application_Folder_Name ("Bundle_Test");
   Put ("Set application folder name to """);
   Put (Bundle.Application_Folder_Name);
   Put_Line ("""");
   
   Put ("Config file: ");
   Put_Line (Bundle.Configuration_Path ("config.txt"));
   
   Put ("Data file: ");
   Put_Line (Bundle.Data_Path ("data.txt"));
   
   Put ("User-specific config file: ");
   Put_Line (Bundle.User_Configuration_Path ("user_config.txt", False));
   
   Put ("User-specific data file: ");
   Put_Line (Bundle.User_Data_Path ("user_data.txt", False));
   
   Put ("User-specific cache file: ");
   Put_Line (Bundle.User_Cache_Path ("cache.txt", False));
   
   Put ("User-specific runtime file: ");
   Put_Line (Bundle.User_Runtime_Path ("temp.txt", False));
end Bundle_Test;