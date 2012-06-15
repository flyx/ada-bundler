with Ada.Text_IO;
with Ada.Command_Line;

with Bundler.Lib;

procedure Bundler_Test.Lib is
   use Ada.Text_IO;
begin
   Bundler.Lib.Set_Application_Folder_Name ("Bundler_Test-Lib");
   Put ("Set application folder name to """);
   Put (Bundler.Lib.Application_Folder_Name);
   Put_Line ("""");
   
   Put ("Config file: ");
   Put_Line (Bundler.Lib.Configuration_Path ("config.txt"));
   
   Put ("Data file: ");
   Put_Line (Bundler.Lib.Data_Path ("data.txt"));
   
   Put ("User-specific config file: ");
   Put_Line (Bundler.Lib.User_Configuration_Path ("user_config.txt", False));
   
   Put ("User-specific data file: ");
   Put_Line (Bundler.Lib.User_Data_Path ("user_data.txt", False));
   
   Put ("User-specific cache file: ");
   Put_Line (Bundler.Lib.User_Cache_Path ("cache.txt", False));
   
   Put ("User-specific runtime file: ");
   Put_Line (Bundler.Lib.User_Runtime_Path ("temp.txt", False));
end Bundler_Test.Lib;