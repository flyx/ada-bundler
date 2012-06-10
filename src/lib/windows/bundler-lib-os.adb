with Ada.Directories;

package body Bundler.Lib.OS is
   
   -- Path to Resources directory inside the application's folder
   -- ONLY WORKS IF THE CURRENT DIRECTORY HAS NOT BEEN ALTERED
   function Resources_Dir return String is
      Exec_Dir   : String := Ada.Directories.Current_Directory;
      Containing : String := Ada.Directories.Containing_Directory (Exec_Dir);
   begin
      return Containing & "/Resources/";
   end Resources_Dir;

   function Configuration_Dir return String is
   begin
      return Resources_Dir & "config/";
   end Configuration_Dir;
      
   function Data_Dir return String is
   begin
      return Resources_Dir & "data/";
   end Data_Dir;
   
   function User_Application_Folder return String is
   begin
      return "TODO";
   end User_Application_Folder;
   
   function User_Configuration_Dir return String is
   begin
      return User_Application_Folder & "config/";
   end User_Configuration_Dir;
   
   function User_Data_Dir return String is
   begin
      return User_Application_Folder & "data/";
   end User_Data_Dir;
   
   function User_Cache_Dir return String is
   begin
      return "TODO";
   end User_Cache_Dir;
   
   function User_Runtime_Dir return String is
   begin
      return "TODO";
   end User_Runtime_Dir;

end Bundler.Lib.OS;