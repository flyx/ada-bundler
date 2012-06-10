with Ada.Directories;
with Ada.Environment_Variables;

package body Bundler.Lib.OS is
   -- Path to Resources directory inside App bundle
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
      
   function User_Configuration_Dir return String is
   begin
      return Ada.Environment_Variables.Value ("HOME")
        & "/Library/Application Data/";
   end User_Configuration_Dir;
      
   function User_Data_Dir return String renames User_Configuration_Dir;
   
   function User_Cache_Dir return String is
   begin
      return Ada.Environment_Variables.Value ("HOME")
        & "/Library/Caches/";
   end User_Cache_Dir;
   
   function User_Runtime_Dir return String is
   begin
      -- Covers 99% of all cases. For doing better, ObjC librarys have to
      -- be used, which is not much fun in Ada.
      return "/tmp/";
   end User_Runtime_Dir;

end Bundler.Lib.OS;