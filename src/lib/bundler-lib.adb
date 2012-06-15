with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Bundler.Configuration;
with Bundler.OS;

package body Bundler.Lib is
   
   -- cache directory paths
   Global_Configuration_Dir : constant String := OS.Configuration_Dir;
   Global_Data_Dir          : constant String := OS.Data_Dir;
   User_Configuration_Dir   : constant String := OS.User_Configuration_Dir;
   User_Data_Dir            : constant String := OS.User_Data_Dir;
   User_Cache_Dir           : constant String := OS.User_Cache_Dir;
   User_Runtime_Dir         : constant String := OS.User_Runtime_Dir;
   
   procedure Set_Application_Folder_Name (Name : String) is
      use Bundler.Configuration;
      
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
      
   begin
      if Cur_Application_Folder_Name /= null then
         if Cur_Application_Folder_Name.all'Length /= Name'Length then
            Free (Cur_Application_Folder_Name);
         end if;
      end if;
      if Cur_Application_Folder_Name = null then
         Cur_Application_Folder_Name := new String(Name'Range);
      end if;
      Cur_Application_Folder_Name.all := Name;
   end Set_Application_Folder_Name;
   
   function Application_Folder_Name return String is
   begin
      return Configuration.Cur_Application_Folder_Name.all;
   end Application_Folder_Name;
   
   procedure Create_Containing_Dir (Path : String) is
      Last_Slash : Natural := Ada.Strings.Fixed.Index (Path, "/",
                                                       Ada.Strings.Backward);
   begin
      Ada.Directories.Create_Path (Path (Path'First .. (Last_Slash - 1)));
   end Create_Containing_Dir;
   pragma Inline (Create_Containing_Dir);

   function Configuration_Path (Relative_Path : String) return String is
   begin
      return Global_Configuration_Dir & Relative_Path;
   end Configuration_Path;
   
   function Data_Path (Relative_Path : String) return String is
   begin
      return Global_Data_Dir & Relative_Path;
   end Data_Path;

   function User_Configuration_Path (Relative_Path     : String;
                                     Create_Containing : Boolean := True)
                                    return String is
      Result_Path : String := User_Configuration_Dir &
        Configuration.Cur_Application_Folder_Name.all & "/" & Relative_Path;
   begin
      if Create_Containing then
         Create_Containing_Dir (Result_Path);
      end if;
      return Result_Path;
   end User_Configuration_Path;

   function User_Data_Path (Relative_Path     : String;
                            Create_Containing : Boolean := True)
                           return String is
      Result_Path : String := User_Data_Dir &
        Configuration.Cur_Application_Folder_Name.all & "/" & Relative_Path;
   begin
      if Create_Containing then
         Create_Containing_Dir (Result_Path);
      end if;
      return Result_Path;
   end User_Data_Path;

   function User_Cache_Path (Relative_Path     : String;
                             Create_Containing : Boolean := True)
                            return String is
      Result_Path : String := User_Cache_Dir & 
        Configuration.Cur_Application_Folder_Name.all & "/" & Relative_Path;
   begin
      if Create_Containing then
         Create_Containing_Dir (Result_Path);
      end if;
      return Result_Path;
   end User_Cache_Path;

   function User_Runtime_Path (Relative_Path     : String;
                               Create_Containing : Boolean := True)
                              return String is
      Result_Path : String := User_Runtime_Dir &
        Configuration.Cur_Application_Folder_Name.all & "/" & Relative_Path;
   begin
      if Create_Containing then
         Create_Containing_Dir (Result_Path);
      end if;
      return Result_Path;
   end User_Runtime_Path;

end Bundler.Lib;