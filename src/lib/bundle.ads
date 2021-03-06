
-- Provides access to resource files in OS-dependent locations
-- Path strings have to use '/' as folder delimiter.
--
-- Return values always return absolute paths.
package Bundle is
   Path_Creation_Exception : exception;
   Environment_Exception   : exception;
   
   -- the name the folder of the application should have.
   procedure Set_Application_Folder_Name (Name : String);
   
   function Application_Folder_Name return String;
   
   -----------------------------------------------------------------------------
   --                              Global Files                               --
   -----------------------------------------------------------------------------
   
   -- global configuration files
   function Configuration_Path (Relative_Path : String) return String;
   
   -- global data files
   function Data_Path (Relative_Path : String) return String;
   
   -----------------------------------------------------------------------------
   --                          User-specific Files                            --
   -----------------------------------------------------------------------------
   -- Create_Containing creates all directories in the path up to the last '/'--
   -----------------------------------------------------------------------------
   
   function User_Configuration_Path (Relative_Path     : String;
                                     Create_Containing : Boolean := True)
                                    return String;
   
   function User_Data_Path (Relative_Path     : String;
                            Create_Containing : Boolean := True)
                           return String;
   
   -- non-essential user-specific files
   function User_Cache_Path (Relative_Path     : String;
                             Create_Containing : Boolean := True)
                            return String;
   
   function User_Runtime_Path (Relative_Path     : String;
                               Create_Containing : Boolean := True)
                              return String;
end Bundle;