
private package Bundler.OS is

   -- Is_Generic and Append_Name are used to return Boolean values. If
   -- Is_Generic is True, the returned folder
   -- is a generic folder that may host other data by this application - for
   -- the requested functionality, a subfolder should be appended.
   -- Append_Name tells Bundler.Lib whether to append the application's folder.
   function Configuration_Dir (Is_Generic, Append_Name : access Boolean) return String;
   function Data_Dir (Is_Generic, Append_Name : access Boolean) return String;
   function User_Configuration_Dir (Is_Generic, Append_Name : access Boolean) return String;
   function User_Data_Dir (Is_Generic, Append_Name : access Boolean) return String;
   function User_Cache_Dir (Is_Generic, Append_Name : access Boolean) return String;
   function User_Runtime_Dir (Is_Generic, Append_Name : access Boolean) return String;

end Bundler.OS;
