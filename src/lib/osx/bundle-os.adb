with Ada.Directories;
with Ada.Environment_Variables;

with Bundle.Configuration;

package body Bundle.OS is
   use Bundle.Configuration;

   -- Path to Resources directory inside App bundle
   -- ONLY WORKS IF THE CURRENT DIRECTORY HAS NOT BEEN ALTERED
   function Resources_Dir return String is
      Exec_Dir   : String := Ada.Directories.Current_Directory;
      Containing : String := Ada.Directories.Containing_Directory (Exec_Dir);
   begin
      return Ada.Directories.Compose (Containing, "Resources");
   end Resources_Dir;

   function Configuration_Dir (Is_Generic, Append_Name : access Boolean) return String is
   begin
      Is_Generic.all  := True;
      Append_Name.all := False;
      return Resources_Dir;
   end Configuration_Dir;

   function Data_Dir (Is_Generic, Append_Name : access Boolean) return String
     renames Configuration_Dir;

   function User_Configuration_Dir (Is_Generic, Append_Name : access Boolean) return String is
      use Ada.Directories;
   begin
      Is_Generic.all  := True;
      Append_Name.all := True;
      return Compose (Compose (Ada.Environment_Variables.Value ("HOME"),
                               "Library"), "Application Support");
   end User_Configuration_Dir;

   function User_Data_Dir (Is_Generic, Append_Name : access Boolean) return String
                           renames User_Configuration_Dir;

   function User_Cache_Dir (Is_Generic, Append_Name : access Boolean) return String is
      use Ada.Directories;
   begin
      Is_Generic.all  := False;
      Append_Name.all := True;
      return Compose (Compose (Ada.Environment_Variables.Value ("HOME"),
                               "Library"), "Caches");
   end User_Cache_Dir;

   function User_Runtime_Dir (Is_Generic, Append_Name : access Boolean) return String is
   begin
      Is_Generic.all  := False;
      Append_Name.all := True;
      -- Covers 99% of all cases. For doing better, ObjC librarys have to
      -- be used, which is not much fun in Ada.
      return "/tmp";
   end User_Runtime_Dir;

end Bundle.OS;
