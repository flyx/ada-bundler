with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;

with Ada.Text_IO;

with Interfaces.C.Strings;

with Bundle.Configuration;

package body Bundle.OS is
   use Bundle.Configuration;
   
   function Get_Bundle_Path return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Bundle_Path, External_Name => "get_bundle_path");
   
   Raw_Bundle_Path : Interfaces.C.Strings.chars_ptr
     := Get_Bundle_Path;
   Bundle_Path : String := Interfaces.C.Strings.Value (Raw_Bundle_Path);
   
   -- Path to Resources directory inside App bundle
   function Resources_Dir return String is
      use Ada.Directories;
   begin
      return Compose (Compose (Bundle_Path, "Contents"), "Resources");
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
