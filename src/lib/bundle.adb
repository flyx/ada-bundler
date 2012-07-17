with Ada.Directories;
with Ada.Unchecked_Deallocation;

with Bundle.Configuration;
with Bundle.OS;

package body Bundle is
   type Path_Retrieval_Function is
     access function (Is_Generic, Append_Name : access Boolean) return String;

   -- Dir_Name is used when the path returned by Getter is generic.
   function Get_Dir (Getter : Path_Retrieval_Function; Dir_Name : String) return String is
      Is_Generic, Append_Name : aliased Boolean := False;
      Path : String := Getter.all (Is_Generic'Access, Append_Name'Access);
   begin
      if Append_Name then
         declare
            Path1 : String := Ada.Directories.Compose
              (Path, Configuration.Cur_Application_Folder_Name.all);
         begin
            if Is_Generic then
               return Ada.Directories.Compose (Path1, Dir_Name);
            else
               return Path1;
            end if;
         end;
      else
         if Is_Generic then
            return Ada.Directories.Compose (Path, Dir_Name);
         else
            return Path;
         end if;
      end if;
   exception when Constraint_Error =>
      raise Environment_Exception with "Application folder name not set!";
   end Get_Dir;

   procedure Set_Application_Folder_Name (Name : String) is
      use Bundle.Configuration;

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
   begin
      Ada.Directories.Create_Path (Ada.Directories.Containing_Directory (Path));
   end Create_Containing_Dir;
   pragma Inline (Create_Containing_Dir);

   function Configuration_Path (Relative_Path : String) return String is
   begin
      return Ada.Directories.Compose (Get_Dir (OS.Configuration_Dir'Access, "config"),
                                      Relative_Path);
   end Configuration_Path;

   function Data_Path (Relative_Path : String) return String is
   begin
      return Ada.Directories.Compose (Get_Dir (OS.Data_Dir'Access, "data"),
                                      Relative_Path);
   end Data_Path;

   function User_Configuration_Path (Relative_Path     : String;
                                     Create_Containing : Boolean := True)
                                    return String is
      Result_Path : String := Ada.Directories.Compose
        (Get_Dir (OS.User_Configuration_Dir'Access, "config"), Relative_Path);
   begin
      if Create_Containing then
         Create_Containing_Dir (Result_Path);
      end if;
      return Result_Path;
   end User_Configuration_Path;

   function User_Data_Path (Relative_Path     : String;
                            Create_Containing : Boolean := True)
                           return String is
      Result_Path : String := Ada.Directories.Compose
        (Get_Dir (OS.User_Data_Dir'Access, "data"), Relative_Path);
   begin
      if Create_Containing then
         Create_Containing_Dir (Result_Path);
      end if;
      return Result_Path;
   end User_Data_Path;

   function User_Cache_Path (Relative_Path     : String;
                             Create_Containing : Boolean := True)
                            return String is
      Result_Path : String := Ada.Directories.Compose
        (Get_Dir (OS.User_Cache_Dir'Access, "cache"), Relative_Path);
   begin
      if Create_Containing then
         Create_Containing_Dir (Result_Path);
      end if;
      return Result_Path;
   end User_Cache_Path;

   function User_Runtime_Path (Relative_Path     : String;
                               Create_Containing : Boolean := True)
                              return String is
      Result_Path : String := Ada.Directories.Compose
        (Get_Dir (OS.User_Runtime_Dir'Access, "temp"), Relative_Path);
   begin
      if Create_Containing then
         Create_Containing_Dir (Result_Path);
      end if;
      return Result_Path;
   end User_Runtime_Path;

end Bundle;
