with Ada.Directories;
with Ada.Environment_Variables;

with Bundle.Configuration;

package body Bundle.OS is

   No_Valid_Dir_Found_Exception : exception;
   
   Home_Dir : constant String := Ada.Environment_Variables.Value ("HOME");
   
   -- Select first fitting directory based on the value of an environment variable
   -- and a default list. If no valid directory is found in the variable / the
   -- default list, a path relative to the executable will be searched.
   function Dir_From_Env_Var (Dir_Var, Default, Required_Subdir,
                              Relative_Default : String;
                              Is_Local         : not null access Boolean;
                              Create_Directory : Boolean := False)
                             return String is
      
      function Is_Valid (Path : String) return Boolean is
         use Ada.Directories;
      begin
         return Exists (Path) and then Kind (Path) = Directory;
      end Is_Valid;
                             
      function Dir_From_List (List, Required_Subdir : String) return String is
         Cur_Start : Integer := List'First;
         Cur_End   : Integer := List'First;
      begin
         if List'Length = 0 then
            raise No_Valid_Dir_Found_Exception;
         end if;
         
         loop
            while Cur_End = List'Last - 1 or else
              (Cur_End < List'Last - 1 and then List (Cur_End + 1) /= ':') loop
               Cur_End := Cur_End + 1;
            end loop;
            declare
               Cur_Dir : String := List (Cur_Start .. Cur_End);
            begin
               if Required_Subdir'Length > 0 then
                  if Is_Valid (Ada.Directories.Compose (Cur_Dir, Required_Subdir)) then
                     return Cur_Dir;
                  end if;
               elsif Is_Valid (Cur_Dir) then
                  return Cur_Dir;
               end if;
               if Create_Directory then
                  if Required_Subdir'Length > 0 then
                     Ada.Directories.Create_Path (
                       Ada.Directories.Compose (Cur_Dir, Required_Subdir));
                  else
                     Ada.Directories.Create_Path (Cur_Dir);
                  end if;
                  return Cur_Dir;
               end if;
            exception
               -- directory could not be created. Just try the next one on the list.
               when Ada.Directories.Use_Error | Ada.Directories.Name_Error => null;
            end;
            Cur_Start := Cur_End + 2;
            Cur_End   := Cur_Start;
            exit when Cur_End > List'Last;
         end loop;
         raise No_Valid_Dir_Found_Exception;
      end Dir_From_List;
      
   begin
      Is_Local.all := False;
      if Ada.Environment_Variables.Exists (Dir_Var) then
         return Dir_From_List (Ada.Environment_Variables.Value (Dir_Var), Required_Subdir);
      else
         return Dir_From_List (Default, Required_Subdir);
      end if;
   exception when No_Valid_Dir_Found_Exception =>
      Is_Local.all := True;
      return Dir_From_List (Ada.Directories.Current_Directory & "/" & Relative_Default, "");
   end Dir_From_Env_Var;

   function Configuration_Dir (Is_Generic, Append_Name : access Boolean) return String is
      Is_Local : aliased Boolean := False;
      Path : String := Dir_From_Env_Var (
        "XDG_CONFIG_DIRS", "/usr/local/share/:/usr/share/",
        Configuration.Cur_Application_Folder_Name.all, "config", Is_Local'Access);
   begin
      Is_Generic.all := not Is_Local;
      Append_Name.all := not Is_Local;
      return Path;
   end Configuration_Dir;
   
   function Data_Dir (Is_Generic, Append_Name : access Boolean) return String is
      Is_Local : aliased Boolean := False;
      Path : String := Dir_From_Env_Var (
        "XDG_DATA_DIRS", "/usr/local/share/:/usr/share/",
        Configuration.Cur_Application_Folder_Name.all, "data", Is_Local'Access);
   begin
      Is_Generic.all := not Is_Local;
      Append_Name.all := not Is_Local;
      return Path;
   end Data_Dir;
   
   function User_Configuration_Dir (Is_Generic, Append_Name : access Boolean)
                                   return String is
      Is_Local : aliased Boolean := False;
      Path : String := Dir_From_Env_Var (
        "XDG_CONFIG_HOME", Home_Dir & "/.config/",
        Configuration.Cur_Application_Folder_Name.all, "user/config/", 
        Is_Local'Access, True);
   begin
      Is_Generic.all := False;
      Append_Name.all := not Is_Local;
      return Path;
   end User_Configuration_Dir;
   
   function User_Data_Dir (Is_Generic, Append_Name : access Boolean) return String is
      Is_Local : aliased Boolean := False;
      Path : String := Dir_From_Env_Var (
        "XDG_DATA_HOME", Home_Dir & "/.local/share/",
        Configuration.Cur_Application_Folder_Name.all, "user/data/",
        Is_Local'Access, True);
   begin
      Is_Generic.all := False;
      Append_Name.all := not Is_Local;
      return Path;
   end User_Data_Dir;
   
   function User_Cache_Dir (Is_Generic, Append_Name : access Boolean) return String is
      Is_Local : aliased Boolean := False;
      Path : String := Dir_From_Env_Var (
        "XDG_CACHE_HOME", Home_Dir & "/.cache/",
        Configuration.Cur_Application_Folder_Name.all, "user/cache/",
        Is_Local'Access, True);
   begin
      Is_Generic.all := False;
      Append_Name.all := not Is_Local;
      return Path;
   end User_Cache_Dir;
   
   function User_Runtime_Dir (Is_Generic, Append_Name : access Boolean) return String is
      Is_Local : aliased Boolean := False;
      Path : String := Dir_From_Env_Var (
        "XDG_RUNTIME_DIR", "", Configuration.Cur_Application_Folder_Name.all,
        "user/runtime/", Is_Local'Access, True);
   begin
      Is_Generic.all := False;
      Append_Name.all := not Is_Local;
      return Path;
   end User_Runtime_Dir;

end Bundle.OS;