with Ada.Directories;
with Ada.Environment_Variables;

with Bundler.Configuration;

package body Bundler.OS is
   
   No_Valid_Dir_Found_Exception : exception;
   
   -- Select first fitting directory based on the value of an environment variable
   -- and a default list. If no valid directory is found in the variable / the
   -- default list, a path relative to the executable will be searched.
   function Dir_From_Env_Var (Dir_Var, Default, Relative_Default : String)
                             return String is
      
      function Is_Valid (Path : String) return Boolean is
         use Ada.Directories;
      begin
         return Exists (Path) and then Kind (Path) = Directory;
      end Is_Valid;
                             
      function Dir_From_List (List : String) return String is
         Cur_Start : Integer := List'First;
         Cur_End   : Integer := List'First;
      begin
         if List'Length = 0 then
            raise No_Valid_Dir_Found_Exception;
         end if;
         
         loop
            while Cur_End <= List'Last - 1 and then List (Cur_End) /= ':' loop
               Cur_End := Cur_End + 1;
            end loop;
            declare
               Cur_Dir : String := List (Cur_Start .. Cur_End);
            begin
               if Is_Valid (Cur_Dir) then
                  return Cur_Dir;
               end if;
            end;
            Cur_Start := Cur_End + 1;
            Cur_End   := Cur_Start;
            exit when Cur_End > List'Last;
         end loop;
         raise No_Valid_Dir_Found_Exception;
      end Dir_From_List;
      
   begin
      if Ada.Environment_Variables.Exists (Dir_Var) then
         return Dir_From_List (Ada.Environment_Variables.Value (Dir_Var));
      else
         return Dir_From_List (Default);
      end if;
   exception when No_Valid_Dir_Found_Exception =>
      return Dir_From_List (Ada.Directories.Current_Directory & Relative_Default);
   end Dir_From_Env_Var;

   function Configuration_Dir return String is
   begin
      return Dir_From_Env_Var ("XDG_CONFIG_DIRS", "/usr/local/share/:/usr/share/",
                               "config");
   end Configuration_Dir;
   
   function Data_Dir return String is
   begin
      return Dir_From_Env_Var ("XDG_DATA_DIRS", "/usr/local/share/:/usr/share/",
                               "data");
   end Data_Dir;
   
   function User_Configuration_Dir return String is
   begin
      return "TODO/";
   end User_Configuration_Dir;
   
   function User_Data_Dir return String is
   begin
      return "TODO/";
   end User_Data_Dir;
   
   function User_Cache_Dir return String is
   begin
      return "TODO/";
   end User_Cache_Dir;
   
   function User_Runtime_Dir return String is
   begin
      return "TODO/";
   end User_Runtime_Dir;

end Bundler.OS;